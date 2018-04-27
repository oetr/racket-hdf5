#lang racket


(require (rename-in ffi/unsafe
                    (in-array ffi-in-array)
                    (array? ffi-array?)
                    (array-set! ffi-array-set!)
                    (array-ref ffi-array-ref))
         ffi/unsafe/define
         rackunit
         math/array)

(require "../unsafe/hdf5.rkt"
         "../hl/hdf5.rkt")

;; get the names of all
(define (h5file->name-type fid)
  (define results '())
  (define (iter-proc obj name info op-data)
    (set! results (cons (list name (H5O_info_t-type info)) results)))
  (H5Ovisit fid 'H5_INDEX_NAME 'H5_ITER_NATIVE iter-proc #f)
  (reverse results))

(define s-int-table
  (list (cons 8 _sint8)
        (cons 16 _sint16)
        (cons 32 _sint32)
        (cons 64 _sint64)))

(define u-int-table
  (list (cons 8 _uint8)
        (cons 16 _uint16)
        (cons 32 _uint32)
        (cons 64 _uint64)))

(define int-sign-table
  (list (cons 'H5T_SGN_NONE u-int-table)
        (cons 'H5T_SGN_2 s-int-table)))

(define float-table
  (list (cons 32 _float)
        (cons 64 _double)
        (cons 128 _longdouble)))

(define (lookup-table table precision)
  (cond [(empty? table)
         (error 'lookup-table "Precision not found: ~a~n" precision)]
        [(eq? precision (caar table)) (cdar table)]
        [else (lookup-table (cdr table) precision)]))

(define (hdf5-type->ctype hdf5type)
  (define class (H5Tget_class hdf5type))
  (define precision (H5Tget_precision hdf5type))
  (cond [(eq? class 'H5T_INTEGER)
         (define sign (H5Tget_sign hdf5type))
         (lookup-table (lookup-table int-sign-table sign)
                                      precision)]
        [(eq? class 'H5T_FLOAT) (lookup-table float-table precision)]
        [(eq? class 'H5T_STRING) _string]
        [(eq? class 'H5T_VLEN) _pointer]
        [(eq? class 'H5T_ENUM) _pointer]
        [(eq? class 'H5T_ARRAY) _pointer]
        [(eq? class 'H5T_REFERENCE) _pointer]
        [else (error 'hdf5-type->ctype "Unsupported class: ~a~n" class)]))

;; lookup 
(define (reconstruct data-ptr type dims (compound-string? #f))
  (define type-size (H5Tget_size type))
  (define class (H5Tget_class type))
  (define precision (H5Tget_precision type))
  (define len (apply * dims))
  (cond
    [(eq? class 'H5T_FLOAT)
     (printf "FLOAT ----------------------------------------~n ")
     (define c-type (lookup-table float-table precision))
     (define result (cblock->vector data-ptr c-type len))
     (if (= len 1)
         (vector-ref result 0)
         (vector->array (list->vector dims) result))]
    [(eq? class 'H5T_INTEGER)
     (printf "INTEGER ----------------------------------------~n ")
     (define sign (H5Tget_sign type))
     (define c-type (lookup-table (lookup-table int-sign-table sign)
                                  precision))
     (define result (cblock->vector data-ptr c-type len))
     (if (= len 1)
         (vector-ref result 0)
         (vector->array (list->vector dims) result))]
    [(symbol=? class 'H5T_STRING)
     (printf "STRING ----------------------------------------~n ")
     (define result 
       (for/array ([offset len])
                  (if compound-string?
                      (ptr-ref data-ptr _string offset)
                      (cast (ptr-add data-ptr offset _pointer) _pointer
                            _string))))
     (if (= len 1)
         (array-ref result #(0))
         result)]
    [(symbol=? class 'H5T_ENUM)
     (printf "ENUM ----------------------------------------~n ")
     (define sign (H5Tget_sign type))
     (define c-type (lookup-table (lookup-table int-sign-table sign)
                                  precision))
     ;; get data
     (define all-data (cblock->vector data-ptr c-type
                                      (apply * dims)))
     ;; get enum values and corresponding symbols
     (define symbol-mapping
       (for/list ([i (H5Tget_nmembers type)])
         (define name (H5Tget_member_name type i))
         (cons (H5Tget_member_index type name)
               (string->symbol name))))
     ;; map values into symbols, convert into math/array
     (vector->array (list->vector dims)
                    (vector-map
                     (lambda (n)
                       (lookup-table symbol-mapping n))
                     all-data))]
    [(symbol=? class 'H5T_VLEN)
     (printf "VARIABLE LENGTH ----------------------------------------~n ")
     (define rdata-converted (cblock->vector data-ptr _hvl_t len))
     (define super-type (H5Tget_super type))
     (vector-map
      (lambda (hvl)
        (define block-len (hvl_t-len hvl))
        (reconstruct (hvl_t-p hvl) super-type (list block-len)))
      rdata-converted)]
    [(symbol=? class 'H5T_OPAQUE)
     (printf "OPAQUE ----------------------------------------~n ")
     (vector->array
      (cblock->vector data-ptr _ubyte
                      (* (apply * dims) type-size)))]
    [(symbol=? class 'H5T_ARRAY)
     (printf "ARRAY ----------------------------------------~n ")
     (printf "TYPE: ~a~n" (H5Tget_native_type type 'H5T_DIR_DEFAULT))
     (printf "TYPE SIZE: ~a~n" type-size)
     (printf "TYPE: ~a~n" (H5Tget_class (H5Tget_native_type type 'H5T_DIR_DEFAULT)))
     (define super-class (H5Tget_class (H5Tget_super type)))
     (printf "SUPER-CLASS: ~a~n" super-class)
     (define array-dims (H5Tget_array_dims type))
     (reconstruct data-ptr (H5Tget_super type) (append dims array-dims))]
    [(symbol=? class 'H5T_COMPOUND)
     (printf "COMPOUND ----------------------------------------~n ")
     (define nmembers (H5Tget_nmembers type))

     (define type-list
       (for/list ([i nmembers])
         (H5Tget_member_type type i)))

     (printf "TYPE LIST: ~a~n" type-list)

     (define name-list
       (for/list ([i nmembers])
         (H5Tget_member_name type i)))

     (printf "NAME LIST: ~a~n" name-list)

     (define offset-list
       (for/list ([i nmembers])
         (H5Tget_member_offset type i)))

     (define (struct-type->vector block types names offsets n)
       (for/vector ([i n])
         (for/hash ([name names]
                    [child-type types]
                    [offset offsets])
           (define val
             (reconstruct (ptr-add block (+ offset (* i type-size)) _byte)
                          child-type (list 1)
                          #t))
           (values name val))))

     (struct-type->vector data-ptr type-list name-list offset-list (apply * dims))]
    [else (error 'reconstruct "Unkown class: ~a~n" class)]))


(define (dataset-get-data fid name)
  (define dset (H5Dopen fid name H5P_DEFAULT))
  (define space (H5Dget_space dset))
  (define all-dims (H5Sget_simple_extent_dims space))
  (define dims (vector-ref all-dims 1))
  (define type (H5Dget_type dset))
  (define type-size (H5Tget_size type))
  (define class (H5Tget_class type))
  (define memtype (H5Tget_native_type type 'H5T_DIR_DEFAULT))
  (cond [(eq? class 'H5T_STRING)
         (set! memtype (H5Tcopy H5T_C_S1))
         (H5Tset_size memtype (+ 1 type-size))
         (set! type-size (* (+ 1 type-size) (ctype-sizeof _ubyte)))]
        [(eq? class 'H5T_COMPOUND)
         (set! memtype (H5Tcreate 'H5T_COMPOUND type-size))
         (for ([i (H5Tget_nmembers type)])
           (printf "H5Tinsert: ~a ~a ~a ~a\n"
                   memtype
                   (H5Tget_member_name type i)
                   (H5Tget_member_offset type i)
                   (H5Tget_class (H5Tget_native_type (H5Tget_member_type type i)
                                       'H5T_DIR_DEFAULT)))
           (H5Tinsert memtype
                      (H5Tget_member_name type i)
                      (H5Tget_member_offset type i)
                      (H5Tget_native_type (H5Tget_member_type type i)
                                          'H5T_DIR_DEFAULT)))]
        [(eq? class 'H5T_ARRAY)
         (printf "ALL-size: ~a~n" (* (apply * dims) type-size))]
        [else 'do-nothing])
  (define rdata-ptr (malloc (* (apply * dims) type-size)))
  (H5Dread dset memtype H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr)
  (define data (reconstruct rdata-ptr type dims))
  (H5Dclose dset)
  data)

(define files
  (filter
   (lambda (file-dir)
     (regexp-match? #rx".*\\.h5" file-dir))
   (directory-list "./data" #:build? #t)))

(for ([file-name files])
  (printf "file: ~s~n" file-name)
  ;;(define file-name (list-ref files 8))

  (define fid (H5Fopen file-name H5F_ACC_RDWR H5P_DEFAULT))
  (define objects (h5file->name-type fid))
  (define dsets (filter (lambda (o) (symbol=? 'H5O_TYPE_DATASET (cadr o)))
                        objects))
  (unless (empty? dsets)
    (define dset-name (car (list-ref dsets 0)))
     (when (string=? (path->string file-name) "./data/h5ex_t_cpxcmpd.h5")
       (printf "DATASETS: ~a~n" dsets)
       (set! dset-name (car (list-ref dsets 1))))
    (define data (dataset-get-data fid dset-name))
    (cond [(array? data)
           (printf "SHAPE: ~a~n" (array-shape data))]
          [(vector? data)
           (printf "SHAPE vector: ~a~n"
                   (vector-length data))]
          [(list? data)
           (printf "SHAPE list: ~a~n" (length data))]
          [else (printf "SHAPE: ~a~n" data)])
    (pretty-print data))
    ;;data

    ;; (array-shape data)
    ;; (array-slice-ref data (list (:: 5) ::...))
    ;; (array-slice-ref data (list (:: 0 1) (::)))
    ;; (define status (H5Fclose fid))
    ;; (hdf5-list-all fid)

  (H5Fclose fid)
  )

;; https://support.hdfgroup.org/HDF5/doc1.6/UG/11_Datatypes.html
;; https://support.hdfgroup.org/HDF5/doc/RM/RM_H5T.html
