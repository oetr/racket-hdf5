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
        [else (error 'hdf5-type->ctype "Unsupported class: ~a~n" class)]))

(define (dataset-get-data fid name)
  (define dset (H5Dopen fid name H5P_DEFAULT))
  (define space (H5Dget_space dset))
  (define ndims (H5Sget_simple_extent_ndims space))
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
           (H5Tinsert memtype
                      (H5Tget_member_name type i)
                      (H5Tget_member_offset type i)
                      (H5Tget_native_type (H5Tget_member_type type i)
                                          'H5T_DIR_DEFAULT)))
         ]
        [(eq? class 'H5T_ARRAY)
         (printf "ALL-size: ~a~n" (* (apply * dims) type-size))
         ]
        [(eq? class 'H5T_OPAQUE)
         'do-nothing
         ]
        [else 'do-nothing])
  (define rdata-ptr (malloc (* (apply * dims) type-size)))
  ;;(memset rdata-ptr #xff (* (apply * dims) type-size))
  ;;(* (apply * dims) type-size)

  ;;(printf "size: ~a~n" (* (apply * dims) type-size))
  
  (define status (H5Dread dset memtype
                          H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr))

  (define precision (H5Tget_precision type))
  
  (define data #f)
  (cond [(symbol=? class 'H5T_FLOAT)
         (printf "FLOAT ----------------------------------------~n ")
         (define c-type (lookup-table float-table precision))
         (set! data
               (vector->array (list->vector dims)
                              (cblock->vector rdata-ptr c-type
                                              (apply * dims))))]
        [(symbol=? class 'H5T_INTEGER)
         (printf "INTEGER ----------------------------------------~n ")
         (define sign (H5Tget_sign type))
         (define c-type (lookup-table (lookup-table int-sign-table sign)
                                      precision))
         (set! data
               (vector->array (list->vector dims)
                              (cblock->vector rdata-ptr c-type
                                              (apply * dims))))]
        [(symbol=? class 'H5T_ENUM)
         (printf "ENUM ----------------------------------------~n ")
         (define sign (H5Tget_sign type))
         (define c-type (lookup-table (lookup-table int-sign-table sign)
                                      precision))
         ;; get data
         (define all-data (cblock->vector rdata-ptr c-type
                                          (apply * dims)))
         ;; get enum values and corresponding symbols
         (define symbol-mapping
           (for/list ([i (H5Tget_nmembers type)])
             (define name (H5Tget_member_name type i))
             (cons (H5Tget_member_index type name)
                   (string->symbol name))))
         ;; map values into symbols, convert into math/array
         (set! data
               (vector->array (list->vector dims)
                             (vector-map
                              (lambda (n)
                                (lookup-table symbol-mapping n))
                              all-data)))]
        [(symbol=? class 'H5T_VLEN)
         (printf "VARIABLE LENGTH ----------------------------------------~n ")
         (define rdata-converted (cblock->vector rdata-ptr _hvl_t (apply * dims)))
         (define super-class (H5Tget_class (H5Tget_super type)))
         (define c-type #f)
         (cond [(symbol=? super-class 'H5T_INTEGER)
                (define sign (H5Tget_sign type))
                (set! c-type (lookup-table (lookup-table int-sign-table
                                                           sign)
                                             precision))]
                [(symbol=? super-class 'H5T_FLOAT)
                 (set! c-type (lookup-table float-table precision))]
                [else
                 (error 'dataset-get-data "Unsupported class ~a~n" super-class)
                 ])
         (set! data
               (vector-map
                (lambda (hvl)
                  (define len (hvl_t-len hvl))
                  (vector->array (vector len)
                                 (cblock->vector (hvl_t-p hvl) c-type
                                                 len)))
                rdata-converted))
         ]
        [(symbol=? class 'H5T_STRING)
         (printf "STRING ----------------------------------------~n ")
         (set! data
           (for/array ([offset (apply * dims)])
             (cast (ptr-add rdata-ptr offset _pointer) _pointer _string)))
         ]
        [(symbol=? class 'H5T_COMPOUND)
         (printf "COMPOUND ----------------------------------------~n ")
         (define nmembers (H5Tget_nmembers type))

         (define hdf5-type-list
           (for/list ([i nmembers])
             (H5Tget_member_type type i)))

         (define type-list
           (for/list ([i nmembers])
             (define member-type (H5Tget_member_type type i))
             (hdf5-type->ctype member-type)))
         
         (printf "TYPE LIST: ~a~n" type-list)

         (define size-list
           (map H5Tget_size hdf5-type-list))

         (define name-list
           (for/list ([i nmembers])
             (H5Tget_member_name type i)))

         (printf "NAME LIST: ~a~n" name-list)

         (define offset-list
           (for/list ([i nmembers])
             (H5Tget_member_offset type i)))

         (define (struct-type->list block ctypes names offsets n)
           (for/list ([i n])
               (for/hash ([name names]
                          [ctype ctypes]
                          [offset offsets])
                 (define val (ptr-ref block ctype 'abs (+ offset (* i type-size))))
                 (values name val))))

         (set! data (struct-type->list rdata-ptr type-list name-list offset-list (apply * dims)))
         ]
        [(symbol=? class 'H5T_OPAQUE)
         (printf "OPAQUE ----------------------------------------~n ")
         (set! data (vector->array
                     (cblock->vector rdata-ptr _ubyte
                                     (* (apply * dims) type-size))))
         ]
        [(symbol=? class 'H5T_ARRAY)
         (printf "ARRAY ----------------------------------------~n ")
         (printf "TYPE: ~a~n" (H5Tget_native_type type 'H5T_DIR_DEFAULT))
         (printf "TYPE SIZE: ~a~n" type-size)
         (printf "TYPE: ~a~n" (H5Tget_class (H5Tget_native_type type 'H5T_DIR_DEFAULT)))
         (define super-class (H5Tget_class (H5Tget_super type)))
         
         (printf "SUPER-CLASS: ~a~n" super-class)
         (printf "C-TYPE: ~a~n" (hdf5-type->ctype (H5Tget_super type)))
         (define ctype (hdf5-type->ctype (H5Tget_super type)))
         ;; get array shape

         (define array-dims (H5Tget_array_dims type))
         (set! data (vector->array
                     (list->vector (append dims array-dims))
                     (cblock->vector rdata-ptr ctype
                                     (* (apply * (append dims array-dims))))))

         ]
        [else
         (error 'dataset-get-data "Unsupported class ~a~n" class)])


  (H5Dclose dset)
  data)

(define files
  (filter
   (lambda (file-dir)
     (regexp-match? #rx".*\\.h5" file-dir))
   (directory-list "./data" #:build? #t)))

(for ([file-name files])
  (printf "file: ~a~n" file-name)
  ;;(define file-name (list-ref files 8))

  (define fid (H5Fopen file-name H5F_ACC_RDWR H5P_DEFAULT))
  (define objects (h5file->name-type fid))
  (define dsets (filter (lambda (o) (symbol=? 'H5O_TYPE_DATASET (cadr o)))
                        objects))

  (unless (empty? dsets)
    (define dset-name (car (list-ref dsets 0)))
    (define data (dataset-get-data fid dset-name))
    (cond [(array? data)
           (printf "SHAPE: ~a~n" (array-shape data))]
          [(vector? data)
           (printf "SHAPE vector: ~a~n"
                   (vector-length data)
                   )]
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
