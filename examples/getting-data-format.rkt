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

(define (dataset-get-data fid name)
  (define dset (H5Dopen fid dset-name H5P_DEFAULT))
  (define space (H5Dget_space dset))
  (define ndims (H5Sget_simple_extent_ndims space))
  (define all-dims (H5Sget_simple_extent_dims space))
  (define dims (vector-ref all-dims 1))
  (define type (H5Dget_type dset))
  (define rdata-ptr (malloc (* (apply * dims) (H5Tget_size type))))
  (define status (H5Dread dset (H5Tget_native_type type 'H5T_DIR_DEFAULT)
                          H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr))

  (define class (H5Tget_class type))
  (define size (H5Tget_size type))
  (define precision (H5Tget_precision type))
  
  (define data #f)
  (cond [(symbol=? class 'H5T_FLOAT)
         (define c-type (lookup-table float-table precision))
         (set! data
               (vector->array (list->vector dims)
                              (cblock->vector rdata-ptr c-type
                                              (apply * dims))))]
        [(symbol=? class 'H5T_INTEGER)
         (define sign (H5Tget_sign type))
         (define c-type (lookup-table (lookup-table int-sign-table sign)
                                      precision))
         (set! data
               (vector->array (list->vector dims)
                              (cblock->vector rdata-ptr c-type
                                              (apply * dims))))]
        [(symbol=? class 'H5T_ENUM)
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
        [else
         (error 'dataset-get-data "Unsupported class ~a~n" class)])

  (H5Dclose dset)
  data)

(define files
  (filter
   (lambda (file-dir)
     (regexp-match? #rx".*\\.h5" file-dir))
   (directory-list "./data" #:build? #t)))

(define file-name (list-ref files 8))

(define fid (H5Fopen file-name H5F_ACC_RDWR H5P_DEFAULT))
(define objects (h5file->name-type fid))
(define dsets (filter (lambda (o) (symbol=? 'H5O_TYPE_DATASET (cadr o)))
                      objects))

(define dset-name (car (list-ref dsets 0)))
(define data (dataset-get-data fid dset-name))
data

;; (array-shape data)
;; (array-slice-ref data (list (:: 5) ::...))
;; (array-slice-ref data (list (:: 0 1) (::)))
;; (define status (H5Fclose fid))

(hdf5-list-all fid)

(define status (H5Fclose fid))

;; https://support.hdfgroup.org/HDF5/doc1.6/UG/11_Datatypes.html
;; https://support.hdfgroup.org/HDF5/doc/RM/RM_H5T.html
