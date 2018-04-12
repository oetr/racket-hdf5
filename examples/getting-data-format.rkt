#lang racket


(require (rename-in ffi/unsafe
                    (in-array ffi-in-array)
                    (array? ffi-array?)
                    (array-set! ffi-array-set!)
                    (array-ref ffi-array-ref))
         ffi/unsafe/define
         rackunit
         math/array
         )

(require "../unsafe/hdf5.rkt")

(define datasets
  (filter
   (lambda (file-dir)
     (regexp-match? #rx".*\\.h5" file-dir))
   (directory-list "./data" #:build? #t)))

(define file-name (list-ref datasets 0))

(define fid (H5Fopen file-name H5F_ACC_RDWR H5P_DEFAULT))


;; get the names of all
(define (h5file->name-type fid)
  (define results '())
  (define (iter-proc obj name info op-data)
    (set! results (cons (list name (H5O_info_t-type info)) results)))

  (H5Ovisit fid 'H5_INDEX_NAME 'H5_ITER_NATIVE iter-proc #f)
  (reverse results))

(define objects (h5file->name-type fid))

(define dsets (filter (lambda (o) (symbol=? 'H5O_TYPE_DATASET (cadr o))) objects))



(define axis0 (H5Dopen fid (car (list-ref dsets 0)) H5P_DEFAULT))
(define space-axis0 (H5Dget_space axis0))
(define ndims (H5Sget_simple_extent_ndims space-axis0))
(define all-dims (H5Sget_simple_extent_dims space-axis0))
(define dims (vector-ref all-dims 1))

(define type (H5Dget_type axis0))
(H5Tget_class type)
(H5Tget_size type)
(H5Tget_sign type)
(H5Tget_precision type)
(H5Tget_native_type type 'H5T_DIR_DEFAULT)


(define rdata-ptr (malloc (* (apply * dims) (H5Tget_size type))))

(set! status (H5Dread axis0 (H5Tget_native_type type 'H5T_DIR_DEFAULT)
                      H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr))

(define axis0-data (cblock->vector rdata-ptr _long (apply * dims)))




;; print all object names in the dataset and some of their properties
(define (iter-proc obj name info op-data)
  (define (print-val name val)
    (printf (~a "  -> " name ": " val  "\n")))
  (printf "~s:~n" name)
  (print-val "type" (H5O_info_t-type info))
  (print-val "rc" (H5O_info_t-rc info))
  (print-val "num_attrs"  (H5O_info_t-num_attrs info)))

(H5Ovisit fid 'H5_INDEX_NAME 'H5_ITER_NATIVE iter-proc #f)
  





;; (define group-info (H5Gget_info fid))
;; (define obj-info (H5Oget_info fid))

;; (H5O_info_t-type obj-info)
;; (H5O_info_t-fileno obj-info)
;; (H5O_info_t-rc obj-info)
;; (H5O_info_t-btime obj-info)



;; ;;(define nobjects (H5Gget_num_objs fid))

(define status (H5Fclose fid))

;;https://support.hdfgroup.org/HDF5/doc1.6/UG/11_Datatypes.html
