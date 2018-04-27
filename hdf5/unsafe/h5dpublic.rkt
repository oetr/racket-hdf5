#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5ipublic.rkt") ;; ids

(provide (all-defined-out))

#|****************|#
#| Public Macros  |#
#|****************|#

;; Macros used to "unset" chunk cache configuration parameters
(define H5D_CHUNK_CACHE_NSLOTS_DEFAULT      (cast -1 _int64 _size))
(define H5D_CHUNK_CACHE_NBYTES_DEFAULT      (cast -1 _int64 _size))
(define H5D_CHUNK_CACHE_W0_DEFAULT          (cast -1.0 _float _float))

;; Property names for H5LTDdirect_chunk_write
(define H5D_XFER_DIRECT_CHUNK_WRITE_FLAG_NAME	  "direct_chunk_flag")
(define H5D_XFER_DIRECT_CHUNK_WRITE_FILTERS_NAME  "direct_chunk_filters")
(define H5D_XFER_DIRECT_CHUNK_WRITE_OFFSET_NAME	  "direct_chunk_offset")
(define H5D_XFER_DIRECT_CHUNK_WRITE_DATASIZE_NAME "direct_chunk_datasize")


;;******************/
;; Public Typedefs */
;;******************/

;; Values for the H5D_LAYOUT property */
(define H5D_layout_t
  (_enum
   '(
     H5D_LAYOUT_ERROR = -1
     H5D_COMPACT = 0 ;; raw data is very small
     H5D_CONTIGUOUS = 1 ;;the default         */
     H5D_CHUNKED  = 2 ;;slow and fancy        */
     H5D_NLAYOUTS = 3 ;;this one must be last!       */
     )))

;; Types of chunk index data structures */
(define H5D_chunk_index_t
  (_enum
   '(
     H5D_CHUNK_BTREE = 0 ;; v1 B-tree index        */
     )))

;; Values for the space allocation time property */
(define H5D_alloc_time_t
  (_enum
   '(
     H5D_ALLOC_TIME_ERROR = -1
     H5D_ALLOC_TIME_DEFAULT   = 0
     H5D_ALLOC_TIME_EARLY = 1
     H5D_ALLOC_TIME_LATE  = 2
     H5D_ALLOC_TIME_INCR  = 3
     )))

;; Values for the status of space allocation */
(define H5D_space_status_t
  (_enum
   '(
     H5D_SPACE_STATUS_ERROR  = -1
     H5D_SPACE_STATUS_NOT_ALLOCATED = 0
     H5D_SPACE_STATUS_PART_ALLOCATED = 1
     H5D_SPACE_STATUS_ALLOCATED  = 2
     )))

;; Values for time of writing fill value property */
(define H5D_fill_time_t
  (_enum
   '(
     H5D_FILL_TIME_ERROR = -1
     H5D_FILL_TIME_ALLOC = 0
     H5D_FILL_TIME_NEVER = 1
     H5D_FILL_TIME_IFSET = 2
     )))

;; Values for fill value status */
(define H5D_fill_value_t
  (_enum
   '(
     H5D_FILL_VALUE_ERROR        = -1
     H5D_FILL_VALUE_UNDEFINED    = 0
     H5D_FILL_VALUE_DEFAULT      = 1
     H5D_FILL_VALUE_USER_DEFINED = 2
     )))


;; Define the operator function pointer for H5Diterate()
(define-cpointer-type _H5D_operator_t)

;; Define the operator function pointer for H5Dscatter()
(define-cpointer-type _H5D_scatter_func_t)

;; Define the operator function pointer for H5Dgather()
(define-cpointer-type _H5D_gather_func_t)


(define-hdf5 H5Dcreate2
  (_fun (loc_id : hid_t)
        (name : _string)
        (type_id : hid_t)
        (space_id : hid_t)
        (lcpl_id : hid_t)
        (dcpl_id : hid_t)
        (dapl_id : hid_t)
        -> hid_t))

(define H5Dcreate H5Dcreate2)

(define-hdf5 H5Dcreate_anon
  (_fun (file_id : hid_t)
        (type_id : hid_t)
        (space_id : hid_t)
        (plist_id : hid_t)
        (dapl_id : hid_t)
        -> hid_t))

(define-hdf5 H5Dopen2
  (_fun (file_id : hid_t)
        (name : _string)
        (dapl_id : hid_t)
        -> hid_t))

(define H5Dopen H5Dopen2)

(define-hdf5 H5Dclose
  (_fun (dset_id : hid_t)
        -> (status : herr_t)
        -> (when (< status 0)
             (error 'H5Dclose "Failed to close dataset."))))

(define-hdf5 H5Dget_space
  (_fun (dset_id : hid_t)
        -> hid_t))

(define-hdf5 H5Dget_space_status
  (_fun (dset_id : hid_t)
        (allocation : H5D_space_status_t)
        -> herr_t))

(define-hdf5 H5Dget_type
  (_fun (dset_id : hid_t)
        -> hid_t))

(define-hdf5 H5Dget_create_plist
  (_fun (dset_id : hid_t)
        -> hid_t))

(define-hdf5 H5Dget_access_plist
  (_fun (dset_id : hid_t)
        -> hid_t))

(define-hdf5 H5Dget_storage_size
  (_fun (dset_id : hid_t)
        -> hsize_t))

(define-hdf5 H5Dget_offset
  (_fun (dset_id : hid_t)
        -> haddr_t))

(define-hdf5 H5Dread
  (_fun (dset_id : hid_t)
        (mem_type_id : hid_t)
        (mem_space_id : hid_t)
        (file_space_id : hid_t)
        (plist_id : hid_t)
        (buf : _pointer) ;; TODO: out
        -> (status : herr_t)
        -> (when (< status 0)
             (error 'H5Dread "Failed to read data."))))

(define-hdf5 H5Dwrite
  (_fun (dset_id mem_type_id mem_space_id file_space_id plist_id buf) ::
        (dset_id : hid_t)
        (mem_type_id : hid_t)
        (mem_space_id : hid_t)
        (file_space_id : hid_t)
        (plist_id : hid_t)
        (buf : _pointer)
        -> (status : herr_t)
        -> (when (< status 0)
             (error 'H5Dwrite "Failed to write dataset."))))


(define-hdf5 H5Diterate
  (_fun (buf : _pointer)
        (type_id : hid_t)
        (space_id : hid_t)
        (op : _H5D_operator_t/null)
        (operator_data : _pointer)
        -> herr_t))

(define-hdf5 H5Dvlen_reclaim
  (_fun (type_id : hid_t)
        (space_id : hid_t)
        (plist_id : hid_t)
        (buf : _pointer)
        -> (status : herr_t)
        -> (when (< status 0)
             (error 'H5Dclose "Failed to reclaim vlen."))))

(define-hdf5 H5Dvlen_get_buf_size
  (_fun (dataset_id type_id space_id) ::
        (dataset_id : hid_t)
        (type_id : hid_t)
        (space_id : hid_t)
        (size : _pointer = (malloc hsize_t 1 'atomic-interior))
        -> (status : herr_t)
        -> (list status (ptr-ref size hsize_t))))

(define-hdf5 H5Dfill
  (_fun (fill : (_ptr i _void))
        (fill_type : hid_t)
        (buf : _pointer)
        (buf_type : hid_t)
        (space : hid_t)
        -> herr_t))

(define-hdf5 H5Dset_extent
  (_fun (dset_id size-in) ::
        (dset_id : hid_t)
        (size : (_list i hsize_t) = (seq->list size-in))
        -> herr_t))

(define-hdf5 H5Dscatter
  (_fun (op : _H5D_scatter_func_t/null)
        (op_data : _pointer)
        (type_id : hid_t)
        (dst_space_id : hid_t)
        (dst_buf : _pointer)
        -> herr_t))

(define-hdf5 H5Dgather
  (_fun (src_space_id : hid_t)
        (src_buf : (_ptr i _void))
        (type_id : hid_t)
        (dst_buf_size : _size)
        (dst_buf : _pointer)
        (op : _H5D_gather_func_t/null)
        (op_data : _pointer)
        -> herr_t))


(define-hdf5 H5Ddebug
  (_fun (dset_id : hid_t)
        -> herr_t))

#| Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 |#


;; Function prototypes
(define-hdf5 H5Dcreate1
  (_fun (file_id : hid_t)
        (name : _string)
        (type_id : hid_t)
        (space_id : hid_t)
        (dcpl_id : hid_t)
        -> hid_t))

(define-hdf5 H5Dopen1
  (_fun (file_id : hid_t)
        (name : _string)
        -> hid_t))

(define-hdf5 H5Dextend
  (_fun (dset_id : hid_t)
        (size : (_ptr i hsize_t))
        -> herr_t))
