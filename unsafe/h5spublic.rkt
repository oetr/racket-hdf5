#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5ipublic.rkt")

(provide (all-defined-out))

;; Define atomic datatypes
(define H5S_ALL         0)
(define H5S_UNLIMITED (cast -1 _int64 hsize_t))

;; Define user-level maximum number of dimensions 
(define H5S_MAX_RANK    32)


;; Different types of dataspaces
(define H5S_class_t
  (_enum '(
           H5S_NO_CLASS = -1  ;; error
           H5S_SCALAR   =  0  ;; scalar variable
           H5S_SIMPLE   =  1  ;; simple data space
           H5S_NULL     =  2  ;; null data space
           )))

;; Different ways of combining selections
(define H5S_seloper_t
  (_enum
   '(
     H5S_SELECT_NOOP      = -1  ;; error
     H5S_SELECT_SET       = 0   ;; Select "set" operation
     H5S_SELECT_OR
     #| Binary "or" operation for hyperslabs
     * (add new selection to existing selection)
     * Original region:  AAAAAAAAAA
     * New region:             BBBBBBBBBB
     * A or B:           CCCCCCCCCCCCCCCC
     |#
     H5S_SELECT_AND
     #| Binary "and" operation for hyperslabs
     * (only leave overlapped regions in selection)
     * Original region:  AAAAAAAAAA
     * New region:             BBBBBBBBBB
     * A and B:                CCCC
     |#
     H5S_SELECT_XOR
     #| Binary "xor" operation for hyperslabs
     * (only leave non-overlapped regions in selection)
     * Original region:  AAAAAAAAAA
     * New region:             BBBBBBBBBB
     * A xor B:          CCCCCC    CCCCCC
     |#
     H5S_SELECT_NOTB
     #| Binary "not" operation for hyperslabs
     * (only leave non-overlapped regions in original selection)
     * Original region:  AAAAAAAAAA
     * New region:             BBBBBBBBBB
     * A not B:          CCCCCC
     |#
     H5S_SELECT_NOTA
     #| Binary "not" operation for hyperslabs
     * (only leave non-overlapped regions in new selection)
     * Original region:  AAAAAAAAAA
     * New region:             BBBBBBBBBB
     * B not A:                    CCCCCC
     |#
     H5S_SELECT_APPEND  ;; Append elements to end of point selection
     H5S_SELECT_PREPEND ;; Prepend elements to beginning of point selection
     H5S_SELECT_INVALID ;; Invalid upper bound on selection operations
     )))


;; Enumerated type for the type of selection
(define H5S_sel_type
  (_enum
   '(
     H5S_SEL_ERROR      = -1 ;; Error
     H5S_SEL_NONE       =  0 ;; Nothing selected
     H5S_SEL_POINTS     =  1 ;; Sequence of points selected
     H5S_SEL_HYPERSLABS =  2 ;; "New-style" hyperslab selection defined
     H5S_SEL_ALL        =  3 ;; Entire extent selected
     H5S_SEL_N               ;; THIS MUST BE LAST
     )))

;; Functions in H5S.c
(define-hdf5 H5Screate
  (_fun (type : H5S_class_t)
        -> hid_t))

(define-hdf5 H5Screate_simple
  (_fun (rank dims-in (maxdims-in '())) ::
        (rank : _int)
        (dims : (_list i hsize_t) = (seq->list dims-in))
        (maxdims : (_list i hsize_t) = (seq->list maxdims-in))
        -> hid_t))

(define-hdf5 H5Sset_extent_simple
  (_fun (space_id : hid_t)
        (rank : _int)
        (dims : (_list i hsize_t))
        (max : (_list i hsize_t))
        -> herr_t))

(define-hdf5 H5Scopy 
  (_fun (space_id : hid_t)
        -> hid_t))

(define-hdf5 H5Sclose
  (_fun (space_id : hid_t)
        -> (status : herr_t)
        -> (when (< status 0)
             (error 'H5Sclose "Failed to close space."))))

(define-hdf5 H5Sencode
  (_fun (obj_id : hid_t)
        (buf : _pointer)
        (nalloc : _pointer)
        -> herr_t))

(define-hdf5 H5Sdecode
  (_fun (buf : _pointer)
        -> hid_t))

(define-hdf5 H5Sget_simple_extent_npoints
  (_fun (space_id : hid_t)
        -> hssize_t))

(define-hdf5 H5Sget_simple_extent_ndims
  (_fun (space_id : hid_t)
        -> _int))


(define-hdf5 H5Sget_simple_extent_dims
  (_fun (space_id) ::
        (space_id : hid_t)
        (dims : (_list o hsize_t (H5Sget_simple_extent_ndims space_id)))
        (maxdims : (_list o hsize_t (H5Sget_simple_extent_ndims space_id)))
        -> (status : _int)
        -> (vector status dims maxdims)))

(define-hdf5 H5Sis_simple
  (_fun (space_id : hid_t)
        -> htri_t))

(define-hdf5 H5Sget_select_npoints
  (_fun (space_id : hid_t)
        -> hssize_t))

(define-hdf5 H5Sselect_hyperslab
  (_fun (space_id op start-in _stride-in count-in _block-in) ::
        (space_id : hid_t)
        (op : H5S_seloper_t)
        (start : (_list i hsize_t) = (seq->list start-in))
        (_stride : (_list i hsize_t) = (seq->list _stride-in))
        (count : (_list i hsize_t) = (seq->list count-in))
        (_block : (_list i hsize_t) = (seq->list _block-in))
        -> herr_t))


;; #ifdef NEW_HYPERSLAB_API
;; H5_DLL hid_t H5Scombine_hyperslab(hid_t space_id, H5S_seloper_t op,
;; 				   const hsize_t start[],
;; 				   const hsize_t _stride[],
;; 				   const hsize_t count[],
;; 				   const hsize_t _block[]);
;; H5_DLL herr_t H5Sselect_select(hid_t space1_id, H5S_seloper_t op,
;;                                   hid_t space2_id);
;; H5_DLL hid_t H5Scombine_select(hid_t space1_id, H5S_seloper_t op,
;;                                   hid_t space2_id);
;; #endif /* NEW_HYPERSLAB_API */


(define-hdf5 H5Sselect_elements
  (_fun (space_id : hid_t)
        (op : H5S_seloper_t)
        (num_elem : _size)
        (coord : (_ptr i hsize_t))
        -> herr_t))

(define-hdf5 H5Sget_simple_extent_type
  (_fun (space_id : hid_t)
        -> H5S_class_t))

(define-hdf5 H5Sset_extent_none
  (_fun (space_id : hid_t)
        -> herr_t))

(define-hdf5 H5Sextent_copy
  (_fun (dst_id : hid_t)
        (src_id : hid_t)
        -> herr_t))

(define-hdf5 H5Sextent_equal
  (_fun (sid1 : hid_t)
        (sid2 : hid_t)
        -> htri_t))

(define-hdf5 H5Sselect_all
  (_fun (space_id : hid_t)
        -> herr_t))

(define-hdf5 H5Sselect_none
  (_fun (space_id : hid_t)
        -> herr_t))

(define-hdf5 H5Soffset_simple
  (_fun (space_id : hid_t)
        (offset : (_ptr i hssize_t))
        -> herr_t))

(define-hdf5 H5Sselect_valid
  (_fun (space_id : hid_t)
        -> htri_t))

(define-hdf5 H5Sget_select_hyper_nblocks
  (_fun (space_id : hid_t)
        -> hssize_t))

(define-hdf5 H5Sget_select_elem_npoints
  (_fun (space_id : hid_t)
        -> hssize_t))

(define-hdf5 H5Sget_select_hyper_blocklist
  (_fun (space_id : hid_t)
        (startblock : hsize_t)
        (numblocks : hsize_t)
        (buf : _pointer)
        -> herr_t))

(define-hdf5 H5Sget_select_elem_pointlist
  (_fun (space_id : hid_t)
        (startpoint : hsize_t)
        (numpoints : hsize_t)
        (buf : _pointer)
        -> herr_t))

(define-hdf5 H5Sget_select_bounds
  (_fun (space_id : hid_t)
        (start : _pointer)
        (end : _pointer)
        -> herr_t))

(define-hdf5 H5Sget_select_type
  (_fun (space_id : hid_t)
        -> H5S_sel_type))
