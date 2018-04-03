#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5acpublic.rkt"
         "h5ipublic.rkt")

#|
* These are the bits that can be passed to the `flags' argument of
* H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
* them as needed.  As a side effect, they call H5check_version() to make sure
* that the application is compiled with a version of the hdf5 header files
* which are compatible with the library to which the application is linked.
* We're assuming that these constants are used rather early in the hdf5
* session.
*
* H5F_ACC_DEBUG no longer has any prints any special debug info. The symbol is
* being retained and will be listed as deprecated in HDF5 1.10.0.
|#
(define+provide H5F_ACC_RDONLY #x0000) ;; absence of rdwr => rd-only
(define+provide H5F_ACC_RDWR   #x0001) ;; open for read and write
(define+provide H5F_ACC_TRUNC  #x0002) ;; overwrite existing files
(define+provide H5F_ACC_EXCL   #x0004) ;; fail if file already exists
(define+provide H5F_ACC_DEBUG  #x0000) ;; print debug info (no longer used)
(define+provide H5F_ACC_CREAT  #x0010) ;; create non-existing files



#| Value passed to H5Pset_elink_acc_flags to cause flags to be taken from the
* parent file. |#
(define+provide H5F_ACC_DEFAULT #xffff)	;;ignore setting on lapl     */

;; Flags for H5Fget_obj_count() & H5Fget_obj_ids() calls */
(define+provide H5F_OBJ_FILE	#x0001)       ;; File objects */
(define+provide H5F_OBJ_DATASET	#x0002)       ;; Dataset objects */
(define+provide H5F_OBJ_GROUP	#x0004)       ;; Group objects */
(define+provide H5F_OBJ_DATATYPE #x0008)      ;; Named datatype objects */
(define+provide H5F_OBJ_ATTR    #x0010)       ;; Attribute objects */
(define+provide H5F_OBJ_ALL (bitwise-ior H5F_OBJ_FILE H5F_OBJ_DATASET H5F_OBJ_GROUP H5F_OBJ_DATATYPE H5F_OBJ_ATTR))
(define+provide H5F_OBJ_LOCAL #x0020)       ;; Restrict search to objects opened through current file ID */
;; (as opposed to objects opened through any file ID accessing this file) */

(define+provide H5F_FAMILY_DEFAULT 0)

;; The difference between a single file and a set of mounted files
(define+provide H5F_scope_t
  (_enum '(
           H5F_SCOPE_LOCAL	= 0     ;; specified file handle only
           H5F_SCOPE_GLOBAL	= 1))) 	;; entire virtual file

;; Unlimited file size for H5Pset_external()
(define+provide H5F_UNLIMITED (cast -1 _int64 hsize_t))

#| How does file close behave?
 * H5F_CLOSE_DEFAULT - Use the degree pre-defined by underlining VFL
 * H5F_CLOSE_WEAK    - file closes only after all opened objects are closed
 * H5F_CLOSE_SEMI    - if no opened objects, file is close; otherwise, file
		       close fails
 * H5F_CLOSE_STRONG  - if there are opened objects, close them first, then
		       close file
|#
(define+provide H5F_close_degree_t
  (_enum '(
           H5F_CLOSE_DEFAULT   = 0
           H5F_CLOSE_WEAK      = 1
           H5F_CLOSE_SEMI      = 2
           H5F_CLOSE_STRONG    = 3)))

;; Current "global" information about file
;; (just size info currently)
(provide _H5F_info_t)
(provide make-H5F_info_t)
(provide _H5F_info_t-pointer/null)
(define-cstruct _H5F_info_t
  ([super_ext_size hsize_t]
   [sohm (make-cstruct-type (list hsize_t _H5_ih_info_t-pointer/null))]))


#|
 * Types of allocation requests. The values larger than H5FD_MEM_DEFAULT
 * should not change other than adding new types to the end. These numbers
 * might appear in files.
 *
 * Note: please change the log VFD flavors array if you change this
 * enumeration.
|#
(define+provide H5F_mem_t
  (_enum
   '(
     H5FD_MEM_NOLIST     = -1   #| Data should not appear in the free list.
     * Must be negative.
     |#
     H5FD_MEM_DEFAULT    = 0    #| Value not yet set.  Can also be the
     * datatype set in a larger allocation
     * that will be suballocated by the library.
     * Must be zero.
     |#
     H5FD_MEM_SUPER      = 1    ;; Superblock data
     H5FD_MEM_BTREE      = 2    ;; B-tree data
     H5FD_MEM_DRAW       = 3    ;; Raw data (content of datasets etc.)
     H5FD_MEM_GHEAP      = 4    ;; Global heap data
     H5FD_MEM_LHEAP      = 5    ;; Local heap data
     H5FD_MEM_OHDR       = 6    ;; Object header data
     H5FD_MEM_NTYPES             ;; Sentinel value - must be last
     )))


;; Library's file format versions
(define+provide H5F_libver_t
  (_enum
   '(
     H5F_LIBVER_EARLIEST ;; Use the earliest possible format for storing objects
     H5F_LIBVER_LATEST ;;Use the latest possible format available for storing objects
     )))
#| Define file format version for 1.8 to prepare for 1.10 release.  
 * (Not used anywhere now)|#
(define+provide H5F_LIBVER_18 'H5F_LIBVER_LATEST)


;; Functions in H5F.c
(define-hdf5 H5Fis_hdf5
  (_fun (filename : _path)
        -> htri_t))

(define-hdf5 H5Fcreate
  (_fun (filename     : _path)
        (flags        : _uint)
        (create_plist : hid_t)
        (access_plist : hid_t)
        -> (file-identifier : hid_t)
        -> (if (< file-identifier 0)
               (error 'H5Fcreate "unable to create file.")
               file-identifier)))

(define-hdf5 H5Fopen
  (_fun (filename     : _path)
        (flags        : _uint)
        (access_plist : hid_t)
        -> (identifier : hid_t)
        -> (if (< identifier 0)
               (error 'H5Fopen "unable to open file.")
               identifier)))

(define-hdf5 H5Freopen
  (_fun (file_id : hid_t)
        -> hid_t))

(define-hdf5 H5Fflush
  (_fun (object_id : hid_t)
        (scope : H5F_scope_t)
        -> herr_t))

(define-hdf5 H5Fclose
  (_fun (file_id : hid_t)
        -> herr_t))

(define-hdf5 H5Fget_create_plist
  (_fun (file_id : hid_t)
        -> hid_t))

(define-hdf5 H5Fget_access_plist
  (_fun (file_id : hid_t)
        -> hid_t))

(define-hdf5 H5Fget_intent
  (_fun (file_id : hid_t)
        (intent : _pointer)
        -> herr_t))

(define-hdf5 H5Fget_obj_count
  (_fun (file_id : hid_t)
        (types : _uint)
        -> _ssize))

(define-hdf5 H5Fget_obj_ids
  (_fun (file_id : hid_t)
        (types : _uint)
        (max_objs : _size)
        (obj_id_list : _pointer)
        -> _ssize))

(define-hdf5 H5Fget_vfd_handle
  (_fun (file_id : hid_t)
        (fapl : hid_t)
        (file_handle : _pointer)
        -> herr_t))

(define-hdf5 H5Fmount
  (_fun (loc : hid_t)
        (name : _string)
        (child : hid_t)
        (plist : hid_t)
        -> herr_t))

(define-hdf5 H5Funmount
  (_fun (loc : hid_t)
        (name : _string)
        -> herr_t))

(define-hdf5 H5Fget_freespace
  (_fun (file_id : hid_t)
        -> hssize_t))

(define-hdf5 H5Fget_filesize
  (_fun (file_id : hid_t)
        (size : _pointer)
        -> hssize_t))

(define-hdf5 H5Fget_file_image
  (_fun (file_id : hid_t)
        (buf_ptr : _pointer)
        (buf_len : _size)
        -> _ssize))

(define-hdf5 H5Fget_mdc_config
  (_fun (file_id : hid_t)
        (config_ptr : _H5AC_cache_config_t-pointer/null)
        -> herr_t))

(define-hdf5 H5Fset_mdc_config
  (_fun (file_id : hid_t)
        (config_ptr : _H5AC_cache_config_t-pointer/null)
        -> herr_t))

(define-hdf5 H5Fget_mdc_hit_rate
  (_fun (file_id : hid_t)
        (hit_date_ptr : _pointer)
        -> herr_t))

(define-hdf5 H5Fget_mdc_size
  (_fun (file_id : hid_t)
        (max_size_ptr : _pointer)
        (min_clean_size_ptr : _pointer)
        (cur_size_ptr : _pointer)
        (cur_num_entries_ptr : _pointer)
        -> herr_t))

(define-hdf5 H5Freset_mdc_hit_rate_stats
  (_fun (file_id : hid_t)
        -> herr_t))

(define-hdf5 H5Fget_name
  (_fun (obj_id : hid_t)
        (name : _string)
        (size : _size)
        -> _ssize))

(define-hdf5 H5Fget_info
  (_fun (obj_id : hid_t)
        (bh_info : _H5F_info_t-pointer/null)
        -> herr_t))

(define-hdf5 H5Fclear_elink_file_cache
  (_fun (file_id : hid_t)
        -> herr_t))

;; #ifdef H5_HAVE_PARALLEL
;; H5_DLL herr_t H5Fset_mpi_atomicity(hid_t file_id, hbool_t flag);
;; H5_DLL herr_t H5Fget_mpi_atomicity(hid_t file_id, hbool_t *flag);
