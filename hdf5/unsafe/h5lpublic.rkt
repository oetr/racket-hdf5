#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5ipublic.rkt"
         "h5tpublic.rkt")

(provide (all-defined-out))

;;***************
;; Public Macros 
;;***************

;; Maximum length of a link's name
;; (encoded in a 32-bit unsigned integer)
(define H5L_MAX_LINK_NAME_LEN (cast -1 _int32 _uint32))  ;; (4GB - 1)

;; Macro to indicate operation occurs on same location
(define H5L_SAME_LOC (cast 0 _int32 hid_t))

;; Current version of the H5L_class_t struct
(define H5L_LINK_CLASS_T_VERS 0)


;;*****************
;; Public Typedefs 
;;*****************

#| Link class types.
 * Values less than 64 are reserved for the HDF5 library's internal use.
 * Values 64 to 255 are for "user-defined" link class types; these types are
 * defined by HDF5 but their behavior can be overridden by users.
 * Users who want to create new classes of links should contact the HDF5
 * development team at hdfhelp@ncsa.uiuc.edu .
 * These values can never change because they appear in HDF5 files.
|#
(define H5L_type_t
  (_enum '(
    H5L_TYPE_ERROR = -1     ;; Invalid link type id
    H5L_TYPE_HARD = 0         ;; Hard link id
    H5L_TYPE_SOFT = 1         ;; Soft link id
    H5L_TYPE_EXTERNAL = 64    ;; External link id
    H5L_TYPE_MAX = 255	      ;; Maximum link type id
)))
(define H5L_TYPE_BUILTIN_MAX 'H5L_TYPE_SOFT)      ;; Maximum value link value for "built-in" link types
(define H5L_TYPE_UD_MIN      'H5L_TYPE_EXTERNAL)  ;; Link ids at or above this value are "user-defined" link types.

;;Information struct for link (for H5Lget_info/H5Lget_info_by_idx)
(provide _H5L_info_t)
(provide make-H5L_info_t)
(provide _H5L_info_t-pointer/null)
(define-cstruct _H5L_info_t
  ([type H5L_type_t]      ;; Type of link
   [corder_valid hbool_t] ;; Indicate if creation order is valid
   [corder _int64]      ;; Creation order
   [cset H5T_cset_t]     ;; Character set of link name
   [u (_union haddr_t ;; address ;; Address hard link points to
              _size)] ;; val_size ;; Size of a soft link or UD link value
   ))


#| The H5L_class_t struct can be used to override the behavior of a
 * "user-defined" link class. Users should populate the struct with callback
 * functions defined below.
 |#
;; Callback prototypes for user-defined links
;; Link creation callback
(define-cpointer-type+provide _H5L_create_func_t)
;;typedef herr_t (*H5L_create_func_t)(const char *link_name, hid_t loc_group,
;;    const void *lnkdata, size_t lnkdata_size, hid_t lcpl_id);

;; Callback for when the link is moved
(define-cpointer-type+provide _H5L_move_func_t)
;; typedef herr_t (*H5L_move_func_t)(const char *new_name, hid_t new_loc,
;;     const void *lnkdata, size_t lnkdata_size);

;; Callback for when the link is copied
(define-cpointer-type+provide _H5L_copy_func_t)
;;typedef herr_t (*H5L_copy_func_t)(const char *new_name, hid_t new_loc,
;;    const void *lnkdata, size_t lnkdata_size);

;; Callback during link traversal
(define-cpointer-type+provide _H5L_traverse_func_t)
;;typedef hid_t (*H5L_traverse_func_t)(const char *link_name, hid_t cur_group,
;;    const void *lnkdata, size_t lnkdata_size, hid_t lapl_id);

;; Callback for when the link is deleted
(define-cpointer-type+provide _H5L_delete_func_t)
;;typedef herr_t (*H5L_delete_func_t)(const char *link_name, hid_t file,
;;    const void *lnkdata, size_t lnkdata_size);

;; Callback for querying the link
;; Returns the size of the buffer needed
(define-cpointer-type+provide _H5L_query_func_t)
;;typedef ssize_t (*H5L_query_func_t)(const char *link_name, const void *lnkdata,
;;    size_t lnkdata_size, void *buf ;; out*/, size_t buf_size);


;; User-defined link types
(define-cstruct _H5L_class_t
  ([version _int]                    ;; Version number of this struct       
   [id H5L_type_t]                  ;; Link type ID                        
   [comment _string]                ;; Comment for debugging               
   [create_func _H5L_create_func_t] ;; Callback during link creation       
   [move_func _H5L_move_func_t]     ;; Callback after moving link          
   [copy_func _H5L_copy_func_t]     ;; Callback after copying link         
   [trav_func _H5L_traverse_func_t] ;; Callback during link traversal      
   [del_func _H5L_delete_func_t]     ;; Callback for link deletion          
   [query_func _H5L_query_func_t]))  ;; Callback for queries                

;; Prototype for H5Literate/H5Literate_by_name() operator
;;(define-cpointer-type+provide H5L_iterate_t)
(define H5L_iterate_t
  (_fun (group : hid_t)
        (name : _string)
        (info : _H5L_info_t-pointer/null)
        (op_data : _pointer)
        -> herr_t))
;;typedef herr_t (*H5L_iterate_t)(hid_t group, const char *name, const H5L_info_t *info,
;;    void *op_data);

;; Callback for external link traversal
(define H5L_elink_traverse_t
  (_fun (parent_file_name : _string)
        (parent_group_name : _string)
        (child_file_name : _string)
        (child_object_name : _string)
        (acc_flags : _pointer)
        (fapl_id : hid_t)
        (op_data : _pointer)
        -> herr_t))

;;*******************
;; Public Prototypes 
;;*******************
(define-hdf5 H5Lmove
  (_fun (src_loc : hid_t)
        (src_name : _string)
        (dst_loc : hid_t)
        (dst_name : _string)
        (lcpl_id : hid_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lcopy
  (_fun (src_loc : hid_t)
        (src_name : _string)
        (dst_loc : hid_t)
        (dst_name : _string)
        (lcpl_id : hid_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lcreate_hard
  (_fun (cur_loc : hid_t)
        (cur_name : _string)
        (dst_loc : hid_t)
        (dst_name : _string)
        (lcpl_id : hid_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lcreate_soft
  (_fun (link_target : _string)
        (link_loc-id : hid_t)
        (link_name : _string)
        (lcpl_id : hid_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Ldelete
  (_fun (loc_id : hid_t)
        (name : _string)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Ldelete_by_idx
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (n : hsize_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lget_val
  (_fun (loc_id : hid_t)
        (name : _string)
        (buf : _pointer) ;; TODO: out
        (size : _size)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lget_val_by_idx
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (n : hsize_t)
        (buf : _pointer) ;; TODO: out
        (size : _size)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lexists
  (_fun (loc_id : hid_t)
        (name : _string)
        (lapl_id : hid_t)
        -> htri_t))

(define-hdf5 H5Lget_info
  (_fun (loc_id : hid_t)
        (name : _string)
        (linfo : _H5L_info_t) ;; TODO:out
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lget_info_by_idx
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (n : hsize_t)
        (linfo : _H5L_info_t) ;; TODO: out
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lget_name_by_idx
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (n : hsize_t)
        (name : _pointer) ;; TODO: out
        (size : _size)
        (lapl_id : hid_t)
        -> _ssize))

(define-hdf5 H5Literate
  (_fun (grp_id : hid_t)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (idx : _pointer)
        (op : H5L_iterate_t)
        (op_data : _pointer)
        -> herr_t))

(define-hdf5 H5Literate_by_name
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (idx : _pointer)
        (op : H5L_iterate_t)
        (op_data : _pointer)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lvisit
  (_fun (grp_id : hid_t)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (op : H5L_iterate_t)
        (op_data : _pointer)
        -> herr_t))

(define-hdf5 H5Lvisit_by_name
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (op : H5L_iterate_t)
        (op_data : _pointer)
        (lapl_id : hid_t)
        -> herr_t))

;; UD link functions
(define-hdf5 H5Lcreate_ud
  (_fun (loc_id : hid_t)
        (link_name : _string)
        (link_type : H5L_type_t)
        (udata : _pointer)
        (udata_size : _size)
        (lcpl_id : hid_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Lregister
  (_fun (cls : _H5L_class_t-pointer/null)
        -> herr_t))

(define-hdf5 H5Lunregister
  (_fun (id : H5L_type_t)
        -> herr_t))

(define-hdf5 H5Lis_registered
  (_fun (id : H5L_type_t)
        -> htri_t))

;; External link functions
(define-hdf5 H5Lunpack_elink_val
  (_fun (ext_linkval : _pointer) ;; TODO: in
        (link_size : _size)
        (flags : _pointer)
        (filename : _pointer) ;; TODO: out
        (obj_path : _pointer) ;; TODO: out
        -> herr_t))

(define-hdf5 H5Lcreate_external
  (_fun (file_name : _string)
        (obj_name : _string)
        (link_loc_id : hid_t)
        (link_name : _string)
        (lcpl_id : hid_t)
        (lapl_id : hid_t)
        -> herr_t))
