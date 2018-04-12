#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5acpublic.rkt"
         "h5dpublic.rkt"
         "h5fpublic.rkt"
         "h5fdpublic.rkt"
         "h5ipublic.rkt"
         "h5lpublic.rkt"
         "h5opublic.rkt"
         "h5mmpublic.rkt"
         "h5tpublic.rkt"
         "h5zpublic.rkt")

(provide (all-defined-out))

#|
* The library's property list classes
|#

(define H5P_ROOT (dynamic-constant 'H5P_CLS_ROOT hdf5-lib hid_t))
(define H5P_OBJECT_CREATE (dynamic-constant 'H5P_CLS_OBJECT_CREATE hdf5-lib hid_t))
(define H5P_FILE_CREATE (dynamic-constant 'H5P_CLS_FILE_CREATE hdf5-lib hid_t))
(define H5P_FILE_ACCESS (dynamic-constant 'H5P_CLS_FILE_ACCESS hdf5-lib hid_t))
(define H5P_DATASET_CREATE (dynamic-constant 'H5P_CLS_DATASET_CREATE hdf5-lib hid_t))
(define H5P_DATASET_ACCESS (dynamic-constant 'H5P_CLS_DATASET_ACCESS hdf5-lib hid_t))
(define H5P_DATASET_XFER (dynamic-constant 'H5P_CLS_DATASET_XFER hdf5-lib hid_t))
(define H5P_FILE_MOUNT (dynamic-constant 'H5P_CLS_FILE_MOUNT hdf5-lib hid_t))
(define H5P_GROUP_CREATE (dynamic-constant 'H5P_CLS_GROUP_CREATE hdf5-lib hid_t))
(define H5P_GROUP_ACCESS (dynamic-constant 'H5P_CLS_GROUP_ACCESS hdf5-lib hid_t))
(define H5P_DATATYPE_CREATE (dynamic-constant 'H5P_CLS_DATATYPE_CREATE hdf5-lib hid_t))
(define H5P_DATATYPE_ACCESS (dynamic-constant 'H5P_CLS_DATATYPE_ACCESS hdf5-lib hid_t))
(define H5P_STRING_CREATE (dynamic-constant 'H5P_CLS_STRING_CREATE hdf5-lib hid_t))
(define H5P_ATTRIBUTE_CREATE (dynamic-constant 'H5P_CLS_ATTRIBUTE_CREATE hdf5-lib hid_t))
(define H5P_OBJECT_COPY (dynamic-constant 'H5P_CLS_OBJECT_COPY hdf5-lib hid_t))
(define H5P_LINK_CREATE (dynamic-constant 'H5P_CLS_LINK_CREATE hdf5-lib hid_t))
(define H5P_LINK_ACCESS (dynamic-constant 'H5P_CLS_LINK_ACCESS hdf5-lib hid_t))


#|
* The library's default property lists
|#
(define H5P_FILE_CREATE_DEFAULT (dynamic-constant 'H5P_LST_FILE_CREATE hdf5-lib hid_t))
(define H5P_FILE_ACCESS_DEFAULT (dynamic-constant 'H5P_LST_FILE_ACCESS hdf5-lib hid_t))
(define H5P_DATASET_CREATE_DEFAULT (dynamic-constant 'H5P_LST_DATASET_CREATE hdf5-lib hid_t))
(define H5P_DATASET_ACCESS_DEFAULT (dynamic-constant 'H5P_LST_DATASET_ACCESS hdf5-lib hid_t))
(define H5P_DATASET_XFER_DEFAULT (dynamic-constant 'H5P_LST_DATASET_XFER hdf5-lib hid_t))
(define H5P_FILE_MOUNT_DEFAULT (dynamic-constant 'H5P_LST_FILE_MOUNT hdf5-lib hid_t))
(define H5P_GROUP_CREATE_DEFAULT (dynamic-constant 'H5P_LST_GROUP_CREATE hdf5-lib hid_t))
(define H5P_GROUP_ACCESS_DEFAULT (dynamic-constant 'H5P_LST_GROUP_ACCESS hdf5-lib hid_t))
(define H5P_DATATYPE_CREATE_DEFAULT (dynamic-constant 'H5P_LST_DATATYPE_CREATE hdf5-lib hid_t))
(define H5P_DATATYPE_ACCESS_DEFAULT (dynamic-constant 'H5P_LST_DATATYPE_ACCESS hdf5-lib hid_t))
(define H5P_ATTRIBUTE_CREATE_DEFAULT (dynamic-constant 'H5P_LST_ATTRIBUTE_CREATE hdf5-lib hid_t))
(define H5P_OBJECT_COPY_DEFAULT (dynamic-constant 'H5P_LST_OBJECT_COPY hdf5-lib hid_t))
(define H5P_LINK_CREATE_DEFAULT (dynamic-constant 'H5P_LST_LINK_CREATE hdf5-lib hid_t))
(define H5P_LINK_ACCESS_DEFAULT (dynamic-constant 'H5P_LST_LINK_ACCESS hdf5-lib hid_t))


;; Common creation order flags (for links in groups and attributes on objects)
(define H5P_CRT_ORDER_TRACKED           #x0001)
(define H5P_CRT_ORDER_INDEXED           #x0002)

;; Default value for all property list classes
(define H5P_DEFAULT 0)

;; Define property list class callback function pointer types
;; typedef herr_t (*H5P_cls_create_func_t)(hid_t prop_id, void *create_data);
(define H5P_cls_create_func_t
  (_fun (prop_id : hid_t)
        (create_data : _pointer)
        -> herr_t))

;; typedef herr_t (*H5P_cls_copy_func_t)(hid_t new_prop_id, hid_t old_prop_id,
;;                                       void *copy_data);
(define H5P_cls_copy_func_t
  (_fun (new_prop_id : hid_t)
        (old_prop_id : hid_t)
        (copy_data : _pointer)
        -> herr_t))

;; typedef herr_t (*H5P_cls_close_func_t)(hid_t prop_id, void *close_data);
(define H5P_cls_close_func_t
  (_fun (prop_id : hid_t)
        (close_data : _pointer)
        -> herr_t))

;; Define property list callback function pointer types
;; typedef herr_t (*H5P_prp_cb1_t)(const char *name, size_t size, void *value);
(define H5P_prp_cb1_t
  (_fun (name : _string)
        (size : _size)
        (value : _pointer)
        -> herr_t))

;; typedef herr_t (*H5P_prp_cb2_t)(hid_t prop_id, const char *name, size_t size, void *value);
(define H5P_prp_cb2_t
  (_fun (prop_id : hid_t)
        (name : _string)
        (size : _size)
        (value : _pointer)
        -> herr_t))

(define H5P_prp_create_func_t H5P_prp_cb1_t)
(define H5P_prp_set_func_t H5P_prp_cb2_t)
(define H5P_prp_get_func_t H5P_prp_cb2_t)
(define H5P_prp_delete_func_t H5P_prp_cb2_t)
(define H5P_prp_copy_func_t H5P_prp_cb1_t)


;; typedef int (*H5P_prp_compare_func_t)(const void *value1, const void *value2, size_t size);
(define H5P_prp_compare_func_t
  (_fun (value1 : _pointer)
        (value2 : _pointer)
        (size : _size)
        -> _int))

(define H5P_prp_close_func_t H5P_prp_cb1_t)

;; Define property list iteration function type
;; typedef herr_t (*H5P_iterate_t)(hid_t id, const char *name, void *iter_data);
(define H5P_iterate_t
  (_fun (id : hid_t)
        (name : _string)
        (iter_data : _pointer)
        -> herr_t))

;; Actual IO mode property
(define H5D_mpio_actual_chunk_opt_mode_t
  (_enum
   '(
     #| The default value, H5D_MPIO_NO_CHUNK_OPTIMIZATION, is used for all I/O
     * operations that do not use chunk optimizations, including non-collective
     * I/O and contiguous collective I/O.
     |#
     H5D_MPIO_NO_CHUNK_OPTIMIZATION = 0
     H5D_MPIO_LINK_CHUNK
     H5D_MPIO_MULTI_CHUNK
    )))

(define H5D_mpio_actual_io_mode_t
  (_enum
   `(
     #| The following four values are conveniently defined as a bit field so that
     * we can switch from the default to indpendent or collective and then to
     * mixed without having to check the original value. 
     * 
     * NO_COLLECTIVE means that either collective I/O wasn't requested or that 
     * no I/O took place.
     *
     * CHUNK_INDEPENDENT means that collective I/O was requested, but the
     * chunk optimization scheme chose independent I/O for each chunk.
     |#
     H5D_MPIO_NO_COLLECTIVE = #x0
     H5D_MPIO_CHUNK_INDEPENDENT = #x1
     H5D_MPIO_CHUNK_COLLECTIVE = #x2
     H5D_MPIO_CHUNK_MIXED = ,(bitwise-ior #x1 #x2)
     ;; The contiguous case is separate from the bit field.
     H5D_MPIO_CONTIGUOUS_COLLECTIVE = #x4
    )))

;; Broken collective IO property
(define H5D_mpio_no_collective_cause_t
  (_enum
   '(
     H5D_MPIO_COLLECTIVE = #x00
     H5D_MPIO_SET_INDEPENDENT = #x01
     H5D_MPIO_DATATYPE_CONVERSION = #x02
     H5D_MPIO_DATA_TRANSFORMS = #x04
     H5D_MPIO_MPI_OPT_TYPES_ENV_VAR_DISABLED = #x08
     H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES = #x10
     H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET = #x20
     H5D_MPIO_FILTERS = #x40
    )))




;;*******************
;; Public Prototypes 
;;*******************

;; Generic property list routines 
(define-hdf5 H5Pcreate_class
  (_fun (parent : hid_t)
        (name : _string)
        (cls_create : H5P_cls_create_func_t)
        (create_data : _pointer)
        (cls_copy : H5P_cls_copy_func_t)
        (copy_data : _pointer)
        (cls_close : H5P_cls_close_func_t)
        (close_data : _pointer)
        -> hid_t))

(define-hdf5 H5Pget_class_name
  (_fun (pclass_id : hid_t)
        -> _pointer))

(define-hdf5 H5Pcreate
  (_fun (cls_id     : hid_t)
        -> (result : hid_t)))

(define-hdf5 H5Pregister2
  (_fun (cls_id : hid_t)
        (name : _string)
        (size : _size)
        (def_value : _pointer)
        (prp_create :  H5P_prp_create_func_t)
        (prp_set : H5P_prp_set_func_t)
        (prp_get : H5P_prp_get_func_t)
        (prp_del : H5P_prp_delete_func_t)
        (prp_copy : H5P_prp_copy_func_t)
        (prp_cmp : H5P_prp_compare_func_t)
        (prp_close : H5P_prp_close_func_t) -> herr_t))

(define-hdf5 H5Pinsert2
  (_fun (plist_id : hid_t)
        (name : _string)
        (size : _size)
        (value : _pointer)
        (prp_set : H5P_prp_set_func_t)
        (prp_get : H5P_prp_get_func_t)
        (prp_delete : H5P_prp_delete_func_t)
        (prp_copy : H5P_prp_copy_func_t)
        (prp_cmp : H5P_prp_compare_func_t)
        (prp_close : H5P_prp_close_func_t)
        -> herr_t))

(define-hdf5 H5Pset
  (_fun (plist_id : hid_t)
        (name : _string)
        (value : _pointer)
        -> herr_t))

(define-hdf5 H5Pexist
  (_fun (plist_id : hid_t)
        (name : _string)
        -> htri_t))

(define-hdf5 H5Pget_size
  (_fun (id : hid_t)
        (name : _string)
        (size : _pointer) ;; TODO: out
        -> herr_t))

(define-hdf5 H5Pget_nprops
  (_fun (id : hid_t)
        (nprops : _pointer)
        -> herr_t))

(define-hdf5 H5Pget_class
  (_fun (plist_id : hid_t)
        -> hid_t))

(define-hdf5 H5Pget_class_parent
  (_fun (pclass_id : hid_t)
        -> hid_t))

(define-hdf5 H5Pget
  (_fun (plist_id : hid_t)
        (name : _string)
        (value : _pointer)
        -> herr_t))

(define-hdf5 H5Pequal
  (_fun (id1 : hid_t)
        (id2 : hid_t)
        -> htri_t))

(define-hdf5 H5Pisa_class
  (_fun (plist_id : hid_t)
        (pclass_id : hid_t)
        -> htri_t))

(define-hdf5 H5Piterate
  (_fun (id : hid_t)
        (idx : _pointer)
        (iter_func : H5P_iterate_t)
        (iter_data : _pointer)
        -> _int))

(define-hdf5 H5Pcopy_prop
  (_fun (dst_id : hid_t)
        (src_id : hid_t)
        (name : _string)
        -> herr_t))

(define-hdf5 H5Premove
  (_fun (plist_id : hid_t)
        (name : _pointer)
        -> herr_t))

(define-hdf5 H5Punregister
  (_fun (pclass_id : hid_t)
        (name : _string)
        -> herr_t))

(define-hdf5 H5Pclose_class
  (_fun (plist_id : hid_t)
        -> herr_t))

(define-hdf5 H5Pclose
  (_fun (plist_id : hid_t)
        -> herr_t))

(define-hdf5 H5Pcopy
  (_fun (plist_id : hid_t)
        -> hid_t))

;; Object creation property list (OCPL) routines 
(define-hdf5 H5Pset_attr_phase_change
  (_fun (plist_id : hid_t)
        (max_compact : _uint)
        (min_dense : _uint)
        -> herr_t))

(define-hdf5 H5Pget_attr_phase_change
  (_fun (plist_id : hid_t)
        (max_compact : _pointer)
        (min_dense : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_attr_creation_order
  (_fun (plist_id : hid_t)
        (crt_order_flags : _uint)
        -> herr_t))

(define-hdf5 H5Pget_attr_creation_order
  (_fun (plist_id : hid_t)
        (crt_order_flags : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_obj_track_times
  (_fun (plist_id : hid_t)
        (track_times : hbool_t)
        -> herr_t))

(define-hdf5 H5Pget_obj_track_times
  (_fun (plist_id : hid_t)
        (track_times : _pointer)
        -> herr_t))

(define-hdf5 H5Pmodify_filter
  (_fun (plist_id : hid_t)
        (filter : H5Z_filter_t)
        (flags : _uint)
        (cd_nelmts : _size)
        (cd_values : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_filter
  (_fun (plist_id : hid_t)
        (filter : H5Z_filter_t)
        (flags : _uint)
        (cd_nelmts : _size)
        (c_values : _pointer) ;; TODO: cont 
        -> herr_t))

(define-hdf5 H5Pget_nfilters
  (_fun (plist_id : hid_t)
        -> _int))

(define-hdf5 H5Pget_filter2
  (_fun (plist_id : hid_t)
        (filter : _uint)
        (flagsout : _uint) ;; TODO: out
        (cd_nelmts : _size) ;; TODO out
        (cd_values : _uint) ;;TODO
        (namelen : _size)
        (name : _string)
        (filter_config : _pointer)
        -> H5Z_filter_t)) ;; out

(define-hdf5 H5Pget_filter_by_id2
  (_fun (plist_id : hid_t)
        (id : H5Z_filter_t)
        (flags : _uint) ;; TODO out
        (cd_nelmts : _pointer) ;; TODO out
        (cd_values : _pointer) ;; TODO: out uint[]
        (namelen : _size)
        (name : _string) ;; TODO out
        (filter_config : _pointer) ;; TODO out
        -> herr_t))

(define-hdf5 H5Pall_filters_avail
  (_fun (plist_id : hid_t)
        -> htri_t))

(define-hdf5 H5Premove_filter
  (_fun (plist_id : hid_t)
        (filter : H5Z_filter_t)
        -> herr_t))

(define-hdf5 H5Pset_deflate
  (_fun (plist_id : hid_t)
        (aggression : _uint)
        -> herr_t))

(define-hdf5 H5Pset_fletcher32
  (_fun (plist_id : hid_t)
        -> herr_t))

;; File creation property list (FCPL) routines
;; TODO
(define-hdf5 H5Pget_version
  (_fun (plist_id : hid_t)
        (boot : _pointer) #|out|#
        (freelist : _pointer) #|out|#
        (stab : _pointer) #|out|#
        (shhdr : _pointer) #|out|#
        -> herr_t))

(define-hdf5 H5Pset_userblock
(_fun (plist_id : hid_t)
      (size : hsize_t)
      -> herr_t))

(define-hdf5 H5Pget_userblock
  (_fun (plist_id : hid_t)
        (size : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_sizes
  (_fun (plist_id : hid_t)
        (sizeof_addr : _size)
        (sizeof_size : _size)
        -> herr_t))

(define-hdf5 H5Pget_sizes
  (_fun (plist_id : hid_t)
        (sizeof_addr : _pointer) ;; TODO:out
        (sizeof_size : _pointer) ;; TODO:out
        -> herr_t))

(define-hdf5 H5Pset_sym_k
  (_fun (plist_id : hid_t)
        (ik : _uint)
        (lk : _uint)
        -> herr_t))

(define-hdf5 H5Pget_sym_k
  (_fun (plist_id : hid_t)
        (ik : _pointer) ;; TODO:out
        (lk : _pointer) ;; TODO:out
        -> herr_t))

(define-hdf5 H5Pset_istore_k
  (_fun (plist_id : hid_t)
        (ik : _uint)
        -> herr_t))

(define-hdf5 H5Pget_istore_k
  (_fun (plist_id : hid_t)
        (ik : _uint)
        -> herr_t))

(define-hdf5 H5Pset_shared_mesg_nindexes
  (_fun (plist_id : hid_t)
        (nindexes : _uint)
        -> herr_t))

(define-hdf5 H5Pget_shared_mesg_nindexes
  (_fun (plist_id : hid_t)
        (nindexes : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_shared_mesg_index
  (_fun (plist_id : hid_t)
        (index_num : _uint)
        (mesg_type_flags : _uint)
        (min_mesg_size : _uint)
        -> herr_t))

(define-hdf5 H5Pget_shared_mesg_index
  (_fun (plist_id : hid_t)
        (index_num : _uint)
        (mesg_type_flags : _pointer)
        (min_mesg_size : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_shared_mesg_phase_change
  (_fun (plist_id : hid_t)
        (max_list : _uint)
        (min_btree : _uint)
        -> herr_t))

(define-hdf5 H5Pget_shared_mesg_phase_change
  (_fun (plist_id : hid_t)
        (max_list : _pointer)
        (min_btree : _pointer)
        -> herr_t))

;; File access property list (FAPL) routines
(define-hdf5 H5Pset_alignment
  (_fun (fapl_id : hid_t)
        (threshold : hsize_t)
        (alignment : hsize_t)
        -> herr_t))

(define-hdf5 H5Pget_alignment
  (_fun (fapl_id : hid_t)
        (threshold : _pointer)
        (alignment : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_driver
  (_fun (plist_id : hid_t)
        (driver_id : hid_t)
        (driver_info : _pointer) ;; TODO: const
        -> herr_t))

(define-hdf5 H5Pget_driver
  (_fun (plist_id : hid_t)
        -> hid_t))

(define-hdf5 H5Pget_driver_info
  (_fun (plist_id : hid_t)
        -> _pointer))

(define-hdf5 H5Pset_family_offset
(_fun (fapl_id : hid_t)
      (offset : hsize_t)
      -> herr_t))

(define-hdf5 H5Pget_family_offset
  (_fun (fapl_id : hid_t)
        (offset : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_multi_type
  (_fun (fapl_id : hid_t)
        (type : H5FD_mem_t)
        -> herr_t))

(define-hdf5 H5Pget_multi_type
  (_fun (fapl_id : hid_t)
        (type : H5FD_mem_t)
        -> herr_t))

(define-hdf5 H5Pset_cache
  (_fun (plist_id : hid_t)
        (mdc_nelmts : _int)
        (rdcc_nslots : _size)
        (rdcc_nbytes : _size)
        (rdcc_w0 : _double)
        -> herr_t))

(define-hdf5 H5Pget_cache
  (_fun (plist_id : hid_t)
        (mdc_nelmts : _pointer) ;; TODO out
        (rdcc_nslots : _pointer) #|TODO out|#
        (rdcc_nbytes : _pointer) #|TODO out|#
        (rdcc_w0 : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_mdc_config
  (_fun (plist_id : hid_t)
        (config_ptr : _H5AC_cache_config_t)
        -> herr_t))

(define-hdf5 H5Pget_mdc_config
  (_fun (plist_id : hid_t)
        (config_ptr : _H5AC_cache_config_t)
        -> herr_t))	#| TODO: out |#

(define-hdf5 H5Pset_gc_references
  (_fun (fapl_id : hid_t)
        (gc_ref : _pointer)
        -> herr_t))

(define-hdf5 H5Pget_gc_references
  (_fun (fapl_id : hid_t)
        (gc_ref : _pointer)
        -> herr_t)) ;; TODO out

(define-hdf5 H5Pset_fclose_degree
  (_fun (fapl_id : hid_t)
        (degree : H5F_close_degree_t)
        -> herr_t))

(define-hdf5 H5Pget_fclose_degree
(_fun (fapl_id : hid_t)
      (degree : H5F_close_degree_t)
      -> herr_t))

(define-hdf5 H5Pset_meta_block_size
  (_fun (fapl_id : hid_t)
        (size : hsize_t)
        -> herr_t))

(define-hdf5 H5Pget_meta_block_size
  (_fun (fapl_id : hid_t)
        (size : _pointer) ;; TODO out hsize_t
        -> herr_t))

(define-hdf5 H5Pset_sieve_buf_size
  (_fun (fapl_id : hid_t)
        (size :  _size)
        -> herr_t))

(define-hdf5 H5Pget_sieve_buf_size
  (_fun (fapl_id : hid_t)
        (size : _pointer) #|TODO: out|#
        -> herr_t))

(define-hdf5 H5Pset_small_data_block_size
  (_fun (fapl_id : hid_t)
        (size : hsize_t)
        -> herr_t))

(define-hdf5 H5Pget_small_data_block_size
  (_fun (fapl_id : hid_t)
        (size : _pointer) #| TODO: out hsize_t|#
        -> herr_t))

(define-hdf5 H5Pset_libver_bounds
  (_fun (plist_id : hid_t)
        (low : H5F_libver_t)
        (high : H5F_libver_t)
        -> herr_t))

(define-hdf5 H5Pget_libver_bounds
  (_fun (plist_id : hid_t)
        (low : H5F_libver_t)
        (high : H5F_libver_t)
        -> herr_t))

(define-hdf5 H5Pset_elink_file_cache_size
  (_fun (plist_id : hid_t)
        (efc_size : _uint)
        -> herr_t))

(define-hdf5 H5Pget_elink_file_cache_size
  (_fun (plist_id : hid_t)
        (efc_size : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_file_image
  (_fun (fapl_id : hid_t)
        (buf_ptr : _pointer)
        (buf_len : _size)
        -> herr_t))

(define-hdf5 H5Pget_file_image
(_fun (fapl_id : hid_t)
      (buf_ptr_ptr : _pointer)
      (buf_len_ptr : _pointer)
      -> herr_t))

(define-hdf5 H5Pset_file_image_callbacks
  (_fun (fapl_id : hid_t)
        (callbacks_ptr : _H5FD_file_image_callbacks_t)
        -> herr_t))

(define-hdf5 H5Pget_file_image_callbacks
  (_fun (fapl_id : hid_t)
        (callbacks_ptr : _H5FD_file_image_callbacks_t)
        -> herr_t))

(define-hdf5 H5Pset_core_write_tracking
  (_fun (fapl_id : hid_t)
        (is_enabled : hbool_t)
        (page_size : _size)
        -> herr_t))

(define-hdf5 H5Pget_core_write_tracking
  (_fun (fapl_id : hid_t)
        (is_enabled : _pointer)
        (page_size : _pointer)
        -> herr_t))

;; Dataset creation property list (DCPL) routines
(define-hdf5 H5Pset_layout
  (_fun (plist_id : hid_t)
        (layout : H5D_layout_t)
        -> herr_t))

(define-hdf5 H5Pget_layout
  (_fun (plist_id : hid_t)
        -> H5D_layout_t))

(define-hdf5 H5Pset_chunk
  (_fun (plist_id dim) ::
        (plist_id : hid_t)
        (ndims : _int = (sequence-length dim))
        (dim : (_list i hsize_t) = (seq->list dim))
        -> herr_t))

(define-hdf5 H5Pget_chunk
  (_fun (plist_id : hid_t)
        (max_ndims : _int)
        (dim : _pointer) ;; TODO: hsize_t dim[]
        -> _int))

(define-hdf5 H5Pset_external
  (_fun (plist_id : hid_t)
        (name : _string)
        (offset : off_t)
        (size : hsize_t)
        -> herr_t))

(define-hdf5 H5Pget_external_count
  (_fun (plist_id : hid_t)
        -> _int))

(define-hdf5 H5Pget_external
  (_fun (plist_id : hid_t)
        (idx : _uint)
        (name_size : _size)
        (name : _string) ;; TODO: out
        (offset : _pointer) ;;TODO: out off_t
        (size : _pointer) ;; TODO: out hsize_t
        -> herr_t))

(define-hdf5 H5Pset_szip
  (_fun (plist_id : hid_t)
        (options_mask : _uint)
        (pixels_per_block : _uint)
        -> herr_t))

(define-hdf5 H5Pset_shuffle
  (_fun (plist_id : hid_t)
        -> herr_t))

(define-hdf5 H5Pset_nbit
  (_fun (plist_id : hid_t)
        -> herr_t))

(define-hdf5 H5Pset_scaleoffset
  (_fun (plist_id : hid_t)
        (scale_type : H5Z_SO_scale_type_t)
        (scale_factor : _int)
        -> herr_t))

(define-hdf5 H5Pset_fill_value
  (_fun (plist_id : hid_t)
        (type_id : hid_t)
        (value : _pointer)
        -> herr_t))

(define-hdf5 H5Pget_fill_value
  (_fun (plist_id : hid_t)
        (type_id : hid_t)
        (value : _pointer) #|TODO: out|#
        -> herr_t))

(define-hdf5 H5Pfill_value_defined
  (_fun (plist : hid_t)
        (status : H5D_fill_value_t)
        -> herr_t))

(define-hdf5 H5Pset_alloc_time
  (_fun (plist_id : hid_t)
        (alloc_time : H5D_alloc_time_t)
        -> herr_t))

(define-hdf5 H5Pget_alloc_time
  (_fun (plist_id : hid_t)
        (alloc_time : _pointer) ;; TODO out H5D_alloc_time_t
        -> herr_t))

(define-hdf5 H5Pset_fill_time
  (_fun (plist_id : hid_t)
        (fill_time : H5D_fill_time_t)
        -> herr_t))

(define-hdf5 H5Pget_fill_time
  (_fun (plist_id : hid_t)
        (fill_time : _pointer) ;; TODO out H5D_fill_time_t
        -> herr_t))

#| Dataset access property list (DAPL) routines |#
(define-hdf5 H5Pset_chunk_cache
  (_fun (dapl_id : hid_t)
        (rdcc_nslots : _size)
        (rdcc_nbytes : _size)
        (rdcc_w0 : _double)
        -> herr_t))

(define-hdf5 H5Pget_chunk_cache
  (_fun (dapl_id : hid_t)
        (rdcc_nslots : _pointer) ;; out size_t
        (rdcc_nbytes : _pointer) ;; out size_t
        (rdcc_w0 : _pointer) ;; out double
        -> herr_t))

#| Dataset xfer property list (DXPL) routines |#
(define-hdf5 H5Pset_data_transform
  (_fun (plist_id : hid_t)
        (expression : _string)
        -> herr_t))

(define-hdf5 H5Pget_data_transform
  (_fun (plist_id : hid_t)
        (expression : _string) ;; TODO: out
        (size : _size)
        -> _ssize))

(define-hdf5 H5Pset_buffer
  (_fun (plist_id : hid_t)
        (size : _size)
        (tconv : _pointer)
        (bkg : _pointer)
        -> herr_t))

(define-hdf5 H5Pget_buffer
  (_fun (plist_id : hid_t)
        (tconv : _pointer) ;; TODO: out **void
        (bkg : _pointer) ;; TODO: out **void
        -> _size))

(define-hdf5 H5Pset_preserve
  (_fun (plist_id : hid_t)
        (status : hbool_t)
        -> herr_t))

(define-hdf5 H5Pget_preserve
  (_fun (plist_id : hid_t)
        -> _int))

(define-hdf5 H5Pset_edc_check
  (_fun (plist_id : hid_t)
        (check : H5Z_EDC_t)
        -> herr_t))

(define-hdf5 H5Pget_edc_check
  (_fun (plist_id : hid_t)
        -> H5Z_EDC_t))

(define-hdf5 H5Pset_filter_callback
  (_fun (plist_id : hid_t)
        (func : H5Z_filter_func_t)
        (op_data : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_btree_ratios
  (_fun (plist_id : hid_t)
        (left : _double)
        (middle : _double)
        (right : _double)
        -> herr_t))

(define-hdf5 H5Pget_btree_ratios
  (_fun (plist_id : hid_t)
        (left : _pointer) ;; TODO: out double
        (middle : _pointer) ;; TODO: out double
        (right : _pointer) ;; TODO: out double
        -> herr_t))

(define-hdf5 H5Pset_vlen_mem_manager
  (_fun (plist_id : hid_t)
        (alloc_func : H5MM_allocate_t)
        (alloc_info : _pointer)
        (free_func : H5MM_free_t)
        (free_info : _pointer)
        -> herr_t))

(define-hdf5 H5Pget_vlen_mem_manager
  (_fun (plist_id : hid_t)
        (alloc_func : H5MM_allocate_t)
        (alloc_info : _pointer)
        (free_func : H5MM_free_t)
        (free_info : _pointer)
        -> herr_t))

(define-hdf5 H5Pset_hyper_vector_size
  (_fun (fapl_id : hid_t)
        (size : _size)
        -> herr_t))

(define-hdf5 H5Pget_hyper_vector_size
  (_fun (fapl_id : hid_t)
        (size : _pointer) ;; TODO out _size
        -> herr_t))

(define-hdf5 H5Pset_type_conv_cb
  (_fun (dxpl_id : hid_t)
        (op : H5T_conv_except_func_t)
        (operate_data : _pointer)
        -> herr_t))

(define-hdf5 H5Pget_type_conv_cb
  (_fun (dxpl_id : hid_t)
        (op : H5T_conv_except_func_t)
        (operate_data : _pointer)
        -> herr_t))

 (define-hdf5 H5Pget_mpio_actual_chunk_opt_mode
   (_fun (plist_id : hid_t)
         (actual_chunk_opt_mode : H5D_mpio_actual_chunk_opt_mode_t)
         -> herr_t))

 (define-hdf5 H5Pget_mpio_actual_io_mode
   (_fun (plist_id : hid_t)
         (actual_io_mode : H5D_mpio_actual_io_mode_t)
         -> herr_t))

 (define-hdf5 H5Pget_mpio_no_collective_cause
   (_fun (plist_id : hid_t)
         (local_no_collective_cause : _pointer)
         (global_no_collective_cause : _pointer)
         -> herr_t))

#| Link creation property list (LCPL) routines |#
(define-hdf5 H5Pset_create_intermediate_group
  (_fun (plist_id : hid_t)
        (crt_intmd : _uint)
        -> herr_t))

(define-hdf5 H5Pget_create_intermediate_group
(_fun (plist_id : hid_t)
      (crt_intmd : _pointer) ;; TODO out *unsigned
      -> herr_t))

#| Group creation property list (GCPL) routines |#
(define-hdf5 H5Pset_local_heap_size_hint
  (_fun (plist_id : hid_t)
        (size_hint : _size)
        -> herr_t))

(define-hdf5 H5Pget_local_heap_size_hint
  (_fun (plist_id : hid_t)
        (size_hint : _pointer) ;; TODO out *size
        -> herr_t))

(define-hdf5 H5Pset_link_phase_change
  (_fun (plist_id : hid_t)
        (max_compact : _uint)
        (min_dense : _uint)
        -> herr_t))

(define-hdf5 H5Pget_link_phase_change
  (_fun (plist_id : hid_t)
        (max_compact : _pointer) ;; TODO out *unsigned
        (min_dense : _pointer) ;; TODO out *unsigned
        -> herr_t))

(define-hdf5 H5Pset_est_link_info
  (_fun (plist_id : hid_t)
        (est_num_entries : _uint)
        (est_name_len : _uint)
        -> herr_t))

(define-hdf5 H5Pget_est_link_info
  (_fun (plist_id : hid_t)
        (est_num_entries : _pointer) ;; TODO: out unigned*
        (est_name_len : _pointer)  ;; TODO: out unigned*
        -> herr_t))

(define-hdf5 H5Pset_link_creation_order
  (_fun (plist_id : hid_t)
        (crt_order_flags : _uint)
        -> herr_t))

(define-hdf5 H5Pget_link_creation_order
  (_fun (plist_id : hid_t)
        (crt_order_flags : _pointer) ;; TODO: out unigned*
        -> herr_t))

#| String creation property list (STRCPL) routines |#
(define-hdf5 H5Pset_char_encoding
  (_fun (plist_id : hid_t)
        (encoding : H5T_cset_t)
        -> herr_t))

(define-hdf5 H5Pget_char_encoding
(_fun (plist_id : hid_t)
      (encoding : H5T_cset_t) ;; TODO out
      -> herr_t))

#| Link access property list (LAPL) routines |#
(define-hdf5 H5Pset_nlinks
  (_fun (plist_id : hid_t)
        (nlinks : _size)
        -> herr_t))

(define-hdf5 H5Pget_nlinks
  (_fun (plist_id : hid_t)
        (nlinks : _pointer) ;; TODO out *size
        -> herr_t))

(define-hdf5 H5Pset_elink_prefix
  (_fun (plist_id : hid_t)
        (prefix : _string)
        -> herr_t))

(define-hdf5 H5Pget_elink_prefix
  (_fun (plist_id : hid_t)
        (prefix : _string)
        (size : _size)
        -> _ssize))

(define-hdf5 H5Pget_elink_fapl
  (_fun (lapl_id : hid_t)
        -> hid_t))

(define-hdf5 H5Pset_elink_fapl
  (_fun (lapl_id : hid_t)
        (fapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Pset_elink_acc_flags
  (_fun (lapl_id : hid_t)
        (flags : _uint)
        -> herr_t))

(define-hdf5 H5Pget_elink_acc_flags
  (_fun (lapl_id : hid_t)
        (flags : _pointer) ;; TODO out *uint
        -> herr_t))

(define-hdf5 H5Pset_elink_cb
  (_fun (lapl_id : hid_t)
        (func : H5L_elink_traverse_t)
        (op_data : _pointer)
        -> herr_t))

(define-hdf5 H5Pget_elink_cb
  (_fun (lapl_id : hid_t)
        (func : H5L_elink_traverse_t)
        (op_data : _pointer)
        -> herr_t))

#| Object copy property list (OCPYPL) routines |#
(define-hdf5 H5Pset_copy_object
  (_fun (plist_id : hid_t)
        (crt_intmd : _uint)
        -> herr_t))

(define-hdf5 H5Pget_copy_object
  (_fun (plist_id : hid_t)
        (crt_intmd : _pointer) ;; TODO out *uint
        -> herr_t))

(define-hdf5 H5Padd_merge_committed_dtype_path
  (_fun (plist_id : hid_t)
        (path : _string)
        -> herr_t))

(define-hdf5 H5Pfree_merge_committed_dtype_paths
  (_fun (plist_id : hid_t)
        -> herr_t))

(define-hdf5 H5Pset_mcdt_search_cb
  (_fun (plist_id : hid_t)
        (func : H5O_mcdt_search_cb_t)
        (op_data : _pointer)
        -> herr_t))

(define-hdf5 H5Pget_mcdt_search_cb
  (_fun (plist_id : hid_t)
        (func : H5O_mcdt_search_cb_t)
        (op_data : _pointer)
        -> herr_t))

#| Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 |#
;; #ifndef H5_NO_DEPRECATED_SYMBOLS

#| Macros |#

#| We renamed the "root" of the property list class hierarchy |#
(define H5P_NO_CLASS            H5P_ROOT)


#| Typedefs |#


#| Function prototypes |#
(define-hdf5 H5Pregister1
  (_fun (cls_id : hid_t)
        (name : _string)
        (size : _size)
        (def_value : _pointer)
        (prp_create : H5P_prp_create_func_t)
        (prp_set : H5P_prp_set_func_t)
        (prp_get : H5P_prp_get_func_t)
        (prp_del : H5P_prp_delete_func_t)
        (prp_copy : H5P_prp_copy_func_t)
        (prp_close : H5P_prp_close_func_t)
        -> herr_t))

(define-hdf5 H5Pinsert1
  (_fun (plist_id : hid_t)
        (name : _string)
        (size : _size)
        (value : _pointer)
        (prp_set : H5P_prp_set_func_t)
        (prp_get : H5P_prp_get_func_t)
        (prp_delete : H5P_prp_delete_func_t)
        (prp_copy : H5P_prp_copy_func_t)
        (prp_close : H5P_prp_close_func_t)
        -> herr_t))

(define-hdf5 H5Pget_filter1
  (_fun (plist_id : hid_t)
        (filter : _uint)
        (flags : _pointer) ;; TODO out *unsigned
        (cd_nelmts : _size) ;; TODO out *size_t
        (cd_values : _pointer) ;; TODO out *unsigned
        (namelen : _size)
        (name : _string) ;; TODO out
        -> H5Z_filter_t))

(define-hdf5 H5Pget_filter_by_id1
  (_fun (plist_id : hid_t)
        (id : H5Z_filter_t)
        (flags : _pointer)  ;; TODO out *unsigned
        (cd_nelmts : _pointer)  ;; TODO out *size
        (cd_values : _pointer) ;; TODO out *unsigned
        (namelen : _size)
        (name : _string) ;; TODO out string
        -> herr_t))
