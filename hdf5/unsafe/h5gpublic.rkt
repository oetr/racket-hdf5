#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5lpublic.rkt"
         "h5opublic.rkt"
         "h5tpublic.rkt")

(provide (all-defined-out))


;; Types of link storage for groups
(define H5G_storage_type_t
  (_enum '(
    H5G_STORAGE_TYPE_UNKNOWN = -1	;; Unknown link storage type	
    H5G_STORAGE_TYPE_SYMBOL_TABLE
    ;; Links in group are stored with a "symbol table"
    ;; (this is sometimes called "old-style" groups) 
    H5G_STORAGE_TYPE_COMPACT ;; Links are stored in object header 
    H5G_STORAGE_TYPE_DENSE
    ;; Links are stored in fractal heap & indexed with v2 B-tree 
)))

;; Information struct for group (for H5Gget_info/H5Gget_info_by_name/H5Gget_info_by_idx)
(define-cstruct _H5G_info_t 
  ([storage_type H5G_storage_type_t] ;; Type of storage for links in group
   [nlinks hsize_t]    ;; Number of links in group
   [max_corder _int64] ;; Current max. creation order value for group
   [mounted hbool_t])) ;; Whether group has a file mounted on it

#|******************|#
#| Public Variables |#
#|******************|#


#|*******************|#
#| Public Prototypes |#
#|*******************|#
(define-hdf5 H5Gcreate2
  (_fun (loc_id : hid_t)
        (name : _string)
        (lcpl_id : hid_t)
        (gcpl_id : hid_t)
        (gapl_id : hid_t)
        -> hid_t))

(define H5Gcreate H5Gcreate2)

(define-hdf5 H5Gcreate_anon
  (_fun (loc_id : hid_t)
        (gcpl_id : hid_t)
        (gapl_id : hid_t)
        -> hid_t))

(define-hdf5 H5Gopen2
  (_fun (loc_id : hid_t)
        (name : _string)
        (gapl_id : hid_t)
        -> hid_t))

(define-hdf5 H5Gget_create_plist
  (_fun (group_id : hid_t)
        -> hid_t))

(define-hdf5 H5Gget_info
  (_fun (loc_id) ::
        (loc_id : hid_t)
        (ginfo : (_ptr o _H5G_info_t))
        -> (status : herr_t)
        -> (if (< status 0)
               (error 'H5Gget_info "Unable to get info.")
               ginfo)))

(define-hdf5 H5Gget_info_by_name
  (_fun (loc_id : hid_t)
        (name : _string)
        (ginfo : _H5G_info_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Gget_info_by_idx
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (n : hsize_t)
        (ginfo : _H5G_info_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Gclose
  (_fun (group_id : hid_t)
        -> (status : herr_t)
        -> (when (< status 0)
             (error 'H5Gclose "Unable to close the group."))))

#| Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 |#
;; #ifndef H5_NO_DEPRECATED_SYMBOLS

#| Macros |#

#| Link definitions |#
(define H5G_SAME_LOC H5L_SAME_LOC)
(define H5G_LINK_ERROR 'H5L_TYPE_ERROR)
(define H5G_LINK_HARD 'H5L_TYPE_HARD)
(define H5G_LINK_SOFT 'H5L_TYPE_SOFT)
(define H5G_link_t H5L_type_t)

#| Macros for types of objects in a group (see H5G_obj_t definition) |#
(define H5G_NTYPES	256)		#| Max possible number of types	|#
(define H5G_NLIBTYPES	8)		#| Number of internal types	|#
(define H5G_NUSERTYPES	(- H5G_NTYPES H5G_NLIBTYPES))
(define (H5G_USERTYPE X) #| User defined types |#
  (+ 8 + X))


#| Typedefs |#

#|
 * An object has a certain type. The first few numbers are reserved for use
 * internally by HDF5. Users may add their own types with higher values.  The
 * values are never stored in the file -- they only exist while an
 * application is running.  An object may satisfy the `isa' function for more
 * than one type.
|#
(define H5G_obj_t
  (_enum '(
    H5G_UNKNOWN = -1            #| Unknown object type		|#
    H5G_GROUP                   #| Object is a group		|#
    H5G_DATASET                 #| Object is a dataset		|#
    H5G_TYPE                    #| Object is a named data type	|#
    H5G_LINK		        #| Object is a symbolic link	|#
    H5G_UDLINK		        #| Object is a user-defined link |#
    H5G_RESERVED_5              #| Reserved for future use	|#
    H5G_RESERVED_6              #| Reserved for future use	|#
    H5G_RESERVED_7              #| Reserved for future use	|#
)))

#| Prototype for H5Giterate() operator |#
(define H5G_iterate_t
  (_fun (group : hid_t)
        (name : _string)
        (op_data : _pointer)
        -> herr_t))

#| Information about an object |#
(define-cstruct _H5G_stat_t
  ([fileno (_list io _ulong 2)] #|file number			|#
   [objno (_list io _ulong 2)]  #|object number			|#
   [nlink _uint]                #|number of hard links to object|#
   [type H5G_obj_t]             #|basic object type		|#
   [mtime _time]                #|modification time		|#
   [linklen _size]              #|symbolic link value length	|#
   [ohdr _H5O_stat_t]))         #| Object header information    |#


#| Function prototypes |#
(define-hdf5 H5Gcreate1
  (_fun (loc_id : hid_t)
        (name : _string)
        (size_hint : _size)
        -> hid_t))

(define-hdf5 H5Gopen1
  (_fun (loc_id : hid_t)
        (name : _string)
        -> hid_t))

(define-hdf5 H5Glink
  (_fun (cur_loc_id : hid_t)
        (type : H5G_link_t)
        (cur_name : _string)
        (new_name : _string)
        -> herr_t))

(define-hdf5 H5Glink2
  (_fun (cur_loc_id : hid_t)
        (cur_name : _string)
        (type : H5G_link_t)
        (new_loc_id : hid_t)
        (new_name : _string)
        -> herr_t))

(define-hdf5 H5Gmove
  (_fun (src_loc_id : hid_t)
        (src_name : _string)
        (dst_name : _string)
        -> herr_t))

(define-hdf5 H5Gmove2
  (_fun (src_loc_id : hid_t)
        (src_name : _string)
        (dst_loc_id : hid_t)
        (dst_name : _string)
        -> herr_t))

(define-hdf5 H5Gunlink
  (_fun (loc_id : hid_t)
        (name : _string)
        -> herr_t))

(define-hdf5 H5Gget_linkval
  (_fun (loc_id : hid_t)
        (name : _string)
        (size : _size)
        (buf : _pointer) ;; TODO out *char
        -> herr_t))

(define-hdf5 H5Gset_comment
  (_fun (loc_id : hid_t)
        (name : _string)
        (comment : _string)
        -> herr_t))

(define-hdf5 H5Gget_comment
  (_fun (loc_id : hid_t)
        (name : _string)
        (bufsize : _size)
        (buf : _pointer) ;; TODO pointer char
        -> _int))

(define-hdf5 H5Giterate
  (_fun (loc_id : hid_t)
        (name : _string)
        (idx : _pointer) ;; TODO *int
        (op : H5G_iterate_t)
        (op_data : _pointer)
        -> herr_t))

(define-hdf5 H5Gget_num_objs
  (_fun (loc_id) ::
        (loc_id : hid_t)
        (num_objs : (_ptr o hsize_t)) ;; TODO *hsize_t
        -> (status : herr_t)
        -> (if (< status 0)
               (error 'H5Gget_num_objs "Unable to enumerate objects.")
               num_objs)))

(define-hdf5 H5Gget_objinfo
  (_fun (loc_id name follow_link statbuf) ::
        (loc_id : hid_t)
        (name : _string)
        (follow_link : hbool_t = (if follow_link 1 0))
        (statbuf : _H5G_stat_t-pointer/null)
        -> herr_t))

(define-hdf5 H5Gget_objname_by_idx
  (_fun (loc_id : hid_t)
        (idx : hsize_t)
        (name : _pointer) ;; TODO out? *char
        (size : _size)
        -> _ssize))

(define-hdf5 H5Gget_objtype_by_idx
  (_fun (loc_id : hid_t)
        (idx : hsize_t)
        -> H5G_obj_t))

;; #endif #| H5_NO_DEPRECATED_SYMBOLS |#
