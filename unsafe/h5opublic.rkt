#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"   ;; generic functions
         "h5ipublic.rkt"  ;; ids
         "h5lpublic.rkt") ;; links

(provide (all-defined-out))

;;****************/
;; Public Macros */
;;****************/
;; Flags for object copy (H5Ocopy)
(define H5O_COPY_SHALLOW_HIERARCHY_FLAG #x0001)   ;; Copy only immediate members */
(define H5O_COPY_EXPAND_SOFT_LINK_FLAG  #x0002)   ;; Expand soft links into new objects */
(define H5O_COPY_EXPAND_EXT_LINK_FLAG   #x0004)   ;; Expand external links into new objects */
(define H5O_COPY_EXPAND_REFERENCE_FLAG	#x0008)   ;; Copy objects that are pointed by references */
(define H5O_COPY_WITHOUT_ATTR_FLAG      #x0010)   ;; Copy object without copying attributes */
(define H5O_COPY_PRESERVE_NULL_FLAG     #x0020)   ;; Copy NULL messages (empty space) */
(define H5O_COPY_MERGE_COMMITTED_DTYPE_FLAG #x0040)   ;; Merge committed datatypes in dest file */
(define H5O_COPY_ALL                    #x007F)   ;; All object copying flags (for internal checking) */

#| Flags for shared message indexes.
* Pass these flags in using the mesg_type_flags parameter in
* H5P_set_shared_mesg_index.
* (Developers: These flags correspond to object header message type IDs,
* but we need to assign each kind of message to a different bit so that
* one index can hold multiple types.)
|#
(define H5O_SHMESG_NONE_FLAG    #x0000)          ;; No shared messages */
(define H5O_SHMESG_SDSPACE_FLAG (arithmetic-shift 1 1)) ;; Simple Dataspace Message.  */
(define H5O_SHMESG_DTYPE_FLAG   (arithmetic-shift 1 #x0003)) ;; Datatype Message.  */
(define H5O_SHMESG_FILL_FLAG    (arithmetic-shift 1 #x0005)) ;; Fill Value Message. */
(define H5O_SHMESG_PLINE_FLAG   (arithmetic-shift 1 #x000b)) ;; Filter pipeline message.  */
(define H5O_SHMESG_ATTR_FLAG    (arithmetic-shift 1 #x000c)) ;; Attribute Message.  */
(define H5O_SHMESG_ALL_FLAG     (bitwise-ior H5O_SHMESG_SDSPACE_FLAG  H5O_SHMESG_DTYPE_FLAG  H5O_SHMESG_FILL_FLAG  H5O_SHMESG_PLINE_FLAG  H5O_SHMESG_ATTR_FLAG))

;; Object header status flag definitions */
(define H5O_HDR_CHUNK0_SIZE             #x03)    ;; 2-bit field indicating # of bytes to store the size of chunk 0's data */
(define H5O_HDR_ATTR_CRT_ORDER_TRACKED  #x04)    ;; Attribute creation order is tracked */
(define H5O_HDR_ATTR_CRT_ORDER_INDEXED  #x08)    ;; Attribute creation order has index */
(define H5O_HDR_ATTR_STORE_PHASE_CHANGE #x10)    ;; Non-default attribute storage phase change values stored */
(define H5O_HDR_STORE_TIMES             #x20)    ;; Store access, modification, change & birth times for object */
(define H5O_HDR_ALL_FLAGS       (bitwise-ior H5O_HDR_CHUNK0_SIZE  H5O_HDR_ATTR_CRT_ORDER_TRACKED  H5O_HDR_ATTR_CRT_ORDER_INDEXED  H5O_HDR_ATTR_STORE_PHASE_CHANGE  H5O_HDR_STORE_TIMES))

#| Maximum shared message values.  Number of indexes is 8 to allow room to add
* new types of messages.
|#
(define H5O_SHMESG_MAX_NINDEXES 8)
(define H5O_SHMESG_MAX_LIST_SIZE 5000)


;;******************/
;; Public Typedefs */
;;******************/

;; Types of objects in file */
(define H5O_type_t
  (_enum '(
           H5O_TYPE_UNKNOWN = -1	;; Unknown object type		*/
           H5O_TYPE_GROUP	        ;; Object is a group		*/
           H5O_TYPE_DATASET		;; Object is a dataset		*/
           H5O_TYPE_NAMED_DATATYPE 	;; Object is a named data type	*/
           H5O_TYPE_NTYPES             ;; Number of different object types (must be last!) */
           )))
;; Information struct for object header metadata (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx) */
(define-cstruct _H5O_hdr_info_t
  ([version _uint]		;; Version number of header format in file */
   [nmesgs _uint]		;; Number of object header messages */
   [nchunks _uint]		;; Number of object header chunks */
   [flags _uint]             ;; Object header status flags */
   [space (_list-struct 
           hsize_t ;; total;		;; Total space for storing object header in file */
           hsize_t ;; meta;		;; Space within header for object header metadata information */
           hsize_t ;; mesg;		;; Space within header for actual message information */
           hsize_t ;;free;		;; Free space within object header */
           )]
   [mesg (_list-struct
          _uint64 ;; present;	;; Flags to indicate presence of message type in header */
          _uint64  ;; shared;	;; Flags to indicate message type is shared in header */
          )]))

(define _time _ulong)

;; Information struct for object (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx) */
(define (allocate-H5O_info_t (mode 'atomic-interior))
  (ptr-ref (malloc _H5O_info_t mode) _H5O_info_t))

(define-cstruct _H5O_info_t
  ([fileno _ulong]	;; File number that object is located in
   [addr haddr_t]	;; Object address in file
   [type H5O_type_t] 	;; Basic object type (group, dataset, etc.)
   [rc _uint]		;; Reference count of object
   [atime _time]	;; Access time
   [mtime _time]	;; Modification time
   [ctime _time]	;; Change time
   [btime _time]	;; Birth time
   [num_attrs hsize_t]  ;; # of attributes attached to object
   [hdr _H5O_hdr_info_t];; Object header information
   ;; Extra metadata storage for obj & attributes
   [meta_size (_list-struct 
               _H5_ih_info_t ;;   obj;             ;; v1/v2 B-tree & local/fractal heap for groups, B-tree for chunked datasets */
               _H5_ih_info_t ;;  attr;            ;; v2 B-tree & heap for attributes */
               )]))


;; Typedef for message creation indexes */
(define H5O_msg_crt_idx_t _uint32)

;; Prototype for H5Ovisit/H5Ovisit_by_name() operator */
(define H5O_iterate_t
  (_fun (obj : hid_t)
        (name : _string)
        (info : _H5O_info_t-pointer)
        (op_data : _pointer)
        -> _void
        -> 0))

(define H5O_mcdt_search_ret_t
  (_enum
   '(
     H5O_MCDT_SEARCH_ERROR = -1	;; Abort H5Ocopy */
     H5O_MCDT_SEARCH_CONT	;; Continue the global search of all committed datatypes in the destination file */
     H5O_MCDT_SEARCH_STOP	;; Stop the search, but continue copying.  The committed datatype will be copied but not merged. */
     )))

;; Callback to invoke when completing the search for a matching committed datatype from the committed dtype list */
(define H5O_mcdt_search_cb_t
  (_fun (op_data : _pointer) -> H5O_mcdt_search_ret_t))


;;*******************
;; Public Prototypes
;;*******************
(define-hdf5 H5Oopen
  (_fun (loc_id : hid_t)
        (name : _string)
        (lapl_id : hid_t)
        -> hid_t))

(define-hdf5 H5Oopen_by_addr
  (_fun (loc_id : hid_t)
        (addr : haddr_t)
        -> hid_t))

(define-hdf5 H5Oopen_by_idx
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (n : hsize_t)
        (lapl_id : hid_t)
        -> hid_t))

(define-hdf5 H5Oexists_by_name
  (_fun (loc_id : hid_t)
        (name : _string)
        (lapl_id : hid_t)
        -> htri_t))

(define-hdf5 H5Oget_info
  (_fun (loc_id (oinfo-in #f)) ::
        (loc_id : hid_t)
        (oinfo : _H5O_info_t-pointer/null =
               (if oinfo-in
                   oinfo-in
                   (ptr-ref (malloc _H5O_info_t 'atomic-interior) _H5O_info_t)))
        -> (status : herr_t)
        -> (if (< status 0)
               (error 'H5Oget_info "Unable to get object info.")
               oinfo)))

;; broken interface because of default characters
(define-hdf5 H5Oget_info_by_name
  (_fun (loc_id name lapl_id (oinfo-in #f)) ::
        (loc_id : hid_t)
        (name : _string)
        (oinfo : _H5O_info_t-pointer/null =
               (if oinfo-in
                   oinfo-in
                   (ptr-ref (malloc _H5O_info_t 'atomic-interior) _H5O_info_t)))
        (lapl_id : hid_t)
        -> (status : herr_t)
        -> (list status oinfo)))

(define-hdf5 H5Oget_info_by_idx
  (_fun (loc_id : hid_t)
        (group_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (n : hsize_t)
        (oinfo : _H5O_info_t-pointer/null)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Olink
  (_fun (obj_id : hid_t)
        (new_loc_id : hid_t)
        (new_name : _string)
        (lcpl_id : hid_t)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Oincr_refcount
  (_fun (object_id : hid_t)
        -> herr_t))

(define-hdf5 H5Odecr_refcount
  (_fun (object_id : hid_t)
        -> herr_t))

(define-hdf5 H5Ocopy
  (_fun (src_loc_id : hid_t)
        (src_name : _string)
        (dst_loc_id : hid_t)
        (dst_name : _string)
        (ocpypl_id : hid_t)
        (lcpl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Oset_comment
  (_fun (obj_id : hid_t)
        (comment : _string)
        -> herr_t))

(define-hdf5 H5Oset_comment_by_name
  (_fun (loc_id : hid_t)
        (name : _string)
        (comment : _string)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Oget_comment
  (_fun (obj_id : hid_t)
        (comment : _pointer) ;; TODO: out string
        (bufsize : _size)
        -> _ssize))

(define-hdf5 H5Oget_comment_by_name
  (_fun (loc_id : hid_t)
        (name : _string)
        (comment : _pointer) ;; TODO out string
        (bufsize : _size)
        (lapl_id : hid_t)
        -> _ssize))

(define-hdf5 H5Ovisit
  (_fun (obj_id : hid_t)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (op : H5O_iterate_t)
        (op_data : _pointer)
        -> (status : herr_t)
        -> (if (< status 0)
               (error 'H5Ovisit "Unable to visit object.\n")
               (void))))

(define-hdf5 H5Ovisit_by_name
  (_fun (loc_id : hid_t)
        (obj_name : _string)
        (idx_type : H5_index_t)
        (order : H5_iter_order_t)
        (op : H5O_iterate_t)
        (op_data : _pointer)
        (lapl_id : hid_t)
        -> herr_t))

(define-hdf5 H5Oclose
  (_fun (object_id : hid_t)
        -> herr_t))

#| Symbols defined for compatibility with previous versions of the HDF5 API.
*
* Use of these symbols is deprecated.
|#
;; #ifndef H5_NO_DEPRECATED_SYMBOLS

;; Macros

;; Typedefs

;; A struct that's part of the H5G_stat_t routine (deprecated)
(define-cstruct _H5O_stat_t
  ([size hsize_t]    ;; Total size of object header in file
   [free hsize_t]    ;; Free space within object header
   [nmesgs _uint]    ;; Number of object header messages
   [nchunks _uint])) ;; Number of object header chunks
