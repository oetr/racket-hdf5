#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5fpublic.rkt") ;; for H5F_close_degree_t

(provide (all-defined-out))

(define H5_HAVE_VFL 1) #|define a convenient app feature test|#
(define H5FD_VFD_DEFAULT 0)   #| Default VFL driver value |#

#| Types of allocation requests: see H5Fpublic.h  |#
(define H5FD_mem_t H5F_mem_t)

#| Map "fractal heap" header blocks to 'ohdr' type file memory, since its
* a fair amount of work to add a new kind of file memory and they are similar
* enough to object headers and probably too minor to deserve their own type.
*
* Map "fractal heap" indirect blocks to 'ohdr' type file memory, since they
* are similar to fractal heap header blocks.
*
* Map "fractal heap" direct blocks to 'lheap' type file memory, since they
* will be replacing local heaps.
*
* Map "fractal heap" 'huge' objects to 'draw' type file memory, since they
* represent large objects that are directly stored in the file.
*
*      -QAK
|#
(define H5FD_MEM_FHEAP_HDR      'H5FD_MEM_OHDR)
(define H5FD_MEM_FHEAP_IBLOCK   'H5FD_MEM_OHDR)
(define H5FD_MEM_FHEAP_DBLOCK   'H5FD_MEM_LHEAP)
(define H5FD_MEM_FHEAP_HUGE_OBJ 'H5FD_MEM_DRAW)

#| Map "free space" header blocks to 'ohdr' type file memory, since its
* a fair amount of work to add a new kind of file memory and they are similar
* enough to object headers and probably too minor to deserve their own type.
*
* Map "free space" serialized sections to 'lheap' type file memory, since they
* are similar enough to local heap info.
*
*      -QAK
|#
(define H5FD_MEM_FSPACE_HDR     'H5FD_MEM_OHDR)
(define H5FD_MEM_FSPACE_SINFO   'H5FD_MEM_LHEAP)

#| Map "shared object header message" master table to 'ohdr' type file memory,
* since its a fair amount of work to add a new kind of file memory and they are
* similar enough to object headers and probably too minor to deserve their own
* type.
*
* Map "shared object header message" indices to 'btree' type file memory,
* since they are similar enough to B-tree nodes.
*
*      -QAK
|#
(define H5FD_MEM_SOHM_TABLE     'H5FD_MEM_OHDR)
(define H5FD_MEM_SOHM_INDEX     'H5FD_MEM_BTREE)


#|
* A free-list map which maps all types of allocation requests to a single
* free list.  This is useful for drivers that don't really care about
* keeping different requests segregated in the underlying file and which
* want to make most efficient reuse of freed memory.  The use of the
* H5FD_MEM_SUPER free list is arbitrary.
|#
(define H5FD_FLMAP_SINGLE
  (vector
   'H5FD_MEM_SUPER			#|default|#			      
   'H5FD_MEM_SUPER			#|super|#			      
   'H5FD_MEM_SUPER			#|btree|#			      
   'H5FD_MEM_SUPER			#|draw|#			      
   'H5FD_MEM_SUPER			#|gheap|#			      
   'H5FD_MEM_SUPER			#|lheap|#			      
   'H5FD_MEM_SUPER))			#|ohdr|#			      


#|
* A free-list map which segregates requests into `raw' or `meta' data
* pools.
|#
(define H5FD_FLMAP_DICHOTOMY
  (vector
   'H5FD_MEM_SUPER  #|default|#
   'H5FD_MEM_SUPER  #|super|#
   'H5FD_MEM_SUPER  #|btree|#
   'H5FD_MEM_DRAW   #|draw|#
   'H5FD_MEM_DRAW   #|gheap|#
   'H5FD_MEM_SUPER  #|lheap|#
   'H5FD_MEM_SUPER)) #|ohdr|#

#|
* The default free list map which causes each request type to use it's own
* free-list.
|#
(define H5FD_FLMAP_DEFAULT
  (vector
   'H5FD_MEM_DEFAULT			#|default|#			      
   'H5FD_MEM_DEFAULT			#|super|#			      
   'H5FD_MEM_DEFAULT			#|btree|#			      
   'H5FD_MEM_DEFAULT			#|draw|#			      
   'H5FD_MEM_DEFAULT			#|gheap|#			      
   'H5FD_MEM_DEFAULT			#|lheap|#			      
   'H5FD_MEM_DEFAULT))			#|ohdr|#			      



#| Define VFL driver features that can be enabled on a per-driver basis |#
#| These are returned with the 'query' function pointer in H5FD_class_t |#
#|
* Defining the H5FD_FEAT_AGGREGATE_METADATA for a VFL driver means that
* the library will attempt to allocate a larger block for metadata and
* then sub-allocate each metadata request from that larger block.
|#
(define H5FD_FEAT_AGGREGATE_METADATA    #x00000001)
#|
* Defining the H5FD_FEAT_ACCUMULATE_METADATA for a VFL driver means that
* the library will attempt to cache metadata as it is written to the file
* and build up a larger block of metadata to eventually pass to the VFL
* 'write' routine.
*
* Distinguish between updating the metadata accumulator on writes and
* reads.  This is particularly (perhaps only even) important for MPI-I/O
* where we guarantee that writes are collective but reads may not be.
* If we were to allow the metadata accumulator to be written during a
* read operation the application would hang.
|#
(define H5FD_FEAT_ACCUMULATE_METADATA_WRITE     #x00000002)
(define H5FD_FEAT_ACCUMULATE_METADATA_READ      #x00000004)
(define H5FD_FEAT_ACCUMULATE_METADATA
  (bitwise-ior H5FD_FEAT_ACCUMULATE_METADATA_WRITE
               H5FD_FEAT_ACCUMULATE_METADATA_READ))
#|
* Defining the H5FD_FEAT_DATA_SIEVE for a VFL driver means that
* the library will attempt to cache raw data as it is read from/written to
* a file in a "data seive" buffer.  See Rajeev Thakur's papers:
*  http://www.mcs.anl.gov/~thakur/papers/romio-coll.ps.gz
*  http://www.mcs.anl.gov/~thakur/papers/mpio-high-perf.ps.gz
|#
(define H5FD_FEAT_DATA_SIEVE            #x00000008)
#|
* Defining the H5FD_FEAT_AGGREGATE_SMALLDATA for a VFL driver means that
* the library will attempt to allocate a larger block for "small" raw data
* and then sub-allocate "small" raw data requests from that larger block.
|#
(define H5FD_FEAT_AGGREGATE_SMALLDATA   #x00000010)
#|
* Defining the H5FD_FEAT_IGNORE_DRVRINFO for a VFL driver means that
* the library will ignore the driver info that is encoded in the file
* for the VFL driver.  (This will cause the driver info to be eliminated
* from the file when it is flushed/closed if the file is opened R/W).
|#
(define H5FD_FEAT_IGNORE_DRVRINFO       #x00000020)
#|
* Defining the H5FD_FEAT_DIRTY_SBLK_LOAD for a VFL driver means that
* the library will mark the superblock dirty when the file is opened
* R/W.  This will cause the driver info to be re-encoded when the file
* is flushed/closed.
|#
(define H5FD_FEAT_DIRTY_SBLK_LOAD       #x00000040)
#|
* Defining the H5FD_FEAT_POSIX_COMPAT_HANDLE for a VFL driver means that
* the handle for the VFD (returned with the 'get_handle' callback) is
* of type 'int' and is compatible with POSIX I/O calls.
|#
(define H5FD_FEAT_POSIX_COMPAT_HANDLE   #x00000080)
#|
* Defining the H5FD_FEAT_HAS_MPI for a VFL driver means that
* the driver makes use of MPI communication and code may retrieve
* communicator/rank information from it
|#
(define H5FD_FEAT_HAS_MPI               #x00000100)
#|
* Defining the H5FD_FEAT_ALLOCATE_EARLY for a VFL driver will force
* the library to use the H5D_ALLOC_TIME_EARLY on dataset create
* instead of the default H5D_ALLOC_TIME_LATE
|#
(define H5FD_FEAT_ALLOCATE_EARLY        #x00000200)
#| 
* Defining the H5FD_FEAT_ALLOW_FILE_IMAGE for a VFL driver means that
* the driver is able to use a file image in the fapl as the initial
* contents of a file.
|#
(define H5FD_FEAT_ALLOW_FILE_IMAGE      #x00000400)
#|
* Defining the H5FD_FEAT_CAN_USE_FILE_IMAGE_CALLBACKS for a VFL driver
* means that the driver is able to use callbacks to make a copy of the
* image to store in memory.
|#
(define H5FD_FEAT_CAN_USE_FILE_IMAGE_CALLBACKS #x00000800)

#| Forward declaration |#
;;(define-cpointer-type _H5FD_t)

#|
 * The main datatype for each driver. Public fields common to all drivers
 * are declared here and the driver appends private fields in memory.
|#
(define-cstruct _H5FD_t
  ([driver_id hid_t]      #|driver ID for this file   |#
   [cls (_cpointer 'H5FD_class_t)] #|constant class info       |#
   ;;[cls _pointer] #|constant class info       |#
   [fileno _ulong]         #| File 'serial' number     |#
   [feature_flags _ulong]  #| VFL Driver feature Flags |#
   [maxaddr haddr_t]        #| For this file, overrides class |#
   [base_addr haddr_t]      #| Base address for HDF5 data w/in file |#
   #| Space allocation management fields |#
   [threshold hsize_t]      #| Threshold for alignment  |#
   [alignment hsize_t]))      #| Allocation alignment     |#


;; Class information for each file driver
(define-cstruct _H5FD_class_t
  ([name _string]
   (maxaddr haddr_t)
   (fc_degree H5F_close_degree_t)
   [sb_size (_fun  (file : _H5FD_t-pointer/null)
                   -> hsize_t)]
   [sb_encode (_fun (file : _H5FD_t-pointer/null)
                    (name : _string) ;; TODO: out
                    (pb : _string) ;; TODO: out
                    -> herr_t)]
   [sb_decode (_fun (f : _H5FD_t-pointer/null)
                    (name :  _string)
                    (p : _string)
                    -> herr_t)]
   [fapl_size _size]
   [fapl_get (_fun (file : _H5FD_t-pointer/null) -> _pointer)]
   [fapl_copy (_fun (fapl : _pointer) -> _pointer)]
   [fapl_free (_fun (fapl : _pointer) -> herr_t)]
   [dxpl_size _size]
   [dxpl_copy (_fun (dxpl : _pointer) -> _pointer)]
   [dxpl_free (_fun (dxpl : _pointer) -> herr_t)]
   [open (_fun (name : _string)
               (flags : _uint)
               (fapl : hid_t)
               (maxaddr : haddr_t) -> _H5FD_t-pointer/null)]
   [close (_fun (file : _H5FD_t-pointer/null) -> herr_t)]
   [cmp (_fun (f1 : _H5FD_t-pointer/null)
              (f2 : _H5FD_t-pointer/null) -> _int)]
   [query (_fun (f1 : _H5FD_t-pointer/null)
                (flags : _pointer) -> herr_t)]
   [get_type_map (_fun (file : _H5FD_t-pointer/null)
                       (type_map : H5FD_mem_t) -> herr_t)]
   [alloc (_fun (file : _H5FD_t-pointer/null)
                (type : H5FD_mem_t)
                (dxpl_id : hid_t)
                (size : hsize_t) -> haddr_t)]
   [free (_fun (file : _H5FD_t-pointer/null)
               (type : H5FD_mem_t)
               (dxpl_id : hid_t)
               (addr : haddr_t)
               (size : hsize_t) -> herr_t)]
   [get_eoa (_fun (file : _H5FD_t-pointer/null)
                  (type : H5FD_mem_t) -> haddr_t)]
   [set_eoa (_fun (file : _H5FD_t-pointer/null)
                  (type : H5FD_mem_t)
                  (addr : haddr_t) -> herr_t)]
   [get_eof (_fun (file : _H5FD_t-pointer/null) -> haddr_t)]
   [get_handle (_fun (file : _H5FD_t-pointer/null)
                     (fapl : hid_t)
                     (file_handle : _pointer) -> herr_t)]
   [read (_fun (file : _H5FD_t-pointer/null)
               (type : H5FD_mem_t)
               (dxpl : hid_t)
               (addr : haddr_t)
               (size : _size)
               (buffer : _pointer) -> herr_t)]
   [write (_fun (file : _H5FD_t-pointer/null)
                (type : H5FD_mem_t)
                (dxpl : hid_t)
                (addr : haddr_t)
                (size : _size)
                (buffer : _pointer) -> herr_t)]
   [flush (_fun (file : _H5FD_t-pointer/null)
                (dxpl_id : hid_t)
                (closing : _uint) -> herr_t)]
   [truncate (_fun (file : _H5FD_t-pointer/null)
                   (dxpl_id : hid_t)
                   (closing : hbool_t) -> herr_t)]
   [lock (_fun (file : _H5FD_t-pointer/null)
               (oid : _string)
               (lock_type : _uint)
               (last : hbool_t) -> herr_t)]
   [unlock (_fun (file : _H5FD_t-pointer/null)
                 (oid : _string)
                 (last : hbool_t) -> herr_t)]
   [fl_map (_array H5FD_mem_t H5FD_MEM_NTYPES)]))

;; A free list is a singly-linked list of address/size pairs.
(define-cstruct _H5FD_free_t
  ([addr haddr_t]
   [size hsize_t]
   [next _H5FD_free_t-pointer/null]))

#| Define enum for the source of file image callbacks |#
(define+provide H5FD_file_image_op_t
  (_enum '(
           H5FD_FILE_IMAGE_OP_NO_OP
           H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET     
           H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY
           H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET
           H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE
           H5FD_FILE_IMAGE_OP_FILE_OPEN
           H5FD_FILE_IMAGE_OP_FILE_RESIZE
           H5FD_FILE_IMAGE_OP_FILE_CLOSE
           )))


#| Define structure to hold file image callbacks |#
(define-cstruct _H5FD_file_image_callbacks_t
  ([image_malloc (_fun (size : _size)
                       (file_image_op : H5FD_file_image_op_t)
                       (udata : _pointer) -> _pointer)]
   [image_memcpy (_fun (dest : _pointer)
                       (src : _pointer)
                       (size : _size)
                       (file_image_op : H5FD_file_image_op_t)
                       (udata : _pointer) -> _pointer)]
   [image_realloc (_fun (ptr : _pointer)
                        (size : _size) 
                        (file_image_op : H5FD_file_image_op_t)
                        (udata : _pointer) -> _pointer)]
   [image_free (_fun (ptr : _pointer)
                     (file_image_op : H5FD_file_image_op_t) 
                     (udata : _pointer) -> herr_t)]
   [udata_copy (_fun (udata : _pointer) -> _pointer)]
   [udata_free (_fun (*udata : void) -> herr_t)]
   [udata _pointer]))


;; Function prototypes 
(define-hdf5 H5FDregister
  (_fun (cls : _H5FD_class_t)
        -> hid_t))

(define-hdf5 H5FDunregister
  (_fun (driver_id : hid_t)
        -> herr_t))

(define-hdf5 H5FDopen
  (_fun (name : _string)
        (flags : _uint)
        (fapl_id : hid_t)
        (maxaddr : haddr_t)
        -> _H5FD_t-pointer))

(define-hdf5 H5FDclose
  (_fun (file : _H5FD_t-pointer)
        -> herr_t))

(define-hdf5 H5FDcmp
  (_fun (f1 : _H5FD_t)
        (f2 : _H5FD_t)
        -> _int))

(define-hdf5 H5FDquery
(_fun (f : _H5FD_t)
      (flags : _ulong)
      -> _int))

(define-hdf5 H5FDalloc
  (_fun (file : _H5FD_t)
        (type : H5FD_mem_t)
        (dxpl_id : hid_t)
        (size : hsize_t)
        -> haddr_t))

(define-hdf5 H5FDfree
  (_fun (file : _H5FD_t)
        (type : H5FD_mem_t)
        (dxpl_id : hid_t)
        (addr : haddr_t)
        (size : hsize_t)
        -> herr_t))

(define-hdf5 H5FDget_eoa
(_fun (file : _H5FD_t)
      (type : H5FD_mem_t)
      -> haddr_t))

(define-hdf5 H5FDset_eoa
  (_fun (file : _H5FD_t)
        (type : H5FD_mem_t)
        (eoa : haddr_t)
        -> herr_t))

(define-hdf5 H5FDget_eof
  (_fun (file : _H5FD_t)
        -> haddr_t))

(define-hdf5 H5FDget_vfd_handle
  (_fun (file : _H5FD_t)
        (fapl : hid_t)
        (file_handle : _pointer)
        -> herr_t))

(define-hdf5 H5FDread
  (_fun (file : _H5FD_t)
        (type : H5FD_mem_t)
        (dxpl_id : hid_t)
        (addr : haddr_t)
        (size : _size)
        (buf : _pointer) ;; out
        -> herr_t))

(define-hdf5 H5FDwrite
  (_fun (file : _H5FD_t)
        (type : H5FD_mem_t)
        (dxpl_id : hid_t)
        (addr : haddr_t)
        (size : _size)
        (buf : _pointer)
        -> herr_t))

(define-hdf5 H5FDflush
  (_fun (file : _H5FD_t)
        (dxpl_id : hid_t)
        (closing : _uint)
        -> herr_t))

(define-hdf5 H5FDtruncate
  (_fun (file : _H5FD_t)
        (dxpl_id : hid_t)
        (closing : hbool_t)
        -> herr_t))
