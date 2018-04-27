#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt")


(provide (all-defined-out))


#| Generated automatically by bin/make_err -- do not edit |#
#| Add new errors to H5err.txt file |#


#|*******************|#
#| Major error codes |#
#|*******************|#
(define H5E_DATASET (get-ffi-obj 'H5E_DATASET_g hdf5-lib hid_t return-#f))
(define H5E_FUNC (get-ffi-obj 'H5E_FUNC_g hdf5-lib hid_t return-#f))
(define H5E_STORAGE (get-ffi-obj 'H5E_STORAGE_g hdf5-lib hid_t return-#f))
(define H5E_FILE (get-ffi-obj 'H5E_FILE_g hdf5-lib hid_t return-#f))
(define H5E_SOHM (get-ffi-obj 'H5E_SOHM_g hdf5-lib hid_t return-#f))
(define H5E_SYM (get-ffi-obj 'H5E_SYM_g hdf5-lib hid_t return-#f))
(define H5E_PLUGIN (get-ffi-obj 'H5E_PLUGIN_g hdf5-lib hid_t return-#f))
(define H5E_VFL (get-ffi-obj 'H5E_VFL_g hdf5-lib hid_t return-#f))
(define H5EHID_TERNAL (get-ffi-obj 'H5EHID_TERNAL_g hdf5-lib hid_t return-#f))
(define H5E_BTREE (get-ffi-obj 'H5E_BTREE_g hdf5-lib hid_t return-#f))
(define H5E_REFERENCE (get-ffi-obj 'H5E_REFERENCE_g hdf5-lib hid_t return-#f))
(define H5E_DATASPACE (get-ffi-obj 'H5E_DATASPACE_g hdf5-lib hid_t return-#f))
(define H5E_RESOURCE (get-ffi-obj 'H5E_RESOURCE_g hdf5-lib hid_t return-#f))
(define H5E_PLIST (get-ffi-obj 'H5E_PLIST_g hdf5-lib hid_t return-#f))
(define H5E_LINK (get-ffi-obj 'H5E_LINK_g hdf5-lib hid_t return-#f))
(define H5E_DATATYPE (get-ffi-obj 'H5E_DATATYPE_g hdf5-lib hid_t return-#f))
(define H5E_RS (get-ffi-obj 'H5E_RS_g hdf5-lib hid_t return-#f))
(define H5E_HEAP (get-ffi-obj 'H5E_HEAP_g hdf5-lib hid_t return-#f))
(define H5E_OHDR (get-ffi-obj 'H5E_OHDR_g hdf5-lib hid_t return-#f))
(define H5E_ATOM (get-ffi-obj 'H5E_ATOM_g hdf5-lib hid_t return-#f))
(define H5E_ATTR (get-ffi-obj 'H5E_ATTR_g hdf5-lib hid_t return-#f))
(define H5E_NONE_MAJOR (get-ffi-obj 'H5E_NONE_MAJOR_g hdf5-lib hid_t return-#f))
(define H5E_IO (get-ffi-obj 'H5E_IO_g hdf5-lib hid_t return-#f))
(define H5E_SLIST (get-ffi-obj 'H5E_SLIST_g hdf5-lib hid_t return-#f))
(define H5E_EFL (get-ffi-obj 'H5E_EFL_g hdf5-lib hid_t return-#f))
(define H5E_TST (get-ffi-obj 'H5E_TST_g hdf5-lib hid_t return-#f))
(define H5E_ARGS (get-ffi-obj 'H5E_ARGS_g hdf5-lib hid_t return-#f))
(define H5E_ERROR (get-ffi-obj 'H5E_ERROR_g hdf5-lib hid_t return-#f))
(define H5E_PLINE (get-ffi-obj 'H5E_PLINE_g hdf5-lib hid_t return-#f))
(define H5E_FSPACE (get-ffi-obj 'H5E_FSPACE_g hdf5-lib hid_t return-#f))
(define H5E_CACHE (get-ffi-obj 'H5E_CACHE_g hdf5-lib hid_t return-#f))
(define H5E_DATASET_g (get-ffi-obj 'H5E_DATASET_g hdf5-lib hid_t return-#f)) #| Dataset |#
(define H5E_FUNC_g (get-ffi-obj 'H5E_FUNC_g hdf5-lib hid_t return-#f)) #| Function entry/exit |#
(define H5E_STORAGE_g (get-ffi-obj 'H5E_STORAGE_g hdf5-lib hid_t return-#f)) #| Data storage |#
(define H5E_FILE_g (get-ffi-obj 'H5E_FILE_g hdf5-lib hid_t return-#f)) #| File accessibilty |#
(define H5E_SOHM_g (get-ffi-obj 'H5E_SOHM_g hdf5-lib hid_t return-#f)) #| Shared Object Header Messages |#
(define H5E_SYM_g (get-ffi-obj 'H5E_SYM_g hdf5-lib hid_t return-#f)) #| Symbol table |#
(define H5E_PLUGIN_g (get-ffi-obj 'H5E_PLUGIN_g hdf5-lib hid_t return-#f)) #| Plugin for dynamically loaded library |#
(define H5E_VFL_g (get-ffi-obj 'H5E_VFL_g hdf5-lib hid_t return-#f)) #| Virtual File Layer |#
(define H5EHID_TERNAL_g (get-ffi-obj 'H5EHID_TERNAL_g hdf5-lib hid_t return-#f)) #| Internal error (too specific to document in detail) |#
(define H5E_BTREE_g (get-ffi-obj 'H5E_BTREE_g hdf5-lib hid_t return-#f)) #| B-Tree node |#
(define H5E_REFERENCE_g (get-ffi-obj 'H5E_REFERENCE_g hdf5-lib hid_t return-#f)) #| References |#
(define H5E_DATASPACE_g (get-ffi-obj 'H5E_DATASPACE_g hdf5-lib hid_t return-#f)) #| Dataspace |#
(define H5E_RESOURCE_g (get-ffi-obj 'H5E_RESOURCE_g hdf5-lib hid_t return-#f)) #| Resource unavailable |#
(define H5E_PLIST_g (get-ffi-obj 'H5E_PLIST_g hdf5-lib hid_t return-#f)) #| Property lists |#
(define H5E_LINK_g (get-ffi-obj 'H5E_LINK_g hdf5-lib hid_t return-#f)) #| Links |#
(define H5E_DATATYPE_g (get-ffi-obj 'H5E_DATATYPE_g hdf5-lib hid_t return-#f)) #| Datatype |#
(define H5E_RS_g (get-ffi-obj 'H5E_RS_g hdf5-lib hid_t return-#f)) #| Reference Counted Strings |#
(define H5E_HEAP_g (get-ffi-obj 'H5E_HEAP_g hdf5-lib hid_t return-#f)) #| Heap |#
(define H5E_OHDR_g (get-ffi-obj 'H5E_OHDR_g hdf5-lib hid_t return-#f)) #| Object header |#
(define H5E_ATOM_g (get-ffi-obj 'H5E_ATOM_g hdf5-lib hid_t return-#f)) #| Object atom |#
(define H5E_ATTR_g (get-ffi-obj 'H5E_ATTR_g hdf5-lib hid_t return-#f)) #| Attribute |#
(define H5E_NONE_MAJOR_g (get-ffi-obj 'H5E_NONE_MAJOR_g hdf5-lib hid_t return-#f)) #| No error |#
(define H5E_IO_g (get-ffi-obj 'H5E_IO_g hdf5-lib hid_t return-#f)) #| Low-level I/O |#
(define H5E_SLIST_g (get-ffi-obj 'H5E_SLIST_g hdf5-lib hid_t return-#f)) #| Skip Lists |#
(define H5E_EFL_g (get-ffi-obj 'H5E_EFL_g hdf5-lib hid_t return-#f)) #| External file list |#
(define H5E_TST_g (get-ffi-obj 'H5E_TST_g hdf5-lib hid_t return-#f)) #| Ternary Search Trees |#
(define H5E_ARGS_g (get-ffi-obj 'H5E_ARGS_g hdf5-lib hid_t return-#f)) #| Invalid arguments to routine |#
(define H5E_ERROR_g (get-ffi-obj 'H5E_ERROR_g hdf5-lib hid_t return-#f)) #| Error API |#
(define H5E_PLINE_g (get-ffi-obj 'H5E_PLINE_g hdf5-lib hid_t return-#f)) #| Data filters |#
(define H5E_FSPACE_g (get-ffi-obj 'H5E_FSPACE_g hdf5-lib hid_t return-#f)) #| Free Space Manager |#
(define H5E_CACHE_g (get-ffi-obj 'H5E_CACHE_g hdf5-lib hid_t return-#f)) #| Object cache |#

#|*******************|#
#| Minor error codes |#
#|*******************|#

#| Generic low-level file I/O errors |#
(define H5E_SEEKERROR (get-ffi-obj 'H5E_SEEKERROR_g hdf5-lib hid_t return-#f))
(define H5E_READERROR (get-ffi-obj 'H5E_READERROR_g hdf5-lib hid_t return-#f))
(define H5E_WRITEERROR (get-ffi-obj 'H5E_WRITEERROR_g hdf5-lib hid_t return-#f))
(define H5E_CLOSEERROR (get-ffi-obj 'H5E_CLOSEERROR_g hdf5-lib hid_t return-#f))
(define H5E_OVERFLOW (get-ffi-obj 'H5E_OVERFLOW_g hdf5-lib hid_t return-#f))
(define H5E_FCNTL (get-ffi-obj 'H5E_FCNTL_g hdf5-lib hid_t return-#f))
(define H5E_SEEKERROR_g (get-ffi-obj 'H5E_SEEKERROR_g hdf5-lib hid_t return-#f)) #| Seek failed |#
(define H5E_READERROR_g (get-ffi-obj 'H5E_READERROR_g hdf5-lib hid_t return-#f)) #| Read failed |#
(define H5E_WRITEERROR_g (get-ffi-obj 'H5E_WRITEERROR_g hdf5-lib hid_t return-#f)) #| Write failed |#
(define H5E_CLOSEERROR_g (get-ffi-obj 'H5E_CLOSEERROR_g hdf5-lib hid_t return-#f)) #| Close failed |#
(define H5E_OVERFLOW_g (get-ffi-obj 'H5E_OVERFLOW_g hdf5-lib hid_t return-#f)) #| Address overflowed |#
(define H5E_FCNTL_g (get-ffi-obj 'H5E_FCNTL_g hdf5-lib hid_t return-#f)) #| File control (fcntl) failed |#

#| Resource errors |#
(define H5E_NOSPACE (get-ffi-obj 'H5E_NOSPACE_g hdf5-lib hid_t return-#f))
(define H5E_CANTALLOC (get-ffi-obj 'H5E_CANTALLOC_g hdf5-lib hid_t return-#f))
(define H5E_CANTCOPY (get-ffi-obj 'H5E_CANTCOPY_g hdf5-lib hid_t return-#f))
(define H5E_CANTFREE (get-ffi-obj 'H5E_CANTFREE_g hdf5-lib hid_t return-#f))
(define H5E_ALREADYEXISTS (get-ffi-obj 'H5E_ALREADYEXISTS_g hdf5-lib hid_t return-#f))
(define H5E_CANTLOCK (get-ffi-obj 'H5E_CANTLOCK_g hdf5-lib hid_t return-#f))
(define H5E_CANTUNLOCK (get-ffi-obj 'H5E_CANTUNLOCK_g hdf5-lib hid_t return-#f))
(define H5E_CANTGC (get-ffi-obj 'H5E_CANTGC_g hdf5-lib hid_t return-#f))
(define H5E_CANTGETSIZE (get-ffi-obj 'H5E_CANTGETSIZE_g hdf5-lib hid_t return-#f))
(define H5E_OBJOPEN (get-ffi-obj 'H5E_OBJOPEN_g hdf5-lib hid_t return-#f))
(define H5E_NOSPACE_g (get-ffi-obj 'H5E_NOSPACE_g hdf5-lib hid_t return-#f)) #| No space available for allocation |#
(define H5E_CANTALLOC_g (get-ffi-obj 'H5E_CANTALLOC_g hdf5-lib hid_t return-#f)) #| Can't allocate space |#
(define H5E_CANTCOPY_g (get-ffi-obj 'H5E_CANTCOPY_g hdf5-lib hid_t return-#f)) #| Unable to copy object |#
(define H5E_CANTFREE_g (get-ffi-obj 'H5E_CANTFREE_g hdf5-lib hid_t return-#f)) #| Unable to free object |#
(define H5E_ALREADYEXISTS_g (get-ffi-obj 'H5E_ALREADYEXISTS_g hdf5-lib hid_t return-#f)) #| Object already exists |#
(define H5E_CANTLOCK_g (get-ffi-obj 'H5E_CANTLOCK_g hdf5-lib hid_t return-#f)) #| Unable to lock object |#
(define H5E_CANTUNLOCK_g (get-ffi-obj 'H5E_CANTUNLOCK_g hdf5-lib hid_t return-#f)) #| Unable to unlock object |#
(define H5E_CANTGC_g (get-ffi-obj 'H5E_CANTGC_g hdf5-lib hid_t return-#f)) #| Unable to garbage collect |#
(define H5E_CANTGETSIZE_g (get-ffi-obj 'H5E_CANTGETSIZE_g hdf5-lib hid_t return-#f)) #| Unable to compute size |#
(define H5E_OBJOPEN_g (get-ffi-obj 'H5E_OBJOPEN_g hdf5-lib hid_t return-#f)) #| Object is already open |#

#| Heap errors |#
(define H5E_CANTRESTORE (get-ffi-obj 'H5E_CANTRESTORE_g hdf5-lib hid_t return-#f))
(define H5E_CANTCOMPUTE (get-ffi-obj 'H5E_CANTCOMPUTE_g hdf5-lib hid_t return-#f))
(define H5E_CANTEXTEND (get-ffi-obj 'H5E_CANTEXTEND_g hdf5-lib hid_t return-#f))
(define H5E_CANTATTACH (get-ffi-obj 'H5E_CANTATTACH_g hdf5-lib hid_t return-#f))
(define H5E_CANTUPDATE (get-ffi-obj 'H5E_CANTUPDATE_g hdf5-lib hid_t return-#f))
(define H5E_CANTOPERATE (get-ffi-obj 'H5E_CANTOPERATE_g hdf5-lib hid_t return-#f))
(define H5E_CANTRESTORE_g (get-ffi-obj 'H5E_CANTRESTORE_g hdf5-lib hid_t return-#f)) #| Can't restore condition |#
(define H5E_CANTCOMPUTE_g (get-ffi-obj 'H5E_CANTCOMPUTE_g hdf5-lib hid_t return-#f)) #| Can't compute value |#
(define H5E_CANTEXTEND_g (get-ffi-obj 'H5E_CANTEXTEND_g hdf5-lib hid_t return-#f)) #| Can't extend heap's space |#
(define H5E_CANTATTACH_g (get-ffi-obj 'H5E_CANTATTACH_g hdf5-lib hid_t return-#f)) #| Can't attach object |#
(define H5E_CANTUPDATE_g (get-ffi-obj 'H5E_CANTUPDATE_g hdf5-lib hid_t return-#f)) #| Can't update object |#
(define H5E_CANTOPERATE_g (get-ffi-obj 'H5E_CANTOPERATE_g hdf5-lib hid_t return-#f)) #| Can't operate on object |#

#| Function entry/exit interface errors |#
(define H5E_CANTINIT (get-ffi-obj 'H5E_CANTINIT_g hdf5-lib hid_t return-#f))
(define H5E_ALREADYINIT (get-ffi-obj 'H5E_ALREADYINIT_g hdf5-lib hid_t return-#f))
(define H5E_CANTRELEASE (get-ffi-obj 'H5E_CANTRELEASE_g hdf5-lib hid_t return-#f))
(define H5E_CANTINIT_g (get-ffi-obj 'H5E_CANTINIT_g hdf5-lib hid_t return-#f)) #| Unable to initialize object |#
(define H5E_ALREADYINIT_g (get-ffi-obj 'H5E_ALREADYINIT_g hdf5-lib hid_t return-#f)) #| Object already initialized |#
(define H5E_CANTRELEASE_g (get-ffi-obj 'H5E_CANTRELEASE_g hdf5-lib hid_t return-#f)) #| Unable to release object |#

#| Property list errors |#
(define H5E_CANTGET (get-ffi-obj 'H5E_CANTGET_g hdf5-lib hid_t return-#f))
(define H5E_CANTSET (get-ffi-obj 'H5E_CANTSET_g hdf5-lib hid_t return-#f))
(define H5E_DUPCLASS (get-ffi-obj 'H5E_DUPCLASS_g hdf5-lib hid_t return-#f))
(define H5E_SETDISALLOWED (get-ffi-obj 'H5E_SETDISALLOWED_g hdf5-lib hid_t return-#f))
(define H5E_CANTGET_g (get-ffi-obj 'H5E_CANTGET_g hdf5-lib hid_t return-#f)) #| Can't get value |#
(define H5E_CANTSET_g (get-ffi-obj 'H5E_CANTSET_g hdf5-lib hid_t return-#f)) #| Can't set value |#
(define H5E_DUPCLASS_g (get-ffi-obj 'H5E_DUPCLASS_g hdf5-lib hid_t return-#f)) #| Duplicate class name in parent class |#
(define H5E_SETDISALLOWED_g (get-ffi-obj 'H5E_SETDISALLOWED_g hdf5-lib hid_t return-#f)) #| Disallowed operation |#

#| Free space errors |#
(define H5E_CANTMERGE (get-ffi-obj 'H5E_CANTMERGE_g hdf5-lib hid_t return-#f))
(define H5E_CANTREVIVE (get-ffi-obj 'H5E_CANTREVIVE_g hdf5-lib hid_t return-#f))
(define H5E_CANTSHRINK (get-ffi-obj 'H5E_CANTSHRINK_g hdf5-lib hid_t return-#f))
(define H5E_CANTMERGE_g (get-ffi-obj 'H5E_CANTMERGE_g hdf5-lib hid_t return-#f)) #| Can't merge objects |#
(define H5E_CANTREVIVE_g (get-ffi-obj 'H5E_CANTREVIVE_g hdf5-lib hid_t return-#f)) #| Can't revive object |#
(define H5E_CANTSHRINK_g (get-ffi-obj 'H5E_CANTSHRINK_g hdf5-lib hid_t return-#f)) #| Can't shrink container |#

#| Object header related errors |#
(define H5E_LINKCOUNT (get-ffi-obj 'H5E_LINKCOUNT_g hdf5-lib hid_t return-#f))
(define H5E_VERSION (get-ffi-obj 'H5E_VERSION_g hdf5-lib hid_t return-#f))
(define H5E_ALIGNMENT (get-ffi-obj 'H5E_ALIGNMENT_g hdf5-lib hid_t return-#f))
(define H5E_BADMESG (get-ffi-obj 'H5E_BADMESG_g hdf5-lib hid_t return-#f))
(define H5E_CANTDELETE (get-ffi-obj 'H5E_CANTDELETE_g hdf5-lib hid_t return-#f))
(define H5E_BADITER (get-ffi-obj 'H5E_BADITER_g hdf5-lib hid_t return-#f))
(define H5E_CANTPACK (get-ffi-obj 'H5E_CANTPACK_g hdf5-lib hid_t return-#f))
(define H5E_CANTRESET (get-ffi-obj 'H5E_CANTRESET_g hdf5-lib hid_t return-#f))
(define H5E_CANTRENAME (get-ffi-obj 'H5E_CANTRENAME_g hdf5-lib hid_t return-#f))
(define H5E_LINKCOUNT_g (get-ffi-obj 'H5E_LINKCOUNT_g hdf5-lib hid_t return-#f)) #| Bad object header link count |#
(define H5E_VERSION_g (get-ffi-obj 'H5E_VERSION_g hdf5-lib hid_t return-#f)) #| Wrong version number |#
(define H5E_ALIGNMENT_g (get-ffi-obj 'H5E_ALIGNMENT_g hdf5-lib hid_t return-#f)) #| Alignment error |#
(define H5E_BADMESG_g (get-ffi-obj 'H5E_BADMESG_g hdf5-lib hid_t return-#f)) #| Unrecognized message |#
(define H5E_CANTDELETE_g (get-ffi-obj 'H5E_CANTDELETE_g hdf5-lib hid_t return-#f)) #| Can't delete message |#
(define H5E_BADITER_g (get-ffi-obj 'H5E_BADITER_g hdf5-lib hid_t return-#f)) #| Iteration failed |#
(define H5E_CANTPACK_g (get-ffi-obj 'H5E_CANTPACK_g hdf5-lib hid_t return-#f)) #| Can't pack messages |#
(define H5E_CANTRESET_g (get-ffi-obj 'H5E_CANTRESET_g hdf5-lib hid_t return-#f)) #| Can't reset object |#
(define H5E_CANTRENAME_g (get-ffi-obj 'H5E_CANTRENAME_g hdf5-lib hid_t return-#f)) #| Unable to rename object |#

#| System level errors |#
(define H5E_SYSERRSTR (get-ffi-obj 'H5E_SYSERRSTR_g hdf5-lib hid_t return-#f))
(define H5E_SYSERRSTR_g (get-ffi-obj 'H5E_SYSERRSTR_g hdf5-lib hid_t return-#f)) #| System error message |#

#| I/O pipeline errors |#
(define H5E_NOFILTER (get-ffi-obj 'H5E_NOFILTER_g hdf5-lib hid_t return-#f))
(define H5E_CALLBACK (get-ffi-obj 'H5E_CALLBACK_g hdf5-lib hid_t return-#f))
(define H5E_CANAPPLY (get-ffi-obj 'H5E_CANAPPLY_g hdf5-lib hid_t return-#f))
(define H5E_SETLOCAL (get-ffi-obj 'H5E_SETLOCAL_g hdf5-lib hid_t return-#f))
(define H5E_NOENCODER (get-ffi-obj 'H5E_NOENCODER_g hdf5-lib hid_t return-#f))
(define H5E_CANTFILTER (get-ffi-obj 'H5E_CANTFILTER_g hdf5-lib hid_t return-#f))
(define H5E_NOFILTER_g (get-ffi-obj 'H5E_NOFILTER_g hdf5-lib hid_t return-#f)) #| Requested filter is not available |#
(define H5E_CALLBACK_g (get-ffi-obj 'H5E_CALLBACK_g hdf5-lib hid_t return-#f)) #| Callback failed |#
(define H5E_CANAPPLY_g (get-ffi-obj 'H5E_CANAPPLY_g hdf5-lib hid_t return-#f)) #| Error from filter 'can apply' callback |#
(define H5E_SETLOCAL_g (get-ffi-obj 'H5E_SETLOCAL_g hdf5-lib hid_t return-#f)) #| Error from filter 'set local' callback |#
(define H5E_NOENCODER_g (get-ffi-obj 'H5E_NOENCODER_g hdf5-lib hid_t return-#f)) #| Filter present but encoding disabled |#
(define H5E_CANTFILTER_g (get-ffi-obj 'H5E_CANTFILTER_g hdf5-lib hid_t return-#f)) #| Filter operation failed |#

#| Group related errors |#
(define H5E_CANTOPENOBJ (get-ffi-obj 'H5E_CANTOPENOBJ_g hdf5-lib hid_t return-#f))
(define H5E_CANTCLOSEOBJ (get-ffi-obj 'H5E_CANTCLOSEOBJ_g hdf5-lib hid_t return-#f))
(define H5E_COMPLEN (get-ffi-obj 'H5E_COMPLEN_g hdf5-lib hid_t return-#f))
(define H5E_PATH (get-ffi-obj 'H5E_PATH_g hdf5-lib hid_t return-#f))
(define H5E_CANTOPENOBJ_g (get-ffi-obj 'H5E_CANTOPENOBJ_g hdf5-lib hid_t return-#f)) #| Can't open object |#
(define H5E_CANTCLOSEOBJ_g (get-ffi-obj 'H5E_CANTCLOSEOBJ_g hdf5-lib hid_t return-#f)) #| Can't close object |#
(define H5E_COMPLEN_g (get-ffi-obj 'H5E_COMPLEN_g hdf5-lib hid_t return-#f)) #| Name component is too long |#
(define H5E_PATH_g (get-ffi-obj 'H5E_PATH_g hdf5-lib hid_t return-#f)) #| Problem with path to object |#

#| No error |#
(define H5E_NONE_MINOR (get-ffi-obj 'H5E_NONE_MINOR_g hdf5-lib hid_t return-#f))
(define H5E_NONE_MINOR_g (get-ffi-obj 'H5E_NONE_MINOR_g hdf5-lib hid_t return-#f)) #| No error |#

#| Plugin errors |#
(define H5E_OPENERROR (get-ffi-obj 'H5E_OPENERROR_g hdf5-lib hid_t return-#f))
(define H5E_OPENERROR_g (get-ffi-obj 'H5E_OPENERROR_g hdf5-lib hid_t return-#f)) #| Can't open directory or file |#

#| File accessibilty errors |#
(define H5E_FILEEXISTS (get-ffi-obj 'H5E_FILEEXISTS_g hdf5-lib hid_t return-#f))
(define H5E_FILEOPEN (get-ffi-obj 'H5E_FILEOPEN_g hdf5-lib hid_t return-#f))
(define H5E_CANTCREATE (get-ffi-obj 'H5E_CANTCREATE_g hdf5-lib hid_t return-#f))
(define H5E_CANTOPENFILE (get-ffi-obj 'H5E_CANTOPENFILE_g hdf5-lib hid_t return-#f))
(define H5E_CANTCLOSEFILE (get-ffi-obj 'H5E_CANTCLOSEFILE_g hdf5-lib hid_t return-#f))
(define H5E_NOTHDF5 (get-ffi-obj 'H5E_NOTHDF5_g hdf5-lib hid_t return-#f))
(define H5E_BADFILE (get-ffi-obj 'H5E_BADFILE_g hdf5-lib hid_t return-#f))
(define H5E_TRUNCATED (get-ffi-obj 'H5E_TRUNCATED_g hdf5-lib hid_t return-#f))
(define H5E_MOUNT (get-ffi-obj 'H5E_MOUNT_g hdf5-lib hid_t return-#f))
(define H5E_FILEEXISTS_g (get-ffi-obj 'H5E_FILEEXISTS_g hdf5-lib hid_t return-#f)) #| File already exists |#
(define H5E_FILEOPEN_g (get-ffi-obj 'H5E_FILEOPEN_g hdf5-lib hid_t return-#f)) #| File already open |#
(define H5E_CANTCREATE_g (get-ffi-obj 'H5E_CANTCREATE_g hdf5-lib hid_t return-#f)) #| Unable to create file |#
(define H5E_CANTOPENFILE_g (get-ffi-obj 'H5E_CANTOPENFILE_g hdf5-lib hid_t return-#f)) #| Unable to open file |#
(define H5E_CANTCLOSEFILE_g (get-ffi-obj 'H5E_CANTCLOSEFILE_g hdf5-lib hid_t return-#f)) #| Unable to close file |#
(define H5E_NOTHDF5_g (get-ffi-obj 'H5E_NOTHDF5_g hdf5-lib hid_t return-#f)) #| Not an HDF5 file |#
(define H5E_BADFILE_g (get-ffi-obj 'H5E_BADFILE_g hdf5-lib hid_t return-#f)) #| Bad file ID accessed |#
(define H5E_TRUNCATED_g (get-ffi-obj 'H5E_TRUNCATED_g hdf5-lib hid_t return-#f)) #| File has been truncated |#
(define H5E_MOUNT_g (get-ffi-obj 'H5E_MOUNT_g hdf5-lib hid_t return-#f)) #| File mount error |#

#| Object atom related errors |#
(define H5E_BADATOM (get-ffi-obj 'H5E_BADATOM_g hdf5-lib hid_t return-#f))
(define H5E_BADGROUP (get-ffi-obj 'H5E_BADGROUP_g hdf5-lib hid_t return-#f))
(define H5E_CANTREGISTER (get-ffi-obj 'H5E_CANTREGISTER_g hdf5-lib hid_t return-#f))
(define H5E_CANTINC (get-ffi-obj 'H5E_CANTINC_g hdf5-lib hid_t return-#f))
(define H5E_CANTDEC (get-ffi-obj 'H5E_CANTDEC_g hdf5-lib hid_t return-#f))
(define H5E_NOIDS (get-ffi-obj 'H5E_NOIDS_g hdf5-lib hid_t return-#f))
(define H5E_BADATOM_g (get-ffi-obj 'H5E_BADATOM_g hdf5-lib hid_t return-#f)) #| Unable to find atom information (already closed?) |#
(define H5E_BADGROUP_g (get-ffi-obj 'H5E_BADGROUP_g hdf5-lib hid_t return-#f)) #| Unable to find ID group information |#
(define H5E_CANTREGISTER_g (get-ffi-obj 'H5E_CANTREGISTER_g hdf5-lib hid_t return-#f)) #| Unable to register new atom |#
(define H5E_CANTINC_g (get-ffi-obj 'H5E_CANTINC_g hdf5-lib hid_t return-#f)) #| Unable to increment reference count |#
(define H5E_CANTDEC_g (get-ffi-obj 'H5E_CANTDEC_g hdf5-lib hid_t return-#f)) #| Unable to decrement reference count |#
(define H5E_NOIDS_g (get-ffi-obj 'H5E_NOIDS_g hdf5-lib hid_t return-#f)) #| Out of IDs for group |#

#| Cache related errors |#
(define H5E_CANTFLUSH (get-ffi-obj 'H5E_CANTFLUSH_g hdf5-lib hid_t return-#f))
(define H5E_CANTSERIALIZE (get-ffi-obj 'H5E_CANTSERIALIZE_g hdf5-lib hid_t return-#f))
(define H5E_CANTLOAD (get-ffi-obj 'H5E_CANTLOAD_g hdf5-lib hid_t return-#f))
(define H5E_PROTECT (get-ffi-obj 'H5E_PROTECT_g hdf5-lib hid_t return-#f))
(define H5E_NOTCACHED (get-ffi-obj 'H5E_NOTCACHED_g hdf5-lib hid_t return-#f))
(define H5E_SYSTEM (get-ffi-obj 'H5E_SYSTEM_g hdf5-lib hid_t return-#f))
(define H5E_CANTINS (get-ffi-obj 'H5E_CANTINS_g hdf5-lib hid_t return-#f))
(define H5E_CANTPROTECT (get-ffi-obj 'H5E_CANTPROTECT_g hdf5-lib hid_t return-#f))
(define H5E_CANTUNPROTECT (get-ffi-obj 'H5E_CANTUNPROTECT_g hdf5-lib hid_t return-#f))
(define H5E_CANTPIN (get-ffi-obj 'H5E_CANTPIN_g hdf5-lib hid_t return-#f))
(define H5E_CANTUNPIN (get-ffi-obj 'H5E_CANTUNPIN_g hdf5-lib hid_t return-#f))
(define H5E_CANTMARKDIRTY (get-ffi-obj 'H5E_CANTMARKDIRTY_g hdf5-lib hid_t return-#f))
(define H5E_CANTDIRTY (get-ffi-obj 'H5E_CANTDIRTY_g hdf5-lib hid_t return-#f))
(define H5E_CANTEXPUNGE (get-ffi-obj 'H5E_CANTEXPUNGE_g hdf5-lib hid_t return-#f))
(define H5E_CANTRESIZE (get-ffi-obj 'H5E_CANTRESIZE_g hdf5-lib hid_t return-#f))
(define H5E_CANTFLUSH_g (get-ffi-obj 'H5E_CANTFLUSH_g hdf5-lib hid_t return-#f)) #| Unable to flush data from cache |#
(define H5E_CANTSERIALIZE_g (get-ffi-obj 'H5E_CANTSERIALIZE_g hdf5-lib hid_t return-#f)) #| Unable to serialize data from cache |#
(define H5E_CANTLOAD_g (get-ffi-obj 'H5E_CANTLOAD_g hdf5-lib hid_t return-#f)) #| Unable to load metadata into cache |#
(define H5E_PROTECT_g (get-ffi-obj 'H5E_PROTECT_g hdf5-lib hid_t return-#f)) #| Protected metadata error |#
(define H5E_NOTCACHED_g (get-ffi-obj 'H5E_NOTCACHED_g hdf5-lib hid_t return-#f)) #| Metadata not currently cached |#
(define H5E_SYSTEM_g (get-ffi-obj 'H5E_SYSTEM_g hdf5-lib hid_t return-#f)) #| Internal error detected |#
(define H5E_CANTINS_g (get-ffi-obj 'H5E_CANTINS_g hdf5-lib hid_t return-#f)) #| Unable to insert metadata into cache |#
(define H5E_CANTPROTECT_g (get-ffi-obj 'H5E_CANTPROTECT_g hdf5-lib hid_t return-#f)) #| Unable to protect metadata |#
(define H5E_CANTUNPROTECT_g (get-ffi-obj 'H5E_CANTUNPROTECT_g hdf5-lib hid_t return-#f)) #| Unable to unprotect metadata |#
(define H5E_CANTPIN_g (get-ffi-obj 'H5E_CANTPIN_g hdf5-lib hid_t return-#f)) #| Unable to pin cache entry |#
(define H5E_CANTUNPIN_g (get-ffi-obj 'H5E_CANTUNPIN_g hdf5-lib hid_t return-#f)) #| Unable to un-pin cache entry |#
(define H5E_CANTMARKDIRTY_g (get-ffi-obj 'H5E_CANTMARKDIRTY_g hdf5-lib hid_t return-#f)) #| Unable to mark a pinned entry as dirty |#
(define H5E_CANTDIRTY_g (get-ffi-obj 'H5E_CANTDIRTY_g hdf5-lib hid_t return-#f)) #| Unable to mark metadata as dirty |#
(define H5E_CANTEXPUNGE_g (get-ffi-obj 'H5E_CANTEXPUNGE_g hdf5-lib hid_t return-#f)) #| Unable to expunge a metadata cache entry |#
(define H5E_CANTRESIZE_g (get-ffi-obj 'H5E_CANTRESIZE_g hdf5-lib hid_t return-#f)) #| Unable to resize a metadata cache entry |#

#| Link related errors |#
(define H5E_TRAVERSE (get-ffi-obj 'H5E_TRAVERSE_g hdf5-lib hid_t return-#f))
(define H5E_NLINKS (get-ffi-obj 'H5E_NLINKS_g hdf5-lib hid_t return-#f))
(define H5E_NOTREGISTERED (get-ffi-obj 'H5E_NOTREGISTERED_g hdf5-lib hid_t return-#f))
(define H5E_CANTMOVE (get-ffi-obj 'H5E_CANTMOVE_g hdf5-lib hid_t return-#f))
(define H5E_CANTSORT (get-ffi-obj 'H5E_CANTSORT_g hdf5-lib hid_t return-#f))
(define H5E_TRAVERSE_g (get-ffi-obj 'H5E_TRAVERSE_g hdf5-lib hid_t return-#f)) #| Link traversal failure |#
(define H5E_NLINKS_g (get-ffi-obj 'H5E_NLINKS_g hdf5-lib hid_t return-#f)) #| Too many soft links in path |#
(define H5E_NOTREGISTERED_g (get-ffi-obj 'H5E_NOTREGISTERED_g hdf5-lib hid_t return-#f)) #| Link class not registered |#
(define H5E_CANTMOVE_g (get-ffi-obj 'H5E_CANTMOVE_g hdf5-lib hid_t return-#f)) #| Can't move object |#
(define H5E_CANTSORT_g (get-ffi-obj 'H5E_CANTSORT_g hdf5-lib hid_t return-#f)) #| Can't sort objects |#

#| Parallel MPI errors |#
(define H5E_MPI (get-ffi-obj 'H5E_MPI_g hdf5-lib hid_t return-#f))
(define H5E_MPIERRSTR (get-ffi-obj 'H5E_MPIERRSTR_g hdf5-lib hid_t return-#f))
(define H5E_CANTRECV (get-ffi-obj 'H5E_CANTRECV_g hdf5-lib hid_t return-#f))
(define H5E_MPI_g (get-ffi-obj 'H5E_MPI_g hdf5-lib hid_t return-#f)) #| Some MPI function failed |#
(define H5E_MPIERRSTR_g (get-ffi-obj 'H5E_MPIERRSTR_g hdf5-lib hid_t return-#f)) #| MPI Error String |#
(define H5E_CANTRECV_g (get-ffi-obj 'H5E_CANTRECV_g hdf5-lib hid_t return-#f)) #| Can't receive data |#

#| Dataspace errors |#
(define H5E_CANTCLIP (get-ffi-obj 'H5E_CANTCLIP_g hdf5-lib hid_t return-#f))
(define H5E_CANTCOUNT (get-ffi-obj 'H5E_CANTCOUNT_g hdf5-lib hid_t return-#f))
(define H5E_CANTSELECT (get-ffi-obj 'H5E_CANTSELECT_g hdf5-lib hid_t return-#f))
(define H5E_CANTNEXT (get-ffi-obj 'H5E_CANTNEXT_g hdf5-lib hid_t return-#f))
(define H5E_BADSELECT (get-ffi-obj 'H5E_BADSELECT_g hdf5-lib hid_t return-#f))
(define H5E_CANTCOMPARE (get-ffi-obj 'H5E_CANTCOMPARE_g hdf5-lib hid_t return-#f))
(define H5E_CANTCLIP_g (get-ffi-obj 'H5E_CANTCLIP_g hdf5-lib hid_t return-#f)) #| Can't clip hyperslab region |#
(define H5E_CANTCOUNT_g (get-ffi-obj 'H5E_CANTCOUNT_g hdf5-lib hid_t return-#f)) #| Can't count elements |#
(define H5E_CANTSELECT_g (get-ffi-obj 'H5E_CANTSELECT_g hdf5-lib hid_t return-#f)) #| Can't select hyperslab |#
(define H5E_CANTNEXT_g (get-ffi-obj 'H5E_CANTNEXT_g hdf5-lib hid_t return-#f)) #| Can't move to next iterator location |#
(define H5E_BADSELECT_g (get-ffi-obj 'H5E_BADSELECT_g hdf5-lib hid_t return-#f)) #| Invalid selection |#
(define H5E_CANTCOMPARE_g (get-ffi-obj 'H5E_CANTCOMPARE_g hdf5-lib hid_t return-#f)) #| Can't compare objects |#

#| Argument errors |#
(define H5E_UNINITIALIZED (get-ffi-obj 'H5E_UNINITIALIZED_g hdf5-lib hid_t return-#f))
(define H5E_UNSUPPORTED (get-ffi-obj 'H5E_UNSUPPORTED_g hdf5-lib hid_t return-#f))
(define H5E_BADTYPE (get-ffi-obj 'H5E_BADTYPE_g hdf5-lib hid_t return-#f))
(define H5E_BADRANGE (get-ffi-obj 'H5E_BADRANGE_g hdf5-lib hid_t return-#f))
(define H5E_BADVALUE (get-ffi-obj 'H5E_BADVALUE_g hdf5-lib hid_t return-#f))
(define H5E_UNINITIALIZED_g (get-ffi-obj 'H5E_UNINITIALIZED_g hdf5-lib hid_t return-#f)) #| Information is uinitialized |#
(define H5E_UNSUPPORTED_g (get-ffi-obj 'H5E_UNSUPPORTED_g hdf5-lib hid_t return-#f)) #| Feature is unsupported |#
(define H5E_BADTYPE_g (get-ffi-obj 'H5E_BADTYPE_g hdf5-lib hid_t return-#f)) #| Inappropriate type |#
(define H5E_BADRANGE_g (get-ffi-obj 'H5E_BADRANGE_g hdf5-lib hid_t return-#f)) #| Out of range |#
(define H5E_BADVALUE_g (get-ffi-obj 'H5E_BADVALUE_g hdf5-lib hid_t return-#f)) #| Bad value |#

#| B-tree related errors |#
(define H5E_NOTFOUND (get-ffi-obj 'H5E_NOTFOUND_g hdf5-lib hid_t return-#f))
(define H5E_EXISTS (get-ffi-obj 'H5E_EXISTS_g hdf5-lib hid_t return-#f))
(define H5E_CANTENCODE (get-ffi-obj 'H5E_CANTENCODE_g hdf5-lib hid_t return-#f))
(define H5E_CANTDECODE (get-ffi-obj 'H5E_CANTDECODE_g hdf5-lib hid_t return-#f))
(define H5E_CANTSPLIT (get-ffi-obj 'H5E_CANTSPLIT_g hdf5-lib hid_t return-#f))
(define H5E_CANTREDISTRIBUTE (get-ffi-obj 'H5E_CANTREDISTRIBUTE_g hdf5-lib hid_t return-#f))
(define H5E_CANTSWAP (get-ffi-obj 'H5E_CANTSWAP_g hdf5-lib hid_t return-#f))
(define H5E_CANTINSERT (get-ffi-obj 'H5E_CANTINSERT_g hdf5-lib hid_t return-#f))
(define H5E_CANTLIST (get-ffi-obj 'H5E_CANTLIST_g hdf5-lib hid_t return-#f))
(define H5E_CANTMODIFY (get-ffi-obj 'H5E_CANTMODIFY_g hdf5-lib hid_t return-#f))
(define H5E_CANTREMOVE (get-ffi-obj 'H5E_CANTREMOVE_g hdf5-lib hid_t return-#f))
(define H5E_NOTFOUND_g (get-ffi-obj 'H5E_NOTFOUND_g hdf5-lib hid_t return-#f)) #| Object not found |#
(define H5E_EXISTS_g (get-ffi-obj 'H5E_EXISTS_g hdf5-lib hid_t return-#f)) #| Object already exists |#
(define H5E_CANTENCODE_g (get-ffi-obj 'H5E_CANTENCODE_g hdf5-lib hid_t return-#f)) #| Unable to encode value |#
(define H5E_CANTDECODE_g (get-ffi-obj 'H5E_CANTDECODE_g hdf5-lib hid_t return-#f)) #| Unable to decode value |#
(define H5E_CANTSPLIT_g (get-ffi-obj 'H5E_CANTSPLIT_g hdf5-lib hid_t return-#f)) #| Unable to split node |#
(define H5E_CANTREDISTRIBUTE_g (get-ffi-obj 'H5E_CANTREDISTRIBUTE_g hdf5-lib hid_t return-#f)) #| Unable to redistribute records |#
(define H5E_CANTSWAP_g (get-ffi-obj 'H5E_CANTSWAP_g hdf5-lib hid_t return-#f)) #| Unable to swap records |#
(define H5E_CANTINSERT_g (get-ffi-obj 'H5E_CANTINSERT_g hdf5-lib hid_t return-#f)) #| Unable to insert object |#
(define H5E_CANTLIST_g (get-ffi-obj 'H5E_CANTLIST_g hdf5-lib hid_t return-#f)) #| Unable to list node |#
(define H5E_CANTMODIFY_g (get-ffi-obj 'H5E_CANTMODIFY_g hdf5-lib hid_t return-#f)) #| Unable to modify record |#
(define H5E_CANTREMOVE_g (get-ffi-obj 'H5E_CANTREMOVE_g hdf5-lib hid_t return-#f)) #| Unable to remove object |#

#| Datatype conversion errors |#
(define H5E_CANTCONVERT (get-ffi-obj 'H5E_CANTCONVERT_g hdf5-lib hid_t return-#f))
(define H5E_BADSIZE (get-ffi-obj 'H5E_BADSIZE_g hdf5-lib hid_t return-#f))
(define H5E_CANTCONVERT_g (get-ffi-obj 'H5E_CANTCONVERT_g hdf5-lib hid_t return-#f)) #| Can't convert datatypes |#
(define H5E_BADSIZE_g (get-ffi-obj 'H5E_BADSIZE_g hdf5-lib hid_t return-#f)) #| Bad size for object |#


