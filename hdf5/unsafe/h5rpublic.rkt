#|
 * This file contains public declarations for the H5S module.
 |#

#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5gpublic.rkt"
         "h5ipublic.rkt"
         "h5opublic.rkt")

(provide (all-defined-out))


#|
 * Reference types allowed.
|#
(define H5R_type_t
  (_enum
   '(
    H5R_BADTYPE     =   -1   #|invalid Reference Type                     |#
    H5R_OBJECT                 #|Object reference                           |#
    H5R_DATASET_REGION         #|Dataset Region Reference                   |#
    H5R_MAXTYPE                 #|highest type (Invalid as true type)	     |#
    )))

#| Note! Be careful with the sizes of the references because they should really
 * depend on the run-time values in the file.  Unfortunately, the arrays need
 * to be defined at compile-time, so we have to go with the worst case sizes for
 * them.  -QAK
 |#
(define H5R_OBJ_REF_BUF_SIZE    (ctype-sizeof haddr_t))
#| Object reference structure for user's code |#
(define hobj_ref_t haddr_t) #| Needs to be large enough to store largest haddr_t in a worst case machine (ie. 8 bytes currently) |#

(define H5R_DSET_REG_REF_BUF_SIZE    (+ (ctype-sizeof haddr_t) 4))
#| 4 is used instead of sizeof(int) to permit portability between
   the Crays and other machines (the heap ID is always encoded as an int32 anyway)
|#
#| Dataset Region reference structure for user's code |#
(define hdset_reg_ref_t (_list io _ubyte H5R_DSET_REG_REF_BUF_SIZE)) #| Buffer to store heap ID and index |#
#| Needs to be large enough to store largest haddr_t in a worst case machine (ie. 8 bytes currently) plus an int |#

#| Publicly visible data structures |#

#| Functions in H5R.c |#

(define-hdf5 H5Rcreate
  (_fun (loc-id name ref_type space_id) ::
        (ref : _pointer = (malloc _pointer 1 'atomic))
        (loc-id : hid_t)
        (name : _string)
        (ref_type : H5R_type_t)
        (space_id : hid_t)
        -> (status : herr_t)
        -> (if (< status 0)
               (error 'H5Rcreate "Unable to create reference.")
               ref)))

(define-hdf5 H5Rdereference
  (_fun (dataset : hid_t)
        (ref_type : H5R_type_t)
        (ref : _pointer)
        -> hid_t))

(define-hdf5 H5Rget_region
  (_fun (dataset : hid_t)
        (ref_type : H5R_type_t)
        (ref : _pointer)
        -> hid_t))

(define-hdf5 H5Rget_obj_type2
  (_fun (id : hid_t)
        (ref_type : H5R_type_t)
        (ref : _pointer)
        (obj_type : (_ptr io H5O_type_t))
        -> herr_t))

(define-hdf5 H5Rget_name
  (_fun (loc_id ref_type ref size) ::
        (loc_id : hid_t)
        (ref_type : H5R_type_t)
        (ref : _pointer)
        (name : (_bytes o size))
        (size : _size)
        -> (len-status : _ssize)
        -> (list len-status name)))

#| Function prototypes |#
(define-hdf5 H5Rget_obj_type1
  (_fun (id : hid_t)
        (ref_type : H5R_type_t)
        (_ref : _pointer)
        -> H5G_obj_t))

