#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5ipublic.rkt"
         "h5epubgen.rkt")

(provide (all-defined-out))

#|
* This file contains public declarations for the H5E module.
|#


#| Value for the default error stack |#
(define H5E_DEFAULT    (cast 0 _int hid_t))

#| Different kinds of error information |#
(define H5E_type_t
  (_enum '(H5E_MAJOR
           H5E_MINOR)))

#| Information about an error; element of error stack |#
(define-cstruct _H5E_error2_t
  ([cls_id hid_t] #|class ID       |#
   [maj_num hid_t] #|major error ID  |#
   [min_num hid_t] #|minor error number  |#
   [line _uint] #|line in file where error occurs |#
   [func_name _string] #|function in which error occurred |#
   [file_name _string]#|file in which error occurred  |#
   [desc _string]))#|optional supplied description  |#


#| HDF5 error class |#
(define H5E_ERR_CLS (dynamic-constant 'H5E_ERR_CLS hdf5-lib hid_t))
(define H5E_ERR_CLS_g (dynamic-constant 'H5E_ERR_CLS hdf5-lib hid_t))


#| Error stack traversal direction |#
(define H5E_direction_t
  (_enum
   '(
     H5E_WALK_UPWARD = 0 #|begin deep, end at API function |#
     H5E_WALK_DOWNWARD = 1 #|begin at API function, end deep |#
     )))


#| Error stack traversal callback function pointers |#
(define H5E_walk2_t
  (_fun (n : _uint)
        (err_desc : _H5E_error2_t-pointer/null)
        (client_data : _pointer)
        -> herr_t))

(define H5E_auto2_t
  (_fun (estack : hid_t)
        (client_data : _pointer)
        -> herr_t))

#| Public API functions |#
(define-hdf5 H5Eregister_class
  (_fun (cls_name : _string)
        (lib_name : _string)
        (version : _string)
        -> hid_t))

(define-hdf5 H5Eunregister_class
  (_fun (class_id : hid_t)
        -> herr_t))

(define-hdf5 H5Eclose_msg
  (_fun (err_id : hid_t)
        -> herr_t))

(define-hdf5 H5Ecreate_msg
  (_fun (cls : hid_t)
        (msg_type : H5E_type_t)
        (msg : _string)
        -> hid_t))

(define-hdf5 H5Ecreate_stack
  (_fun -> hid_t))

(define-hdf5 H5Eget_current_stack
  (_fun -> hid_t))

(define-hdf5 H5Eclose_stack
  (_fun (stack_id : hid_t)
        -> herr_t))

(define-hdf5 H5Eget_class_name
  (_fun (class_id : hid_t)
        (name : _string) ;; TODO: out
        (size : _size)
        -> _ssize))

(define-hdf5 H5Eset_current_stack
  (_fun (err_stack_id : hid_t)
        -> herr_t))

(define-hdf5 H5Epush2
  (_fun (err_stack file func line cls_id maj_id min_id msg . args) ::
        (err_stack : hid_t)
        (file : _string)
        (func : _string)
        (line : _uint)
        (cls_id : hid_t)
        (maj_id : hid_t)
        (min_id : hid_t)
        (msg : _string)
        (args : _pointer)
        -> herr_t))

(define-hdf5 H5Epop
  (_fun (err_stack : hid_t)
        (count : _size)
        -> herr_t))

(define-hdf5 H5Eprint2
  (_fun (err_stack : hid_t)
        (stream : _file)
        -> herr_t))

(define-hdf5 H5Ewalk2
  (_fun (err_stack : hid_t)
        (direction : H5E_direction_t)
        (func : H5E_walk2_t)
        (client_data : _pointer)
        -> herr_t))

(define-hdf5 H5Eget_auto2
  (_fun (estack_id : hid_t)
        (func : H5E_auto2_t)
        (client_data : _pointer)
        -> herr_t))

(define-hdf5 H5Eset_auto2
  (_fun (estack_id : hid_t)
        (func : H5E_auto2_t)
        (client_data : _pointer)
        -> herr_t))

(define-hdf5 H5Eclear2
  (_fun (err_stack : hid_t)
        -> herr_t))

(define-hdf5 H5Eauto_is_v2
  (_fun (err_stack : hid_t)
        (is_stack : _pointer)
        -> herr_t))

(define-hdf5 H5Eget_msg
  (_fun (msg_id : hid_t)
        (type : H5E_type_t)
        (msg : _string) ;; TODO : out
        (size : _size)
        -> _ssize))

(define-hdf5 H5Eget_num
  (_fun (error_stack_id : hid_t)
        -> _ssize))


#| Symbols defined for compatibility with previous versions of the HDF5 API.
*
* Use of these symbols is deprecated.
|#
;;#ifndef H5_NO_DEPRECATED_SYMBOLS

#| Typedefs |#

#| Alias major & minor error types to hid_t's, for compatibility with new
*  error API in v1.8
|#
(define H5E_major_t hid_t)
(define H5E_minor_t hid_t)

#| Information about an error element of error stack. |#
(define-cstruct _H5E_error1_t
  ([maj_num H5E_major_t]    #|major error number     |#
   [min_num H5E_minor_t]    #|minor error number     |#
   [func_name _string]      #|function in which error occurred |#
   [file_name _string]      #|file in which error occurred  |#
   [line _uint]             #|line in file where error occurs |#
   [desc _string]))           #|optional supplied description  |#

#| Error stack traversal callback function pointers |#
(define H5E_walk1_t (_fun (n : _int)
                          (err_desc : _H5E_error1_t-pointer/null)
                          (client_data : _pointer)
                          -> herr_t))

(define H5E_auto1_t (_fun (client_data : _pointer) -> herr_t))

#| Function prototypes |#
(define-hdf5 H5Eclear1
  (_fun -> herr_t))

(define-hdf5 H5Eget_auto1
  (_fun (func : H5E_auto1_t)
        (client_data : _pointer)
        -> herr_t))

(define-hdf5 H5Epush1
  (_fun (file : _string)
        (func : _string)
        (line : _uint)
        (maj : H5E_major_t)
        (min : H5E_minor_t)
        (str : _string)
        -> herr_t))

(define-hdf5 H5Eprint1
  (_fun (stream : _file)
        -> herr_t))

(define-hdf5 H5Eset_auto1
  (_fun (func : H5E_auto1_t)
        (client_data : _pointer)
        -> herr_t))

(define-hdf5 H5Ewalk1
  (_fun (direction : H5E_direction_t)
        (func : H5E_walk1_t)
        (client_data : _pointer)
        -> herr_t))

(define-hdf5 H5Eget_major
  (_fun (maj : H5E_major_t)
        -> _string))

(define-hdf5 H5Eget_minor
  (_fun (min : H5E_minor_t)
        -> _string))

;; #endif #| H5_NO_DEPRECATED_SYMBOLS |#
