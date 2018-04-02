#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt")

(define+provide _H5I_type_t
  (_enum '(
           H5I_UNINIT = -2 ;; uninitialized type
           H5I_BADID = -1  ;; invalid Type
           H5I_FILE = 1    ;; type ID for File objects
           H5I_GROUP       ;; type ID for Group objects 
           H5I_DATATYPE    ;; type ID for Datatype objects
           H5I_DATASPACE   ;; type ID for Dataspace objects
           H5I_DATASET     ;; type ID for Dataset objects
           H5I_ATTR        ;; type ID for Attribute objects
           H5I_REFERENCE   ;; type ID for Reference objects
           H5I_VFL         ;; type ID for virtual file layer
           H5I_GENPROP_CLS ;; type ID for generic property list classes 
           H5I_GENPROP_LST ;; type ID for generic property lists
           H5I_ERROR_CLASS ;; type ID for error classes
           H5I_ERROR_MSG   ;; type ID for error messages
           H5I_ERROR_STACK ;; type ID for error stacks
           H5I_NTYPES)))   ;; number of library types, MUST BE LAST!

;; Type of atoms to return to users
(define+provide hid_t _int)
