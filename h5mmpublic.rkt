#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt")

(provide (all-defined-out))

;; These typedefs are currently used for VL datatype allocation/freeing
(define H5MM_allocate_t
  (_fun (size : _size)
        (alloc_info : _pointer)
        -> _pointer))

(define H5MM_free_t
  (_fun (mem : _pointer)
        (free_info : _pointer)
        -> _void))

