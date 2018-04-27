#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5ipublic.rkt"  ;; ids
;;         "h5opublic.rkt"  ;; object headers
;;         "h5tpublic.rkt") ;; datatypes
)

