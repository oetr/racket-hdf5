#lang racket

(require racket/base
         (only-in ffi/unsafe list->cblock)
         ffi/unsafe/define
         rackunit
         math/array)

(provide (all-defined-out))

(define (array->cblock arr (type #f))
  (define data (array->list arr))
  (list->cblock data type))
