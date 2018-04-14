#lang racket

(require racket/base
         (only-in ffi/unsafe
                  vector->cblock
                  cblock->vector)
         ffi/unsafe/define
         rackunit
         math/array)

(provide (all-defined-out))

(define (array->cblock arr (type #f))
  (define data (array->vector arr))
  (vector->cblock data type))


(define (cblock->array cblock (type #f))
  (define data (cblock->vector data type))
  (vector->array data))
