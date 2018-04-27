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

;; recursively convert structure into lists
;; (define (struct*->cblock a-struct)
;;   (define ())
;;   (define (loop ))
;;   )

;; (define (struct->cblock a-struct ctypes)
;;   ;; make sure the number of types equals the number of struct elements
;;   (define struct-list (struct->list a-struct))
;;   (list->cblock(malloc (apply + (map ctype-sizeof ctypes)) 'atomic)
  
  
;;   )

