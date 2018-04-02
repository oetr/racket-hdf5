#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(provide (all-defined-out))

(define-syntax define+provide
  (syntax-rules ()
    [(_ (name args ... . rest) body ...)
     (begin (provide name)
            (define (name args ... . rest)
              body ...))]
    [(_ (name args ...) body ...)
     (begin (provide name)
            (define (name args ...)
              body ...))]
    [(_ name body)
     (begin
       (provide name)
       (define name body))]))

(define-syntax define-c+provide
  (syntax-rules ()
    [(_ name args ...)
     (begin (provide name)
            (define-c name args ...))]))


(define hdf5-lib (ffi-lib "libhdf5_serial"))

(define-ffi-definer define-hdf5-lib-internal hdf5-lib)

(define-syntax define-hdf5
  (syntax-rules ()
    [(_ name body)
     (begin
       ;;(provide name)
       (define-hdf5-lib-internal name body))]))



