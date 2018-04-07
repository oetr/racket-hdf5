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


(provide define-cpointer-type+provide)
(define-syntax (define-cpointer-type+provide stx)
  (syntax-case stx ()
    [(_ _TYPE) #'(define-cpointer-type+provide _TYPE #f #f #f #:tag #f)]
    [(_ _TYPE #:tag the-tag) #'(define-cpointer-type+provide _TYPE #f #f #f #:tag the-tag)]
    [(_ _TYPE ptr-type) #'(define-cpointer-type+provide _TYPE ptr-type #f #f #:tag #f)]
    [(_ _TYPE ptr-type #:tag the-tag) #'(define-cpointer-type+provide _TYPE ptr-type #f #f #:tag the-tag)]
    [(_ _TYPE ptr-type scheme->c c->scheme) #'(define-cpointer-type+provide _TYPE ptr-type scheme->c c->scheme #:tag #f)]
    [(_ _TYPE ptr-type scheme->c c->scheme #:tag the-tag)
     
     (and (identifier? #'_TYPE)
          (regexp-match #rx"^_.+" (symbol->string (syntax-e #'_TYPE))))
     (let ([name (cadr (regexp-match #rx"^_(.+)$"
                                     (symbol->string (syntax-e #'_TYPE))))])
       (define (id . strings)
         (datum->syntax
          #'_TYPE (string->symbol (apply string-append strings)) #'_TYPE))
       (with-syntax ([TYPE       (id name)]
                     [TYPE?      (id name "?")]
                     [TYPE-tag   (id name "-tag")]
                     [_TYPE/null (id "_" name "/null")])
         #'(begin
             ;;(provide TYPE-tag)
             ;;(provide _TYPE)
             ;;(provide _TYPE/null)
             (define-cpointer-type _TYPE ptr-type scheme->c c->scheme #:tag the-tag)
             )))]))

(define-syntax typedef
  (syntax-rules ()
    [(_ name)
     (begin
       ;;(provide name)
       (define name (_cpointer/null 'name)))]))

(define off_t _long)
