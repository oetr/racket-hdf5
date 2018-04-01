#lang racket

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
