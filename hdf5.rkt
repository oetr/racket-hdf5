#lang racket
;; Racket Foreign interface

(require "h5public.rkt"   ;; generic
         "h5apublic.rkt"  ;; attributes
         "h5acpublic.rkt" ;; metadata cache
         "h5dpublic.rkt"  ;; datasets
         ;; errors
         "h5fpublic.rkt"  ;; files
         ;; groups
         "h5ipublic.rkt"  ;; ID management
         "h5lpublic.rkt"  ;; links
         ;; memory management
         "h5opublic.rkt"  ;; object headers
         "h5ppublic.rkt"  ;; property lists
         ;; pl plugins
         ;; r references
         "h5spublic.rkt"  ;; dataspaces
         "h5tpublic.rkt"  ;; datatypes
         ;; z ;; data filters
         )

(provide (all-from-out "h5public.rkt"
                       "h5apublic.rkt"
                       "h5acpublic.rkt"
                       "h5dpublic.rkt"
                       "h5fpublic.rkt"
                       "h5ipublic.rkt"
                       "h5lpublic.rkt"
                       "h5opublic.rkt"
                       "h5ppublic.rkt"
                       "h5spublic.rkt"
                       "h5tpublic.rkt"))
