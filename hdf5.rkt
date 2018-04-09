#lang racket
;; Racket Foreign interface

(require "utilities.rkt"
         "h5public.rkt"
          "h5apublic.rkt"  ;; Attributes
          "h5acpublic.rkt" ;; Metadata cache
          "h5dpublic.rkt"  ;; Datasets
          ;;"h5epublic.rkt"  ;; Errors
          "h5fpublic.rkt"  ;; Files
          "h5fdpublic.rkt" ;; File drivers
          "h5gpublic.rkt"  ;; Groups
          "h5ipublic.rkt"  ;; ID management
          "h5lpublic.rkt"  ;; Links
          "h5mmpublic.rkt" ;; Memory management
          "h5opublic.rkt"  ;; Object headers
          "h5ppublic.rkt"  ;; Property lists
          ;;"h5plpublic.rkt" ;; Plugins
          ;;"h5rpublic.rkt"  ;; References
          "h5spublic.rkt"  ;; Dataspaces
          "h5tpublic.rkt"  ;; Datatypes
          "h5zpublic.rkt"  ;; Data filters
          
          )

(provide (all-from-out
          "utilities.rkt"
          "h5public.rkt"
          "h5apublic.rkt"  ;; Attributes
          "h5acpublic.rkt" ;; Metadata cache
          "h5dpublic.rkt"  ;; Datasets
          ;;"h5epublic.rkt"  ;; Errors
          "h5fpublic.rkt"  ;; Files
          "h5fdpublic.rkt" ;; File drivers
          "h5gpublic.rkt"  ;; Groups
          "h5ipublic.rkt"  ;; ID management
          "h5lpublic.rkt"  ;; Links
          "h5mmpublic.rkt" ;; Memory management
          "h5opublic.rkt"  ;; Object headers
          "h5ppublic.rkt"  ;; Property lists
          ;;"h5plpublic.rkt" ;; Plugins
          ;;"h5rpublic.rkt"  ;; References
          "h5spublic.rkt"  ;; Dataspaces
          "h5tpublic.rkt"  ;; Datatypes
          "h5zpublic.rkt"  ;; Data filters
          ))


