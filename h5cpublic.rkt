#lang racket

(require ffi/unsafe
         "h5-utilities.rkt")

(define+provide H5C_cache_incr_mode
  (_enum '(H5C_incr__off
           H5C_incr__threshold)))

(define+provide H5C_cache_flash_incr_mode
  (_enum '(H5C_flash_incr__off
           H5C_flash_incr__add_space)))

(define+provide H5C_cache_decr_mode
  (_enum '(H5C_decr__off
           H5C_decr__threshold
           H5C_decr__age_out
           H5C_decr__age_out_with_threshold)))
