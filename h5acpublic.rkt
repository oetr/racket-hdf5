#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5cpublic.rkt"
         )
         
(define+provide H5AC__CURR_CACHE_CONFIG_VERSION	1)
(define+provide H5AC__MAX_TRACE_FILE_NAME_LEN 1024)

(define+provide H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY 0)
(define+provide H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED 1)

(provide _H5AC_cache_config_t)
(provide make-H5AC_cache_config_t)
(provide _H5AC_cache_config_t-pointer/null)
(define-cstruct _H5AC_cache_config_t
  (;; general configuration fields:
   [version _int]
   [rpt_fcn_enabled hbool_t]
   [open_trace_file hbool_t]
   [close_trace_file hbool_t]
   [trace_file_name _string]
   [evictions_enabled hbool_t]
   [set_initial_size hbool_t] 
   [initial_size _size]
   [min_clean_fraction _double]
   [max_size _size]
   [min_size _size]
   [epoch_length _long]

   ;; size increase control fields:
   [incr_mode H5C_cache_incr_mode]
   [lower_hr_threshold _double]
   [increment _double]
   
   [apply_max_increment hbool_t]
   [max_increment _size]

   [flash_incr_mode H5C_cache_flash_incr_mode]
   [flash_multiple _double]
   [flash_threshold _double]

    ;; size decrease control fields:
   [decr_mode H5C_cache_decr_mode]
   [upper_hr_threshold _double]
   [decrement _double]
   [apply_max_decrement hbool_t]
   [max_decrement _size]
   [epochs_before_eviction _int]
   [apply_empty_reserve hbool_t]
   [empty_reserve _double]
   ;; parallel configuration fields:
   [dirty_bytes_threshold _int]
   [metadata_write_strategy _int]))

