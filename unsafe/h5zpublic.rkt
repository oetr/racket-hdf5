#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt")

(provide (all-defined-out))

(define H5Z_filter_t _int)

;; Filter IDs */
(define H5Z_FILTER_ERROR       -1) ;; no filter
(define H5Z_FILTER_NONE         0) ;; reserved indefinitely
(define H5Z_FILTER_DEFLATE      1) ;; deflation like gzip     
(define H5Z_FILTER_SHUFFLE      2) ;; shuffle the data              
(define H5Z_FILTER_FLETCHER32   3) ;; fletcher32 checksum of EDC    
(define H5Z_FILTER_SZIP         4) ;; szip compression              
(define H5Z_FILTER_NBIT         5) ;; nbit compression              
(define H5Z_FILTER_SCALEOFFSET  6) ;; scale+offset compression      
(define H5Z_FILTER_RESERVED   256) ;; filter ids below this value are reserved for library use 

(define H5Z_FILTER_MAX       65535) ;; maximum filter id

;; General macros 
(define H5Z_FILTER_ALL           0);; Symbol to remove all filters in H5Premove_filter 
(define H5Z_MAX_NFILTERS        32)      ;; Maximum number of filters allowed in a pipeline 
#| (should probably be allowed to be an
* unlimited amount, but currently each
* filter uses a bit in a 32-bit field,
* so the format would have to be
* changed to accomodate that)
|#

;; Flags for filter definition (stored) 
(define H5Z_FLAG_DEFMASK	#x00ff)	;;definition flag mask		
(define H5Z_FLAG_MANDATORY      #x0000)  ;;filter is mandatory		
(define H5Z_FLAG_OPTIONAL	#x0001)	;;filter is optional		

;; Additional flags for filter invocation (not stored) 
(define H5Z_FLAG_INVMASK	#xff00)	;;invocation flag mask		
(define H5Z_FLAG_REVERSE	#x0100)	;;reverse direction; read	
(define H5Z_FLAG_SKIP_EDC	#x0200)	;;skip EDC filters for read	

;; Special parameters for szip compression 
#| [These are aliases for the similar definitions in szlib.h, which we can't
* include directly due to the duplication of various symbols with the zlib.h
* header file] |#
(define H5_SZIP_ALLOW_K13_OPTION_MASK   1)
(define H5_SZIP_CHIP_OPTION_MASK        2)
(define H5_SZIP_EC_OPTION_MASK          4)
(define H5_SZIP_NN_OPTION_MASK          32)
(define H5_SZIP_MAX_PIXELS_PER_BLOCK    32)

;; Macros for the shuffle filter 
(define H5Z_SHUFFLE_USER_NPARMS    0)    ;; Number of parameters that users can set 
(define H5Z_SHUFFLE_TOTAL_NPARMS   1)    ;; Total number of parameters for filter 

;; Macros for the szip filter 
(define H5Z_SZIP_USER_NPARMS    2)       ;; Number of parameters that users can set 
(define H5Z_SZIP_TOTAL_NPARMS   4)       ;; Total number of parameters for filter 
(define H5Z_SZIP_PARM_MASK      0)       ;; "User" parameter for option mask 
(define H5Z_SZIP_PARM_PPB       1)       ;; "User" parameter for pixels-per-block 
(define H5Z_SZIP_PARM_BPP       2)       ;; "Local" parameter for bits-per-pixel 
(define H5Z_SZIP_PARM_PPS       3)       ;; "Local" parameter for pixels-per-scanline 

;; Macros for the nbit filter 
(define H5Z_NBIT_USER_NPARMS     0)     ;; Number of parameters that users can set 

;; Macros for the scale offset filter 
(define H5Z_SCALEOFFSET_USER_NPARMS      2)    ;; Number of parameters that users can set 


;; Special parameters for ScaleOffset filter
(define H5Z_SO_INT_MINBITS_DEFAULT 0)
(define+provide H5Z_SO_scale_type_t
  (_enum
   '(
     H5Z_SO_FLOAT_DSCALE = 0
     H5Z_SO_FLOAT_ESCALE = 1
     H5Z_SO_INT          = 2
     )))
;; Current version of the H5Z_class_t struct 
(define H5Z_CLASS_T_VERS 1)

;; Values to decide if EDC is enabled for reading data
(define+provide H5Z_EDC_t
  (_enum
   '(
     H5Z_ERROR_EDC       = -1   ;; error value 
     H5Z_DISABLE_EDC     = 0
     H5Z_ENABLE_EDC      = 1
     H5Z_NO_EDC          = 2     ;; must be the last 
     )))
;; Bit flags for H5Zget_filter_info 
(define H5Z_FILTER_CONFIG_ENCODE_ENABLED #x0001)
(define H5Z_FILTER_CONFIG_DECODE_ENABLED #x0002)

;; Return values for filter callback function
(define+provide H5Z_cb_return_t
  (_enum
   '(
     H5Z_CB_ERROR  = -1
     H5Z_CB_FAIL   = 0    ;; I/O should fail if filter fails. 
     H5Z_CB_CONT   = 1    ;; I/O continues if filter fails.   
     H5Z_CB_NO     = 2
     )))

;; Filter callback function definition
(define H5Z_filter_func_t
  (_fun (filter : H5Z_filter_t)
        (buf : _pointer)
        (buf_size : _size)
        (op_data : _pointer)
        -> H5Z_cb_return_t))

;; Structure for filter callback property
(define-cstruct _H5Z_cb_t
  ([func H5Z_filter_func_t]
   [op_data _pointer]))


;; typedef htri_t (*H5Z_can_apply_func_t)(hid_t dcpl_id, hid_t type_id, hid_t space_id);

(define H5Z_can_apply_func_t
  (_fun (dcpl_id : hid_t)
        (type_id : hid_t)
        (space_id : hid_t)
        -> htri_t))

(define H5Z_set_local_func_t
  (_fun (dcpl_id : hid_t)
        (type_id : hid_t)
        (space_id : hid_t)
        -> herr_t))


(define H5Z_func_t
  (_fun (flags : _uint)
        (cd_nelmts : _size)
        (cd_values : _pointer) ;; TODO: const array
        (nbytes : _size)
        (buf_size : _size)
        (buf : _pointer)
        -> _size))

#|
* The filter table maps filter identification numbers to structs that
* contain a pointers to the filter function and timing statistics.
|#
(define-cstruct _H5Z_class2_t
  ([version _int]                ;; Version number of the H5Z_class_t struct
   [id H5Z_filter_t]		;; Filter ID number			 
   [encoder_present _uint]   ;; Does this filter have an encoder? 
   [decoder_present _uint]   ;; Does this filter have a decoder? 
   [name _string]		;; Comment for debugging		     
   [can_apply H5Z_can_apply_func_t] ;; The "can apply" callback for a filter 
   [set_local H5Z_set_local_func_t] ;; The "set local" callback for a filter 
   [filter H5Z_func_t])) ;; The actual filter function		     


(define-hdf5 H5Zregister
  (_fun (cls : _pointer)
        -> herr_t))

(define-hdf5 H5Zunregister
  (_fun (id : H5Z_filter_t)
        -> herr_t))

(define-hdf5 H5Zfilter_avail
  (_fun (id : H5Z_filter_t)
        -> (status : htri_t)
        -> (cond [(> status 0) #t]
                 [(= status 0) #f]
                 [else (error 'H5Zfilter_avail "Failed to check filter availability for id: ~a\n"
                              id)])))
        
(define-hdf5 H5Zget_filter_info
  (_fun (filter : H5Z_filter_t)
        (filter_config_flags : (_ptr o _uint))
        -> (status : herr_t)
        -> (if (< status 0)
               (error 'H5Zget_filter_info "Failed to get filter info, for filter ~a~n" filter)
               filter_config_flags)))

#|
 * The filter table maps filter identification numbers to structs that
 * contain a pointers to the filter function and timing statistics.
|#
(define-cstruct _H5Z_class1_t
  ([id H5Z_filter_t] ;; Filter ID number
   [name _string] ;; Comment for debugging
   [can_apply H5Z_can_apply_func_t] ;; The "can apply" callback for a filter
   [set_local H5Z_set_local_func_t] ;; The "set local" callback for a filter
   [filter H5Z_func_t])) ;; The actual filter function     
