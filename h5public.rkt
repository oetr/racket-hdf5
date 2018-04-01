#lang racket
;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi)

(define hdf5-lib (ffi-lib "libhdf5_serial"))

(define-ffi-definer define-hdf5-lib-internal hdf5-lib)

(define-syntax define-hdf5
  (syntax-rules ()
    [(_ name body)
     (begin
       ;;(provide name)
       (define-hdf5-lib-internal name body))]))

(define _herr_t _int)
(define _hbool_t _uint)

(define-hdf5 H5open
  ;; error - Returns 0 if successful and -1 if fails
  (_fun
   -> (return-value : _herr_t)
   -> (if (not (zero? return-value))
          (error 'H5open "failed to initialize the library")
          return-value)))

;; Flushes all data to disk, closes all open identifiers, and cleans up memory.
(define-hdf5 H5close
  (_fun
   -> (return-value : _herr_t)
   -> (if (not (zero? return-value))
          (error 'H5close "failed to close the library")
          return-value)))

(define-hdf5 H5dont_atexit
  (_fun
   -> (return-value : _herr_t)
   -> (if (not (zero? return-value))
          (error 'H5dont_atexit "")
          return-value)))

(define-hdf5 H5garbage_collect
  (_fun
   -> (return-value : _herr_t)
   -> (if (not (zero? return-value))
          (error 'H5garbage_collect "")
          return-value)))

(define-hdf5 H5set_free_list_limits
  (_fun _int _int _int _int _int _int
   -> (return-value : _herr_t)
   -> (if (not (zero? return-value))
          (error 'H5set_free_list_limits "")
          return-value)))

(define-hdf5 H5get_libversion
  (_fun () ::
        [majnum : (_ptr o _uint)]
        [minnum : (_ptr o _uint)]
        [relnum : (_ptr o _uint)]
        -> (status : _herr_t)
        -> (list majnum minnum relnum)))

(H5get_libversion)

(define-hdf5 H5check_version
  (_fun (majnum : _uint)
        (minnum : _uint)
        (relnum : _uint)
        -> (status : _herr_t)
        -> (unless (zero? status)
             (error 'H5check_version "incompatible versions"))))

(H5check_version 1 2 3)



(define-hdf5 H5is_library_threadsafe
  (_fun () ::
        [is_ts : (_ptr o _hbool_t)]
        -> (status : _herr_t)
        -> (= is_ts 1)))

(H5is_library_threadsafe)

(define-hdf5 H5allocate_memory
  (_fun (size clear) ::
        [size : _size]
        [clear : _hbool_t = (if clear 1 0)]
        -> (mem : _pointer)
        -> (if mem
               mem
               (begin
                 (printf "WARNING: H5allocate_memory allocated a NULL\n")
                 mem))))

(H5allocate_memory 10 #t)
(H5allocate_memory 0 #t)

(define-hdf5 H5free_memory
  (_fun (mem : _pointer)
        -> (status : _herr_t)
        -> (when (< status 0)
             (error 'H5free_memory "unable to free memory"))))

(H5free_memory (H5allocate_memory 10 #t))

(define-hdf5 H5resize_memory
  (_fun (mem : _pointer)
        (size : _size)
        -> _pointer))

(H5open)
(H5close)


(define-hdf5 H5resize_memory
  (_fun (mem : _pointer)
        (size : _size)
        -> _pointer))

