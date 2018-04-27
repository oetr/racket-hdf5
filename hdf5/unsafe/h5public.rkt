#lang racket
;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt")

(provide (all-defined-out))

(define hsize_t _ulong)
(define hssize_t _long)
(define haddr_t _ulong)

#|
 * Status return values.  Failed integer functions in HDF5 result almost
 * always in a negative value (unsigned failing functions sometimes return
 * zero for failure) while successfull return is non-negative (often zero).
 * The negative failure value is most commonly -1, but don't bet on it.  The
 * proper way to detect failure is something like:
 *
 * 	if((dset = H5Dopen2(file, name)) < 0)
 *	    fprintf(stderr, "unable to open the requested dataset\n");
|#
(define herr_t _int)

;; Type of atoms to return to users
(define hid_t _int)


#|
 * Boolean type.  Successful return values are zero (false) or positive
 * (true). The typical true value is 1 but don't bet on it.  Boolean
 * functions cannot fail.  Functions that return `htri_t' however return zero
 * (false), positive (true), or negative (failure). The proper way to test
 * for truth from a htri_t function is:
 *
 * 	if ((retval = H5Tcommitted(type))>0) {
 *	    printf("data type is committed\n");
 *	} else if (!retval) {
 * 	    printf("data type is not committed\n");
 *	} else {
 * 	    printf("error determining whether data type is committed\n");
 *	}
|#
(define hbool_t _uint)
(define htri_t _int)


;; Common iteration orders
(define H5_iter_order_t
  (_enum '(
           H5_ITER_UNKNOWN = -1 ;; Unknown order
           H5_ITER_INC          ;; Increasing order
           H5_ITER_DEC          ;; Decreasing order
           H5_ITER_NATIVE       ;; No particular order, whatever is fastest
           H5_ITER_N)))	        ;; Number of iteration orders

;; Iteration callback values
#| (Actually, any postive value will cause the iterator to stop and pass back
 *      that positive value to the function that called the iterator)
 |#
(define H5_ITER_ERROR -1)
(define H5_ITER_CONT   0)
(define H5_ITER_STOP   1)



#|
 * The types of indices on links in groups/attributes on objects.
 * Primarily used for "<do> <foo> by index" routines and for iterating over
 * links in groups/attributes on objects.
|#
(define H5_index_t
  (_enum '(
    H5_INDEX_UNKNOWN = -1 ;; Unknown index type
    H5_INDEX_NAME         ;; Index on names
    H5_INDEX_CRT_ORDER    ;; Index on creation order
    H5_INDEX_N)))         ;; Number of indices defined


#|
 * Storage info struct used by H5O_info_t and H5F_info_t
|#

(define-cstruct _H5_ih_info_t
  ([oper hsize_t] ;; btree and/or list
   [index hsize_t]))


(define-hdf5 H5open
  ;; error - Returns 0 if successful and -1 if fails
  (_fun
   -> (return-value : herr_t)
   -> (if (not (zero? return-value))
          (error 'H5open "failed to initialize the library")
          return-value)))

;; Flushes all data to disk, closes all open identifiers, and cleans up memory.
(define-hdf5 H5close
  (_fun
   -> (return-value : herr_t)
   -> (if (not (zero? return-value))
          (error 'H5close "failed to close the library")
          return-value)))

(define-hdf5 H5dont_atexit
  (_fun
   -> (return-value : herr_t)
   -> (if (not (zero? return-value))
          (error 'H5dont_atexit "")
          return-value)))

(define-hdf5 H5garbage_collect
  (_fun
   -> (return-value : herr_t)
   -> (if (not (zero? return-value))
          (error 'H5garbage_collect "")
          return-value)))

(define-hdf5 H5set_free_list_limits
  (_fun _int _int _int _int _int _int
   -> (return-value : herr_t)
   -> (if (not (zero? return-value))
          (error 'H5set_free_list_limits "")
          return-value)))

(define-hdf5 H5get_libversion
  (_fun () ::
        [majnum : (_ptr o _uint)]
        [minnum : (_ptr o _uint)]
        [relnum : (_ptr o _uint)]
        -> (status : herr_t)
        -> (list majnum minnum relnum)))


(define-hdf5 H5check_version
  (_fun (majnum : _uint)
        (minnum : _uint)
        (relnum : _uint)
        -> (status : herr_t)
        -> (unless (zero? status)
             (error 'H5check_version "incompatible versions"))))


(define-hdf5 H5is_library_threadsafe
  (_fun () ::
        [is_ts : (_ptr o hbool_t)]
        -> (status : herr_t)
        -> (= is_ts 1)))

(define-hdf5 H5allocate_memory
  (_fun (size clear) ::
        [size : _size]
        [clear : hbool_t = (if clear 1 0)]
        -> (mem : _pointer)
        -> (if mem
               mem
               (begin
                 (printf "WARNING: H5allocate_memory allocated a NULL\n")
                 mem))))

(define-hdf5 H5free_memory
  (_fun (mem : _pointer)
        -> (status : herr_t)
        -> (when (< status 0)
             (error 'H5free_memory "unable to free memory"))))

(define-hdf5 H5resize_memory
  (_fun (mem : _pointer)
        (size : _size)
        -> _pointer))

(module+ test
  (H5get_libversion)
  (H5check_version 1 2 3)
  (H5is_library_threadsafe)
  (H5allocate_memory 10 #t)
  (H5allocate_memory 0 #t)
  (H5free_memory (H5allocate_memory 10 #t))
  (H5open)
  (H5close))
