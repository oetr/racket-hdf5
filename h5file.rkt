#lang racket

(require math/array)
(require racket)
(require "hdf5.rkt"
         (only-in ffi/unsafe _int))

;; Creates or opens an HDF5 file.
(define (make-h5file name (mode #f) (userblock-size #f))
  (define file-access-plist (H5Pcreate H5P_FILE_ACCESS))
  (define fid (make-fid name mode userblock-size file-access-plist))
  fid)

(define (make-fid name mode userblock-size file-access-plist
                  #:fcpl (fcpl #f))
  (when userblock-size
    (when (or (eq? mode 'r)
              (eq? mode 'r+))
      (error 'make-fid "User block used when reading a file. (need mode w)"))
    
    (unless (integer? userblock-size)
      (error 'make-fid "User block size must be integer. (received: ~a~n)"
             userblock-size))

    (unless fcpl
      (set! fcpl (H5Pcreate H5P_FILE_CREATE)))

    (H5Pset_userblock fcpl userblock-size))

  (define fid #f)
  (unless fcpl (set! fcpl H5P_DEFAULT))

  (cond [(eq? mode 'r)
         (set! fid (H5Fopen name H5F_ACC_RDONLY file-access-plist))]
        [(eq? mode 'r+)
         (set! fid (H5Fopen name H5F_ACC_RDWR file-access-plist))]
        [(or (eq? mode 'w-)
             (eq? mode 'x))
         (set! fid (H5Fcreate name H5F_ACC_EXCL fcpl file-access-plist))]
        [(eq? mode 'w)
         (set! fid (H5Fcreate name H5F_ACC_TRUNC fcpl file-access-plist))]
        [(eq? mode 'a)
         (if (file-exists? name)
             (set! fid (H5Fopen name H5F_ACC_RDWR file-access-plist))
             (set! fid (H5Fcreate name H5F_ACC_EXCL fcpl file-access-plist)))]
        [(eq? mode #f) ;; TODO add exception handling
         (if (file-exists? name)
             (begin
               (printf "FILE exists\n")
               (set! fid (H5Fopen name H5F_ACC_RDWR file-access-plist)))
             
             (begin
               (printf "FILE does not exists : ~a, ~a\n" file-access-plist fcpl)
               (set! fid (H5Fcreate name H5F_ACC_EXCL fcpl file-access-plist))))]
        
        [else
         (error 'make-fid "mode must be one of (r, r+, w, w-, x, a)")])

  ;; try
  ;; (when userblock-size
  ;;   (set! existing-fcpl (fid.get_create_plist))
  ;;   (when (existing-fcpl.get_userblock() != userblock-size)
  ;;     (error 'make-fid "mode must be one of (r, r+, w, w-, x, a)")))
  ;; exept
  ;;(fid.close)

  fid)

(define (close-h5file fid)
  (H5Fclose fid))


(define a0 (build-array (vector 10000)
                        (lambda (indices) 
                          (for/fold ([sum 1]) ([i indices])
                            (+ sum i)))))
(define dims (array-shape a0))

(define raw-data (array->cblock a0 _int))


(define fid (make-h5file "./test.h5"))
(define dataspace-id (H5Screate_simple (sequence-length dims)
                                       dims
                                       #f))

(define dset-id (H5Dcreate2 fid "my dataset" H5T_STD_I32LE
                            dataspace-id H5P_DEFAULT
                            H5P_DEFAULT H5P_DEFAULT))

(H5Dwrite dset-id H5T_NATIVE_INT H5S_ALL H5S_ALL H5P_DEFAULT raw-data)

(H5Dclose dset-id)
(H5Sclose dataspace-id)
(H5Fclose fid)
