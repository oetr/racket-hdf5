#lang racket

(require racket)
(require "../unsafe/hdf5.rkt")

(provide (all-defined-out))

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


(define (make-dataset file-id name type space-id lcpl-id dcpl-id dapl-id)
  ;; open and check whether it exists already
  (define status (H5Dopen2 file-id name dapl-id))

  ;; extend
  (if (>= status 0)
      (begin
        (printf "Opening existing dataset: ~s~n" name)
        status)
      (H5Dcreate2 file-id name type space-id lcpl-id dcpl-id dapl-id)))
