#lang racket

(require math/array
         (only-in ffi/unsafe _int)
         "../unsafe/hdf5.rkt"
         "../hl/h5file.rkt")


(define a0 (build-array (vector 1000)
                        (lambda (indices) 
                          (for/fold ([sum 1]) ([i indices])
                            (+ sum i (* i (random 100)))))))
(define dims (array-shape a0))

(define raw-data (array->cblock a0 _int))


(define fid (make-h5file (expand-user-path "~/test.h5")))


(define dataspace-id (H5Screate_simple (sequence-length dims)
                                       dims
                                       #f))

(define dset-id (make-dataset fid "my dataset" H5T_STD_I64LE
                              dataspace-id H5P_DEFAULT
                              H5P_DEFAULT H5P_DEFAULT))

(H5Dwrite dset-id H5T_NATIVE_INT H5S_ALL H5S_ALL H5P_DEFAULT raw-data)

(H5Dclose dset-id)
(H5Sclose dataspace-id)
(H5Fclose fid)
