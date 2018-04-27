#lang racket
(require "hdf5.rkt")
(require ffi/unsafe)


(define FILE        "SDS.h5")
(define DATASETNAME "IntArray")
(define NX     5)
(define NY     6)
(define RANK   2)

(when #f
  (define data (for*/vector ([j NX]
                             [i NY])
                 (+ i j)))

  (define a-file (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

  (define dimsf (list NX NY))

  (define dataspace (H5Screate_simple RANK dimsf #f))

  (define datatype (H5Tcopy H5T_NATIVE_INT))

  (define status (H5Tset_order datatype 'H5T_ORDER_LE))

  (define dataset (H5Dcreate1 a-file DATASETNAME datatype dataspace H5P_DEFAULT))

  (define status2 (H5Dwrite dataset H5T_NATIVE_INT H5S_ALL H5S_ALL
                            H5P_DEFAULT (vector->cblock data _int)))

  (H5Sclose dataspace)
  (H5Tclose datatype)
  (H5Dclose dataset)
  (H5Fclose a-file))



;;(when #t
(define a-file (H5Fopen FILE H5F_ACC_RDONLY H5P_DEFAULT))
(define dataset (H5Dopen1 a-file DATASETNAME))
(define datatype (H5Dget_type dataset))     ;; datatype handle
(define class (H5Tget_class datatype))
(when (eq? class 'H5T_INTEGER)
  (printf "Data set has INTEGER type \n"))
(define order (H5Tget_order datatype))
(when (eq? order 'H5T_ORDER_LE)
  (printf "Little endian order \n"))

(define dataspace (H5Dget_space dataset))    ;; dataspace handle
(define rank (H5Sget_simple_extent_ndims dataspace))
(define status_n (H5Sget_simple_extent_dims dataspace))
(printf "rank ~a, dimensions ~a \n" rank status_n)

(define cparms (H5Dget_create_plist dataset)) ;; Get properties handle first.
;;(H5Pget_layout cparms)

(define memspace (H5Screate_simple RANK (vector-ref status_n 1) #f))

(define status2 (H5Dread dataset H5T_NATIVE_INT memspace dataspace
                         H5P_DEFAULT ))
