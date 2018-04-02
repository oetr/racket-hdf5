#lang racket
(require "hdf5.rkt")


(define FILE        "SDS.h5")
(define DATASETNAME "IntArray" )
(define NX     5)
(define NY     6)
(define RANK   2)

(for/list ([j NX])
  (for/list ([i NY])
    (+ i j)))

(define a-file (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

(define dimsf (list NX NY))

(define dataspace (H5Screate_simple RANK dimsf #f))


H5T_NATIVE_INT8_g

