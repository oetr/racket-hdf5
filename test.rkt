#lang racket
(require "hdf5.rkt")

(define FILE        "SDSextendible.h5")
(define DATASETNAME "ExtendibleArray")
(define RANK         2)
(define NX     10)
(define NY     5)

(define f (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))
(define cparms (H5Pcreate H5P_DATASET_CREATE))
