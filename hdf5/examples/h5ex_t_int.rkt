#|***********************************************************
This example shows how to read and write integer datatypes
to a dataset.  The program first writes integers to a
dataset with a dataspace of DIM0xDIM1, then closes the
file.  Next, it reopens the file, reads back the data, and
outputs it to the screen.

This file is intended for use with HDF5 Library version 1.8
***********************************************************|#
#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit)

(require hdf5/unsafe)

(define FILE    "./data/h5ex_t_int.h5")
(define DATASET "DS1")
(define DIM0    4)
(define DIM1    7)

(when #t
  (define status #f)
  (define dims (list DIM0 DIM1))
  (define wdata (for*/list ([i DIM0]
                            [j DIM1])
                  (- (* i j) j)))

  (define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

  (define space (H5Screate_simple 2 dims #f))
  (define dset (H5Dcreate2 fid DATASET H5T_STD_I64BE space H5P_DEFAULT
                           H5P_DEFAULT H5P_DEFAULT))
  (set! status (H5Dwrite dset H5T_NATIVE_INT H5S_ALL H5S_ALL H5P_DEFAULT
                         (list->cblock wdata _int)))

  (set! status (H5Dclose dset))
  (set! status (H5Sclose space))
  (set! status (H5Fclose fid)))


(when #t
  (define dims (list DIM0 DIM1))


  (define fid (H5Fopen FILE H5F_ACC_RDONLY H5P_DEFAULT))
  (define dset (H5Dopen2 fid DATASET H5P_DEFAULT))

  #|
  * Get dataspace and allocate memory for read buffer.  This is a
  * two dimensional dataset so the dynamic allocation must be done
  * in steps.
  |#
  (define space (H5Dget_space dset))
  (define ndims (H5Sget_simple_extent_ndims space))
  (define all-dims (H5Sget_simple_extent_dims space))
  (define dims-read (vector-ref all-dims 1))
  (define rdata (malloc _int (apply * dims-read) 'atomic))

  ;; Read the data.
  (define status (H5Dread dset H5T_NATIVE_INT H5S_ALL H5S_ALL H5P_DEFAULT
                          rdata))

  (define data (cblock->list rdata _int (apply * dims-read)))
  (pretty-print data)

  (H5Dclose dset)
  (H5Sclose space)
  (H5Fclose fid))
