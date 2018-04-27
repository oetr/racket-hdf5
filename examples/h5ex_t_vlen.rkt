#|***********************************************************

This example shows how to read and write variable-length
datatypes to a dataset.  The program first writes two
variable-length integer arrays to a dataset then closes
the file.  Next, it reopens the file, reads back the data,
and outputs it to the screen.

This file is intended for use with HDF5 Library version 1.8

***********************************************************|#

#lang racket


(require (rename-in ffi/unsafe
                    (in-array ffi-in-array)
                    (array? ffi-array?)
                    (array-set! ffi-array-set!)
                    (array-ref ffi-array-ref))
         ffi/unsafe/define
         rackunit
         math/array
         )

(require hdf5/unsafe)

(define FILE            "./data/h5ex_t_vlen.h5")
(define DATASET         "DS1")
(define LEN0            3)
(define LEN1            10)

(define dims (list 2))


(define (fibo n)
  (define result (make-vector n 0))
  (define (loop! i i-1 i-2)
    (unless (= i n)
      (define next-val (+ i-1 i-2))
      (vector-set! result i next-val)
      (loop! (+ i 1) next-val i-1)))
  (loop! 0 0 1)
  result)


#|
* Initialize variable-length data.  wdata[0] is a countdown of
* length LEN0, wdata[1] is a Fibonacci sequence of length LEN1.
|#
(define wdata0 (build-vector LEN0 (lambda (n) (- LEN0 n))))
(define wdata1 (fibo LEN1))

(define wdata (list (make-hvl_t LEN0 (vector->cblock wdata0 _uint64 LEN0))
                    (make-hvl_t LEN1 (vector->cblock wdata1 _uint64 LEN1))))


#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create variable-length datatype for file and memory.
|#
(define filetype (H5Tvlen_create H5T_STD_I64LE))
(define memtype (H5Tvlen_create H5T_NATIVE_INT64))


#|
* Create dataspace.  Setting maximum size to NULL sets the maximum
* size to be the current size.
|#
(define space (H5Screate_simple 1 dims #f))

#|
* Create the dataset and write the variable-length data to it.
|#
(define dset (H5Dcreate fid DATASET filetype space H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT))
(define status (H5Dwrite dset memtype H5S_ALL H5S_ALL H5P_DEFAULT (list->cblock wdata _hvl_t 2)))

#|
* Close and release resources.  Note the use of H5Dvlen_reclaim
* removes the need to manually free() the previously malloc'ed
* data.
|#
;;(set! status (H5Dvlen_reclaim memtype space H5P_DEFAULT ptr-data))
(set! wdata #f)
(set! status (H5Dclose dset))
(set! status (H5Sclose space))
(set! status (H5Tclose filetype))
(set! status (H5Tclose memtype))
(set! status (H5Fclose fid))




#|
* Now we begin the read section of this example.  Here we assume
* the dataset has the same name and rank, but can have any size.
* Therefore we must allocate a new array to read in data using
* malloc().
|#

#|
* Open file and dataset.
|#
(set! fid (H5Fopen FILE H5F_ACC_RDONLY H5P_DEFAULT))
(set! dset (H5Dopen fid DATASET H5P_DEFAULT))

#|
* Get dataspace and allocate memory for array of vlen structures.
* This does not actually allocate memory for the vlen data, that
* will be done by the library.
|#
(set! space (H5Dget_space dset))
(define ndims (H5Sget_simple_extent_ndims space))
(set! dims (vector-ref (H5Sget_simple_extent_dims space) 1))

(define rdata (malloc (* (apply * dims) (ctype-sizeof _hvl_t))))

#|
* Create the memory datatype.
|#
(set! memtype (H5Tvlen_create H5T_NATIVE_INT64))

#|
* Read the data.
|#
(set! status (H5Dread dset memtype H5S_ALL H5S_ALL H5P_DEFAULT rdata))

(define rdata-converted (cblock->list rdata _hvl_t (apply * dims)))

#|
* Output the variable-length data to the screen.
|#
(for ([i (apply * dims)]
      [data rdata-converted])
  (printf "~s[~a]:\n {" DATASET i)
  (define len (hvl_t-len data))
  (for ([j len]
        [d (cblock->vector (hvl_t-p data) _uint64 len)])
    (printf " ~a" d)
    (when (< (+ j 1) len)
      (printf ",")))
  (printf " }\n"))


#|
* Close and release resources.  Note we must still free the
* top-level pointer "rdata", as H5Dvlen_reclaim only frees the
* actual variable-length data, and not the structures themselves.
|#
(set! status (H5Dvlen_reclaim memtype space H5P_DEFAULT rdata))
(set! status (H5Dclose dset))
(set! status (H5Sclose space))
(set! status (H5Tclose memtype))
(set! status (H5Fclose fid))
