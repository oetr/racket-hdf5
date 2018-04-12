#|***********************************************************

  This example shows how to create and extend an unlimited
  dataset.  The program first writes integers to a dataset
  with dataspace dimensions of DIM0xDIM1, then closes the
  file.  Next, it reopens the file, reads back the data,
  outputs it to the screen, extends the dataset, and writes
  new data to the extended portions of the dataset.  Finally
  it reopens the file again, reads back the data, and
  outputs it to the screen.

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

(require "../unsafe/hdf5.rkt")

(define FILE            "./data/h5ex_d_unlimadd.h5")
(define DATASET         "DS1")
(define DIM0            4)
(define DIM1            7)
(define EDIM0           6)
(define EDIM1           10)
(define CHUNK0          4)
(define CHUNK1          4)

(define dims (list DIM0 DIM1))
(define extdims (list EDIM0 EDIM1))
(define chunk (list CHUNK0 CHUNK1))

#|
* Initialize data.
|#
(define wdata (build-array (vector DIM0 DIM1)
                        (lambda (indices)
                          (define i (vector-ref indices 0))
                          (define j (vector-ref indices 1))
                          (- (* i j) j))))

#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create dataspace with unlimited dimensions.
|#
(define maxdims (list H5S_UNLIMITED H5S_UNLIMITED))
(define space (H5Screate_simple 2 dims maxdims))

#|
* Create the dataset creation property list, and set the chunk
* size.
|#
(define dcpl (H5Pcreate H5P_DATASET_CREATE))
(define status (H5Pset_chunk dcpl chunk))

#|
* Create the unlimited dataset.
|#
(define dset (H5Dcreate fid DATASET H5T_STD_I32LE space H5P_DEFAULT dcpl
                        H5P_DEFAULT))

#|
* Write the data to the dataset.
|#
(set! status (H5Dwrite dset H5T_NATIVE_INT H5S_ALL H5S_ALL H5P_DEFAULT
                       (array->cblock wdata _int)))

#|
* Close and release resources.
|#
(set! status (H5Pclose dcpl))
(set! status (H5Dclose dset))
(set! status (H5Sclose space))
(set! status (H5Fclose fid))


#|
* In this next section we read back the data, extend the dataset,
* and write new data to the extended portions.
|#

#|
* Open file and dataset using the default properties.
|#
(set! fid (H5Fopen FILE H5F_ACC_RDWR H5P_DEFAULT))
(set! dset (H5Dopen fid DATASET H5P_DEFAULT))

#|
* Get dataspace and allocate memory for read buffer.  This is a
* two dimensional dataset so the dynamic allocation must be done
* in steps.
|#
(set! space (H5Dget_space dset))
(define ndims (H5Sget_simple_extent_ndims space))
(define all-dims (H5Sget_simple_extent_dims space))
(set! dims (vector-ref all-dims 1))



#|
* Allocate space for integer data.
|#
(define rdata-ptr (malloc (* (apply * dims) (ctype-sizeof _int))))

#|
* Read the data using the default properties.
|#
(set! status (H5Dread dset H5T_NATIVE_INT H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr))

(define rdata (vector->array (list->vector dims)
                             (cblock->vector rdata-ptr _int (apply * dims))))


#|
* Output the data to the screen.
|#
(printf "Dataset before extension:\n")
(for ([i (list-ref dims 0)])
  (printf " [")
  (for ([j (list-ref dims 1)])
    (printf "~a " (~a 
                   (array-ref rdata (vector i j))
                   #:width 3
                   #:pad-string " ")))
  (printf "]\n"))

(set! status (H5Sclose space))


#|
* Extend the dataset.
|#
(set! status (H5Dset_extent dset extdims))

#|
* Retrieve the dataspace for the newly extended dataset.
|#
(set! space (H5Dget_space dset))

#|
* Initialize data for writing to the extended dataset.
|#
(define wdata2 (build-array (vector EDIM0 EDIM1)
                        (lambda (indices)
                          (define i (vector-ref indices 0))
                          (define j (vector-ref indices 1))
                          j)))

#|
* Select the entire dataspace.
|#
(set! status (H5Sselect_all space))

#|
* Subtract a hyperslab reflecting the original dimensions from the
* selection.  The selection now contains only the newly extended
* portions of the dataset.
|#
(define start (list 0 0))
(define count dims)
(set! status (H5Sselect_hyperslab space 'H5S_SELECT_NOTB start '() count '()))

#|
* Write the data to the selected portion of the dataset.
|#
(set! status (H5Dwrite dset H5T_NATIVE_INT H5S_ALL space H5P_DEFAULT (array->cblock wdata2 _int)))

#|
* Close and release resources.
|#
(set! status (H5Dclose dset))
(set! status (H5Sclose space))
(set! status (H5Fclose fid))


#|
* Now we simply read back the data and output it to the screen.
|#

#|
* Open file and dataset using the default properties.
|#
(set! fid (H5Fopen FILE H5F_ACC_RDONLY H5P_DEFAULT))
(set! dset (H5Dopen fid DATASET H5P_DEFAULT))

#|
* Get dataspace and allocate memory for the read buffer as before.
|#
(set! space (H5Dget_space dset))
(set! ndims (H5Sget_simple_extent_ndims space))
(set! all-dims (H5Sget_simple_extent_dims space))
(set! dims (vector-ref all-dims 1))
(set! rdata-ptr (malloc (* (apply * dims) (ctype-sizeof _int))))

#|
* Read the data using the default properties.
|#
(set! status (H5Dread dset H5T_NATIVE_INT H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr))
                             

#|
* Output the data to the screen.
|#

(set! rdata (vector->array (list->vector dims)
                             (cblock->vector rdata-ptr _int (apply * dims))))


#|
* Output the data to the screen.
|#
(printf "Dataset after extension:\n")
(for ([i (list-ref dims 0)])
  (printf " [")
  (for ([j (list-ref dims 1)])
    (printf "~a " (~a 
                   (array-ref rdata (vector i j))
                   #:width 3
                   #:pad-string " ")))
  (printf "]\n"))

#|
* Close and release resources.
|#
(set! status (H5Dclose dset))
(set! status (H5Sclose space))
(set! status (H5Fclose fid))

