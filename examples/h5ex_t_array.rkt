#|***********************************************************

  This example shows how to read and write array datatypes
  to a dataset.  The program first writes integers arrays of
  dimension ADIM0xADIM1 to a dataset with a dataspace of
  DIM0, then closes the  file.  Next, it reopens the file,
  reads back the data, and outputs it to the screen.

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

(define FILE            "./data/h5ex_t_array.h5")
(define DATASET         "DS1")
(define DIM0            4)
(define ADIM0           3)
(define ADIM1           5)


#|
* Initialize data.  i is the element in the dataspace, j and k the
* elements within the array datatype.
|#
(define wdata (build-array (vector DIM0 ADIM0 ADIM1)
                        (lambda (indices)
                          (define i (vector-ref indices 0))
                          (define j (vector-ref indices 1))
                          (define k (vector-ref indices 2))
                          (+ (* i j) (- (* j k)) (* i k)))))

(define dims (vector (vector-ref (array-shape wdata) 0)))
(define adims (vector-drop (array-shape wdata) 1))

#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create array datatypes for file and memory.
|#
(define filetype (H5Tarray_create H5T_STD_I64LE adims))
(define memtype (H5Tarray_create H5T_NATIVE_INT adims))

    #|
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     |#
(define space (H5Screate_simple dims #f))

#|
* Create the dataset and write the array data to it.
|#
(define dset (H5Dcreate fid DATASET filetype space H5P_DEFAULT H5P_DEFAULT
                        H5P_DEFAULT))

(H5Dwrite dset memtype H5S_ALL H5S_ALL H5P_DEFAULT (array->cblock wdata _int))

    #|
     * Close and release resources.
     |#
(H5Dclose dset)
(H5Sclose space)
(H5Tclose filetype)
(H5Tclose memtype)
(H5Fclose fid)


#|
* Now we begin the read section of this example.  Here we assume
* the dataset and array have the same name and rank, but can have
* any size.  Therefore we must allocate a new array to read in
* data using malloc().
|#

#|
* Open file and dataset.
|#
(set! fid (H5Fopen FILE H5F_ACC_RDONLY H5P_DEFAULT))
(set! dset (H5Dopen fid DATASET H5P_DEFAULT))

#|
* Get the datatype and its dimensions.
|#
(set! filetype (H5Dget_type dset))
(set! adims (H5Tget_array_dims filetype))

#|
* Get dataspace and allocate memory for read buffer.  This is a
* three dimensional dataset when the array datatype is included so
* the dynamic allocation must be done in steps.
|#
(set! space (H5Dget_space dset))
(define all-dims (H5Sget_simple_extent_dims space))
(define ndims (vector-ref all-dims 0))
(set! dims (vector-ref all-dims 1))



#|
* Allocate space for integer data.
|#
(define rdata-ptr (malloc (* (apply * (append dims adims)) (ctype-sizeof _int))))
(memset rdata-ptr 0 (* (apply * (append dims adims)) (ctype-sizeof _int)))


    #|
     * Create the memory datatype.
     |#
(set! memtype (H5Tarray_create H5T_NATIVE_INT adims))

    #|
     * Read the data.
     |#
(H5Dread dset memtype H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr)

(define rdata (vector->array (list->vector (append dims adims))
                             (cblock->vector rdata-ptr _int (apply * (append dims adims)))))

#|
* Output the data to the screen.
|#
(pretty-print rdata)

#|
* Close and release resources.
|#
(H5Dclose dset)
(H5Sclose space)
(H5Tclose filetype)
(H5Tclose memtype)
(H5Fclose fid)

