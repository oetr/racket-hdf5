#|***********************************************************

This example shows how to read and write opaque datatypes
to a dataset.  The program first writes opaque data to a
dataset with a dataspace of DIM0, then closes the file.
Next, it reopens the file, reads back the data, and
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
         math/array)

(require hdf5/unsafe)

(define FILE            "./data/h5ex_t_opaque.h5")
(define DATASET         "DS1")
(define DIM0            4)
(define LEN             7)


(define dims (list DIM0))
(define str "OPAQUE")

#|
* Initialize data.
|#
(define wdata (apply string-append
                     (for/list ([i DIM0])
                       (~a str i))))

#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create opaque datatype and set the tag to something appropriate.
* For this example we will write and view the data as a character
* array.
|#
(define dtype (H5Tcreate 'H5T_OPAQUE LEN))
(H5Tset_tag dtype "Character array")

#|
* Create dataspace.  Setting maximum size to NULL sets the maximum
* size to be the current size.
|#
(define space (H5Screate_simple 1 dims #f))

#|
* Create the dataset and write the opaque data to it.
|#
(define dset (H5Dcreate fid DATASET dtype space H5P_DEFAULT H5P_DEFAULT
                        H5P_DEFAULT))


(H5Dwrite dset dtype H5S_ALL H5S_ALL H5P_DEFAULT
          (cast wdata _string _pointer))

    #|
     * Close and release resources.
     |#
(H5Dclose dset)
(H5Sclose space)
(H5Tclose dtype)
(H5Fclose fid)

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
* Get datatype and properties for the datatype.  Note that H5Tget_tag
* allocates space for the string in tag, so we must remember to H5free_memory() it
* later.
|#
(set! dtype (H5Dget_type dset))
(define len (H5Tget_size dtype))
(define tag (H5Tget_tag dtype))

#|
* Get dataspace and allocate memory for read buffer.
|#
(set! space (H5Dget_space dset))
(define ndims (H5Sget_simple_extent_ndims space))
(define all-dims (H5Sget_simple_extent_dims space))
(set! dims (vector-ref all-dims 1))
(define rdata-ptr (malloc (+ 1 (* (apply * dims) len))))
(ptr-set! rdata-ptr _ubyte (+ (* (apply * dims) len)) 0)

#|
* Read the data.
|#
(H5Dread dset dtype H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr)
(define rdata (cast rdata-ptr _pointer _string))

#|
* Output the data to the screen.
|#
(printf "Datatype tag for ~s is: ~s\n" DATASET tag)
(for ([i (list-ref dims 0)])
  (printf "~s[~a]: " DATASET i)
  (printf "~a~n" (substring rdata (* i len) (* (+ i 1) len))))

#|
* Close resources.
|#
(H5Dclose dset)
(H5Sclose space)
(H5Tclose dtype)
(H5Fclose fid)
