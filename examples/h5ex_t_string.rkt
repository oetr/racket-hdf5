#lang racket

(require (rename-in ffi/unsafe
                    (in-array ffi-in-array)
                    (array? ffi-array?)
                    (array-set! ffi-array-set!)
                    (array-ref ffi-array-ref))
         ffi/unsafe/define
         rackunit
         math/array)

(require "../unsafe/hdf5.rkt")



#|***********************************************************

This example shows how to read and write string datatypes
to a dataset.  The program first writes strings to a
dataset with a dataspace of DIM0, then closes the file.
Next, it reopens the file, reads back the data, and
outputs it to the screen.

This file is intended for use with HDF5 Library version 1.8

***********************************************************|#


(define FILE            "./data/h5ex_t_string.h5")
(define DATASET         "DS1")
(define DIM0            4)
(define SDIM            8)


(define wdata (list "Parting" "is such" "sweet" "sorrow."))
(define dims (list (length wdata)))

#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create file and memory datatypes.  For this example we will save
* the strings as FORTRAN strings, therefore they do not need space
* for the null terminator in the file.
|#
(define filetype (H5Tcopy H5T_FORTRAN_S1))
(define status (H5Tset_size filetype (- SDIM 1)))
(define memtype (H5Tcopy H5T_C_S1))
(set! status (H5Tset_size memtype SDIM))

#|
* Create dataspace.  Setting maximum size to NULL sets the maximum
* size to be the current size.
|#
(define space (H5Screate_simple 1 dims #f))

;; convert strings into char* array
(define block (malloc 'atomic (* DIM0 SDIM)))
(memset block 0 (* DIM0 SDIM) _ubyte)
(for ([str wdata]
      [i (length wdata)])
  (define ptr (cast str _string _pointer))
  (memcpy block (* i SDIM) ptr (string-length str) _ubyte))

#|
* Create the dataset and write the string data to it.
|#
(define dset (H5Dcreate fid DATASET filetype space H5P_DEFAULT H5P_DEFAULT
                        H5P_DEFAULT))

(set! status (H5Dwrite dset memtype H5S_ALL H5S_ALL H5P_DEFAULT
                       block))

;; (set! status (H5Dwrite dset memtype H5S_ALL H5S_ALL H5P_DEFAULT
;;                        (list->cblock (map (lambda (str)
;;                                             (cast str _string _pointer))
;;                                           wdata)
;;                                      _pointer)))

#|
* Close and release resources.
|#
(set! status (H5Dclose dset))
(set! status (H5Sclose space))
(set! status (H5Tclose filetype))
(set! status (H5Tclose memtype))
(set! status (H5Fclose fid))


#|
* Now we begin the read section of this example.  Here we assume
* the dataset and string have the same name and rank, but can have
* any size.  Therefore we must allocate a new array to read in
* data using malloc().
|#

#|
* Open file and dataset.
|#
(set! fid (H5Fopen FILE H5F_ACC_RDONLY H5P_DEFAULT))
(set! dset (H5Dopen fid DATASET H5P_DEFAULT))

#|
* Get the datatype and its size.
|#
(set! filetype (H5Dget_type dset))
(define sdim (+ 1 (H5Tget_size filetype))) #| Make room for null terminator |#


#|
* Get dataspace and allocate memory for read buffer.  This is a
* two dimensional dataset so the dynamic allocation must be done
* in steps.
|#
(set! space (H5Dget_space dset))
(define ndims (H5Sget_simple_extent_ndims space))
(define all-dims (H5Sget_simple_extent_dims space))

#|
* Allocate array of pointers to rows.
|#

#|
* Allocate space for integer data.
|#
(define rdata-ptr (malloc (* (apply * dims) sdim (ctype-sizeof _ubyte))))

#|
* Set the rest of the pointers to rows to the correct addresses.
|#

#|
* Create the memory datatype.
|#
(set! memtype (H5Tcopy H5T_C_S1))
(set! status (H5Tset_size memtype sdim))

     #|
     * Read the data.
     |#
(set! status (H5Dread dset memtype H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr))
;;(define rdata (cblock->list rdata-ptr _bytes 4))
;; several ways to get strings back into array
;; 1) convert the cblock into list, and then convert into bytes -> strings
;;(define rdata (cblock->list rdata-ptr _byte (* (apply * dims) sdim)))
;;(apply bytes rdata)

;; 2) get a string from each pointer
(define rdata
  (for/list ([offset (apply * dims)])
    (cast (ptr-add rdata-ptr offset _pointer) _pointer _string)))

#|
* Output the data to the screen.
|#
 (for ([i (length rdata)]
       [str rdata])
   (printf "~a[~a]: ~s\n" DATASET i str))

#|
* Close and release resources.
|#

 (set! status (H5Dclose dset))
 (set! status (H5Sclose space))
 (set! status (H5Tclose filetype))
 (set! status (H5Tclose memtype))
 (set! status (H5Fclose fid))
