#|***********************************************************
This example shows how to read and write enumerated
datatypes to a dataset.  The program first writes
enumerated values to a dataset with a dataspace of
DIM0xDIM1, then closes the file.  Next, it reopens the
file, reads back the data, and outputs it to the screen.

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

(require "../unsafe/main.rkt")

(define FILE            "./data/h5ex_t_enum.h5")
(define DATASET         "DS1")
(define DIM0            4)
(define DIM1            7)
(define F_BASET         H5T_STD_I16BE)       ;; File base type
(define M_BASET         H5T_NATIVE_INT)      ;; Memory base type
(define NAME_BUF_SIZE   16)

;; enumerated type
(define phase_t
  (_enum '(SOLID
           LIQUID
           GAS
           PLASMA)))

(define names (list "SOLID" "LIQUID" "GAS" "PLASMA"))

(define status #f)
(define dims (list DIM0 DIM1))


#|
* Initialize data.
|#
(define wdata
  (for*/vector ([i DIM0]
              [j DIM1])
    (cast (modulo (- (* (+ i 1) j) j) (cast 'PLASMA phase_t _uint))
          _uint phase_t)))

#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create the enumerated datatypes for file and memory.  This
* process is simplified if native types are used for the file,
* as only one type must be defined.
|#
(define filetype (H5Tenum_create F_BASET))
(define memtype (H5Tenum_create M_BASET))

(for ([name names]
      [i (length names)])
  ;;(define val (string->symbol name))
  ;; Insert enumerated value for memtype.
  (define val (malloc phase_t 'atomic))
  (ptr-set! val phase_t (string->symbol name))
  (set! status (H5Tenum_insert memtype name val))
  
  #|
  * Insert enumerated value for filetype.  We must first convert
  * the numerical value val to the base type of the destination.
  |#
  (set! status (H5Tconvert M_BASET F_BASET 1 val #f H5P_DEFAULT))
  (set! status (H5Tenum_insert filetype name val)))

#|
* Create dataspace.  Setting maximum size to NULL sets the maximum
* size to be the current size.
|#
(define space (H5Screate_simple 2 dims #f))

#|
* Create the dataset and write the enumerated data to it.
|#
(define dset (H5Dcreate2 fid DATASET filetype space H5P_DEFAULT H5P_DEFAULT
                         H5P_DEFAULT))

(set! status (H5Dwrite dset memtype H5S_ALL H5S_ALL H5P_DEFAULT
                       (vector->cblock wdata phase_t)))

#|
* Close and release resources.
|#
(set! status (H5Dclose dset))
(set! status (H5Sclose space))
(set! status (H5Tclose filetype))
(set! status (H5Fclose fid))


#| 
* Now we begin the read section of this example.  Here we assume
* the dataset has the same name and rank, but can have any size.
* Therefore we must allocate a new array to read in data using
* malloc().  For simplicity, we do not rebuild memtype.
|#

#|
* Open file and dataset.
|#
(set! fid (H5Fopen FILE H5F_ACC_RDONLY H5P_DEFAULT))
(set! dset (H5Dopen fid DATASET H5P_DEFAULT))

#|
* Get dataspace and allocate memory for read buffer.  This is a
* two dimensional dataset so the dynamic allocation must be done
* in steps.
|#
(set! space (H5Dget_space dset))
(define all-dims (H5Sget_simple_extent_dims space))
(define ndims (vector-ref all-dims 1))
(define max-dims (vector-ref all-dims 2))

(define rdata-data (list->cblock (make-list (apply * ndims) 'SOLID) phase_t))

(set! status (H5Dread dset memtype H5S_ALL H5S_ALL H5P_DEFAULT rdata-data))

(define recovered-data (cblock->vector rdata-data phase_t (apply * ndims)))

(module+ test
  (check-equal? wdata recovered-data))

(define data-array (vector->array (list->vector ndims) recovered-data))

(pretty-print data-array)
(for ([i (list-ref dims 0)])
  (printf " [")
  (for ([j (list-ref dims 1)])
    (printf "~a "(array-ref data-array (vector i j))))
  (printf "]\n"))

;; extract strings using pointers
(for ([i (list-ref dims 0)])
  (printf " [")
  (for ([j (list-ref dims 1)])
    (printf "~s "
            (cadr (H5Tenum_nameof memtype (ptr-add rdata-data (+ (* i (list-ref dims 1)) j) phase_t) NAME_BUF_SIZE))))
  (printf "]\n"))

(set! status (H5Dclose dset))
(set! status (H5Sclose space))
(set! status (H5Tclose memtype))
(set! status (H5Fclose fid))
