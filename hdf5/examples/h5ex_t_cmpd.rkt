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



#|***********************************************************

This example shows how to read and write compound
datatypes to a dataset.  The program first writes
compound structures to a dataset with a dataspace of DIM0,
then closes the file.  Next, it reopens the file, reads
back the data, and outputs it to the screen.

This file is intended for use with HDF5 Library version 1.8

***********************************************************|#

(define FILE            "./data/h5ex_t_cmpd.h5")
(define DATASET         "DS1")

#| Compound type |#
;;(struct sensor (serial-no location temperature pressure))
(define-cstruct _sensor
  ([serial-no _int]
   [location _string]
   [temperature _double]
   [pressure _double]
   )
  #:alignment 1)


(define (h5t-insert-compound memtype names h5types (ctypes #f))
  (define offset 0)
  (cond [ctypes
         (for ([name names]
               [h5type h5types]
               [ctype ctypes])
           (define size (ctype-sizeof ctype))
           (H5Tinsert memtype name offset h5type)
           (set! offset (+ offset size)))]
        [else
         (for ([name names]
               [h5type h5types])
           (define size (H5Tget_size h5type))
           (H5Tinsert memtype name offset h5type)
           (set! offset (+ offset size)))]))



#|
* Initialize data.
|#
(define wdata
  (list (make-sensor 1153 "Exterior (static)" 53.23 24.57)
        (make-sensor 1184 "Intage" 55.12 22.95)
        (make-sensor 1185 "Whatever 1" 55.12 22.95)
        (make-sensor 1186 "Whatever 50 Intake" 0.12 22.95)
        (make-sensor 1188 "Whatever 40 Intake" 5.12 22.95)
        (make-sensor 1189 "Whatever 30 Intake" 1001.12 12421.19)
        (make-sensor 1190 "Whatever 20 Intake" 55.112031 141.95)
        (make-sensor 112 "Whatever 10 Intake" 148219.12 22.950131290321)
        ))
(define DIM0 (length wdata))
(define dims (list DIM0))

#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create variable-length string datatype.
|#
(define strtype (H5Tcopy H5T_C_S1))
(define status (H5Tset_size strtype H5T_VARIABLE))

#|
* Create the compound datatype for memory.
|#

;; (define str-ptr (malloc 'atomic (ctype-sizeof _sensor)))
;; (memset str-ptr 0 (ctype-sizeof _sensor) _ubyte)
;; (cblock->list str-ptr _ubyte 32)

;; (define s0 (make-sensor 0 "ABCD" 1.0 2.0))
;; (cblock->list s0 _ubyte 32)
;; (memcpy str-ptr 0 s0 (ctype-sizeof _sensor) _ubyte)

;; (memcpy str-ptr 0 (make-sensor 0 (cast "ABCD" _string _pointer) 1.0 2.0) (ctype-sizeof _sensor) _ubyte)
;; (cblock->list str-ptr _ubyte 32)

;; (cblock->list (make-sensor 0 (cast "ABCD" _string _pointer) 1.0 2.0) _ubyte 32)

(define memtype (H5Tcreate 'H5T_COMPOUND (ctype-sizeof _sensor)))
;; (H5Tinsert memtype "Serial number" 0 H5T_NATIVE_INT)
;; (H5Tinsert memtype "Location" 8 strtype)
;; (H5Tinsert memtype "Tempearature (F)" 16 H5T_NATIVE_DOUBLE)
;; (H5Tinsert memtype "Pressure (inHg)" 24 H5T_NATIVE_DOUBLE)
(h5t-insert-compound memtype
                     (list "Serial number" "Location" "Temperature (F)" "Pressure (inHg)")
                     (list H5T_NATIVE_INT strtype H5T_NATIVE_DOUBLE H5T_NATIVE_DOUBLE)
                     (list _int _string _double _double))

#|
* Create the compound datatype for the file.  Because the standard
* types we are using for the file may have different sizes than
* the corresponding native types, we must manually calculate the
* offset of each member.
|#
(define filetype (H5Tcreate 'H5T_COMPOUND (+ 4 8 8 8)))
(H5Tinsert filetype "Serial number" 0 H5T_STD_I32BE)
(H5Tinsert filetype "Location" (+ 4) strtype)
(H5Tinsert filetype "Temperature (F)" (+ 4 8) H5T_IEEE_F64BE)
(H5Tinsert filetype "Pressure (inHg)" (+ 4 8 8) H5T_IEEE_F64BE)


;; (h5t-insert-compound filetype
;;                      (list "Serial number" "Location" "Temperature (F)" "Pressure (inHg)")
;;                      (list H5T_STD_I64BE strtype H5T_IEEE_F64BE H5T_IEEE_F64BE))

#|
* Create dataspace.  Setting maximum size to NULL sets the maximum
* size to be the current size.
|#
(define space (H5Screate_simple 1 dims #f))

#|
* Create the dataset and write the compound data to it.
|#
(define dset (H5Dcreate fid DATASET filetype space H5P_DEFAULT H5P_DEFAULT
                        H5P_DEFAULT))

(define wdata-ptr (list->cblock wdata _sensor))

;;(define wdata-ptr (malloc (* 10 (ctype-sizeof _sensor))))
(H5Dwrite dset memtype H5S_ALL H5S_ALL H5P_DEFAULT wdata-ptr)

#|
* Close and release resources.
|#
(H5Dclose dset)
(H5Sclose space)
(H5Tclose filetype)
(H5Fclose fid)


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
* Get dataspace and allocate memory for read buffer.
|#
(set! space (H5Dget_space dset))
(define ndims (H5Sget_simple_extent_ndims space))
(define all-dims (H5Sget_simple_extent_dims space))
(set! dims (vector-ref all-dims 1))
(define rdata-ptr (malloc (* (apply * dims) (ctype-sizeof _sensor))))
(printf "SIZE: ~a~n" (* (apply * dims) (ctype-sizeof _sensor)))

#|
* Read the data.
|#
(H5Dread dset memtype H5S_ALL H5S_ALL H5P_DEFAULT rdata-ptr)
(printf "~a~n" (ptr-ref rdata-ptr _int32 'abs 0))
(printf "~a~n" (ptr-ref rdata-ptr _string 'abs 4))
;;()
(define rdata (cblock->list rdata-ptr _sensor (apply * dims)))

(map sensor->list rdata)
(for ([d rdata]
      [i (length rdata)])
  (printf "~s[~a]:~n" DATASET i)
  (printf "Serial number  : ~a~n" (sensor-serial-no d))
  (printf "Location       : ~a~n" (sensor-location d))
  (printf "Temperature    : ~a~n" (sensor-temperature d))
  (printf "Pressure       : ~a~n" (sensor-pressure d)))


#|
* Close and release resources.  H5Dvlen_reclaim will automatically
* traverse the structure and free any vlen data (strings in this
* case).
|#
(H5Dvlen_reclaim memtype space H5P_DEFAULT rdata-ptr)
(H5Dclose dset)
(H5Sclose space)
(H5Tclose memtype)
(H5Tclose strtype)
(H5Fclose fid)
