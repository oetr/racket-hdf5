#|***********************************************************

This example shows how to commit a named datatype to a
file, and read back that datatype.  The program first
defines a compound datatype, commits it to a file, then
closes the file.  Next, it reopens the file, opens the
datatype, and outputs the names of its fields to the
screen.

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


(define FILE            "./data/h5ex_t_commit.h5")
(define DATATYPE        "Sensor_Type")
#| Handles |#
(define status #f)
#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create variable-length string datatype.
|#
(define strtype (H5Tcopy H5T_C_S1))
(set! status (H5Tset_size strtype H5T_VARIABLE))

#|
* Create the compound datatype.  Because the standard types we are
* using may have different sizes than the corresponding native
* types, we must manually calculate the offset of each member.
|#
(define filetype (H5Tcreate 'H5T_COMPOUND (+ 8 (ctype-sizeof (_ptr io _ubyte)) 8 8)))
(set! status (H5Tinsert filetype "Serial number" 0 H5T_STD_I64BE))
(set! status (H5Tinsert filetype "Location" 8 strtype))
(set! status (H5Tinsert filetype "Temperature (F)" (+ 8 (ctype-sizeof (_ptr io _ubyte)))
                        H5T_IEEE_F64BE))
(set! status (H5Tinsert filetype "Pressure (inHg)" (+ 8 (ctype-sizeof (_ptr io _ubyte)) 8)
                        H5T_IEEE_F64BE))

#|
* Commit the compound datatype to the file, creating a named
* datatype.
|#
(set! status (H5Tcommit fid DATATYPE filetype H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT))

#|
* Close and release resources.
|#
(set! status (H5Tclose filetype))
(set! status (H5Tclose strtype))
(set! status (H5Fclose fid))


#|
* Now we begin the read section of this example.
|#

#|
* Open file.
|#
(set! fid (H5Fopen FILE H5F_ACC_RDONLY H5P_DEFAULT))

#|
* Open the named datatype.
|#
(set! filetype (H5Topen2 fid DATATYPE H5P_DEFAULT))

#|
* Output the data to the screen.
|#
(printf "Named datatype: ~s:\n" DATATYPE)
#|
* Get datatype class.  If it isn't compound, we won't print
* anything.
|#
(define typeclass (H5Tget_class filetype))
(when (symbol=? typeclass 'H5T_COMPOUND)
  (printf "   Class: H5T_COMPOUND\n");
  (define nmembs (H5Tget_nmembers filetype))
  #|
  * Iterate over compound datatype members.
  |#
  (for ([i nmembs])
    #|
    * Get the member name and print it.  Note that
    * H5Tget_member_name allocates space for the string in
    * name, so we must H5free_memory() it after use.
    |#
    (define name (H5Tget_member_name filetype i))
    (printf "   ~s\n" name)))

#|
* Close and release resources.
|#
(set! status (H5Tclose filetype))
(set! status (H5Fclose fid))
