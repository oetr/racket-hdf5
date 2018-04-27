#|***********************************************************

  This example shows how to read and write a complex
  compound datatype to a dataset.  The program first writes
  complex compound structures to a dataset with a dataspace
  of DIM0, then closes the file.  Next, it reopens the file,
  reads back selected fields in the structure, and outputs
  them to the screen.

  Unlike the other datatype examples, in this example we
  save to the file using native datatypes to simplify the
  type definitions here.  To save using standard types you
  must manually calculate the sizes and offsets of compound
  types as shown in h5ex_t_cmpd.c, and convert enumerated
  values as shown in h5ex_t_enum.c.

  The datatype defined here consists of a compound
  containing a variable-length list of compound types, as
  well as a variable-length string, enumeration, double
  array, object reference and region reference.  The nested
  compound type contains an int, variable-length string and
  two doubles.

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

(define FILE            "./data/h5ex_t_cpxcmpd.h5")
(define DATASET         "DS1")
(define DIM0            2)

(define-cstruct _sensor
  ([serial-no _int]
   [location _string]
   [temperature _double]
   [pressure _double])
  #:define-unsafe)


(define _color (_enum '(RED GREEN BLUE)))

#| Main compound type |#
(define-cstruct _vehicle
  ([sensors _hvl_t]
   [name _string]
   [color _color]
   [location (_list io _double 3)]
   [group hobj_ref_t]
   [surveyed-areas hdset_reg_ref_t])
  #:define-unsafe)

#| Read type |#
(define-cstruct _rvehicle
  ([sensors _hvl_t]
   [name _string])
  #:define-unsafe)

(define dims (list DIM0))
(define adims (list 3))
(define adims2 (list 32 32))
(define start (list 8 26))
(define count (list 4 3))
(define coords (list (list 3 2)
                     (list 3 3)
                     (list 4 4)))

#|
* Create a new file using the default properties.
|#
(define fid (H5Fcreate FILE H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT))

#|
* Create dataset to use for region references.
|#
(define wdata2 (build-array (vector 32 32)
                        (lambda (indices)
                          (define i (vector-ref indices 0))
                          (define j (vector-ref indices 1))
                          (+ 70.0 (* 0.1 (- i 16.0)) (* 0.1 (- j 16.0))))))

(define space (H5Screate_simple adims2))
(define dset (H5Dcreate fid "Ambient_Temperature" H5T_NATIVE_DOUBLE space
                        H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT))
(H5Dwrite dset H5T_NATIVE_DOUBLE H5S_ALL H5S_ALL H5P_DEFAULT
          (array->cblock wdata2 _double))
(H5Dclose dset)

#|
* Create groups to use for object references.
|#
(H5Gclose (H5Gcreate fid "Land_Vehicles" H5P_DEFAULT H5P_DEFAULT
                H5P_DEFAULT))
(H5Gclose (H5Gcreate fid "Air_Vehicles" H5P_DEFAULT H5P_DEFAULT
                     H5P_DEFAULT))

#|
* Initialize variable-length compound in the first data element.
|#
(define sensors0 (list (make-sensor 1153 "Exterior (static)" 53.23 24.57)
                       (make-sensor 1184 "Intake" 55.12 22.95)
                       (make-sensor 1027 "Intake manifold" 103.55 31.23)
                       (make-sensor 1313 "Exhaust manifold" 1252.89 84.11)))


(define sensors1 (list (make-sensor 3244 "Roof" 83.82 29.92)))

(H5Sselect_elements space 'H5S_SELECT_SET (length coords)
                    (list->cblock (map (lambda (n)
                                         (list->cblock n hsize_t))
                                       coords) _pointer))
(H5Sselect_hyperslab space 'H5S_SELECT_SET start #f count #f)

(define wdata
  (list
   (make-vehicle (make-hvl_t (length sensors0)
                             (list->cblock sensors0 _sensor))
                 "Airplane" 'GREEN (list -103234.21 422638.78 5996.43)
                 (ptr-ref (H5Rcreate fid "Air_Vehicles" 'H5R_OBJECT -1) _uint64)
                 (cast (H5Rcreate fid "Ambient_Temperature" 'H5R_DATASET_REGION space)
                       _pointer hdset_reg_ref_t))
   (make-vehicle (make-hvl_t (length sensors1)
                             (list->cblock sensors1 _sensor))
                 "Automobile" 'RED (list 326734.36 221568.23 432.36)
                 (ptr-ref (H5Rcreate fid "Land_Vehicles" 'H5R_OBJECT -1) _uint64)
                 (cast (H5Rcreate fid "Ambient_Temperature" 'H5R_DATASET_REGION space)
                       _pointer hdset_reg_ref_t))))
(H5Sclose space)

#|
* Create variable-length string datatype.
|#
(define strtype (H5Tcopy H5T_C_S1))
(H5Tset_size strtype H5T_VARIABLE)

#|
* Create the nested compound datatype.
|#
(define sensortype (H5Tcreate 'H5T_COMPOUND (ctype-sizeof _sensor)))
(H5Tinsert sensortype "Serial number" sensor-serial-no-offset H5T_NATIVE_INT)
(H5Tinsert sensortype "Location" sensor-location-offset strtype)
(H5Tinsert sensortype "Temperature (F)" sensor-temperature-offset H5T_NATIVE_DOUBLE)
(H5Tinsert sensortype "Pressure (inHg)" sensor-pressure-offset H5T_NATIVE_DOUBLE)

#|
* Create the variable-length datatype.
|#
(define sensorstype (H5Tvlen_create sensortype))

#|
* Create the enumerated datatype.
|#
(define colortype (H5Tenum_create H5T_NATIVE_INT))
(define val (malloc _color 1))
(ptr-set! val _color 'RED)
(H5Tenum_insert colortype "Red" val)
(ptr-set! val _color 'GREEN)
(H5Tenum_insert colortype "Green" val)
(ptr-set! val _color 'BLUE)
(H5Tenum_insert colortype "Blue" val)

#|
* Create the array datatype.
|#
(define loctype (H5Tarray_create H5T_NATIVE_DOUBLE adims))

#|
* Create the main compound datatype.
|#

(define (h5t-insert-compound memtype names h5types (ctypes #f))
  (define offset 0)
  (cond [ctypes
         (for ([name names]
               [h5type h5types]
               [ctype ctypes])
           (define size (ctype-sizeof ctype))
           (printf "~s [~a]: ~a ~a~n" name offset size h5type)
           (H5Tinsert memtype name offset h5type)
           (set! offset (+ offset size)))]
        [else
         (for ([name names]
               [h5type h5types])
           (define size (H5Tget_size h5type))
           (printf "~s [~a]: ~a ~a~n" name offset size h5type)
           (H5Tinsert memtype name offset h5type)
           (set! offset (+ offset size)))]))

(map H5Tget_size
     (list sensorstype strtype colortype loctype H5T_STD_REF_OBJ H5T_STD_REF_DSETREG))



(list vehicle-sensors-offset
      vehicle-name-offset
      vehicle-color-offset
      vehicle-location-offset
      vehicle-group-offset
      vehicle-surveyed-areas-offset)

;; (define-cstruct _vehicle
;;   ([sensors _hvl_t]
;;    [name _string]
;;    [color _color]
;;    [location (_list io _double 3)]
;;    [group hobj_ref_t]
;;    [surveyed-areas hdset_reg_ref_t])
;;   #:define-unsafe)

;; (h5t-insert-compound vehicletype
;;                      (list "Sensors" "Name" "Color" "Location" "Group" "Surveyed areas")
;;                      (list sensorstype strtype colortype loctype H5T_STD_REF_OBJ H5T_STD_REF_DSETREG)
;;                      (list _hvl_t _string _color _pointer hobj_ref_t hdset_reg_ref_t))


(define vehicletype (H5Tcreate 'H5T_COMPOUND (ctype-sizeof _vehicle)))
(H5Tinsert vehicletype "Sensors" vehicle-sensors-offset sensorstype)
(H5Tinsert vehicletype "Name" vehicle-name-offset strtype)
(H5Tinsert vehicletype "Color" vehicle-color-offset colortype)
(H5Tinsert vehicletype "Location" 30 loctype)
(H5Tinsert vehicletype "Group" 44 H5T_STD_REF_OBJ)
;;(H5Tinsert vehicletype "Surveyed areas" vehicle-surveyed-areas-offset H5T_STD_REF_DSETREG)

#|
* Create dataspace.  Setting maximum size to NULL sets the maximum
* size to be the current size.
|#
(set! space (H5Screate_simple dims #f))

#|
* Create the dataset and write the compound data to it.
|#
(set! dset (H5Dcreate fid DATASET vehicletype space H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT))
;;(H5Dwrite dset vehicletype H5S_ALL H5S_ALL H5P_DEFAULT (list->cblock wdata _vehicle))

    #|
     * Close and release resources.  Note that we cannot use
     * H5Dvlen_reclaim as it would attempt to free() the string
     * constants used to initialize the name fields in wdata.  We must
     * therefore manually free() only the data previously allocated
     * through malloc().
     |#
(H5Dclose dset)
(H5Sclose space)
(H5Tclose strtype)
(H5Tclose sensortype)
(H5Tclose sensorstype)
(H5Tclose colortype)
(H5Tclose loctype)
(H5Tclose vehicletype)
(H5Fclose fid)


;;     #|
;;      * Now we begin the read section of this example.  Here we assume
;;      * the dataset has the same name and rank, but can have any size.
;;      * Therefore we must allocate a new array to read in data using
;;      * malloc().  We will only read back the variable length strings.
;;      |#

;;     #|
;;      * Open file and dataset.
;;      |#
;;     file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
;;     dset = H5Dopen (file, DATASET, H5P_DEFAULT);

;;     #|
;;      * Create variable-length string datatype.
;;      |#
;;     strtype = H5Tcopy (H5T_C_S1);
;;     status = H5Tset_size (strtype, H5T_VARIABLE);

;;     #|
;;      * Create the nested compound datatype for reading.  Even though it
;;      * has only one field, it must still be defined as a compound type
;;      * so the library can match the correct field in the file type.
;;      * This matching is done by name.  However, we do not need to
;;      * define a structure for the read buffer as we can simply treat it
;;      * as a char *.
;;      |#
;;     rsensortype = H5Tcreate (H5T_COMPOUND, sizeof (char *));
;;     status = H5Tinsert (rsensortype, "Location", 0, strtype);

;;     #|
;;      * Create the variable-length datatype for reading.
;;      |#
;;     rsensorstype = H5Tvlen_create (rsensortype);

;;     #|
;;      * Create the main compound datatype for reading.
;;      |#
;;     rvehicletype = H5Tcreate (H5T_COMPOUND, sizeof (rvehicle_t));
;;     status = H5Tinsert (rvehicletype, "Sensors", HOFFSET (rvehicle_t, sensors),
;;                 rsensorstype);
;;     status = H5Tinsert (rvehicletype, "Name", HOFFSET (rvehicle_t, name),
;;                 strtype);

;;     #|
;;      * Get dataspace and allocate memory for read buffer.
;;      |#
;;     space = H5Dget_space (dset);
;;     ndims = H5Sget_simple_extent_dims (space, dims, NULL);
;;     rdata = (rvehicle_t *) malloc (dims[0] * sizeof (rvehicle_t));

;;     #|
;;      * Read the data.
;;      |#
;;     status = H5Dread (dset, rvehicletype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

;;     #|
;;      * Output the data to the screen.
;;      |#
;;     for (i=0; i<dims[0]; i++) {
;;         printf ("%s[%d]:\n", DATASET, i);
;;         printf ("   Vehicle name :\n      %s\n", rdata[i].name);
;;         printf ("   Sensor locations :\n");
;;         for (j=0; j<rdata[i].sensors.len; j++)
;;             printf ("      %s\n", ( (char **) rdata[i].sensors.p )[j] );
;;     }

;;     #|
;;      * Close and release resources.  H5Dvlen_reclaim will automatically
;;      * traverse the structure and free any vlen data (including
;;      * strings).
;;      |#
;;     status = H5Dvlen_reclaim (rvehicletype, space, H5P_DEFAULT, rdata);
;;     free (rdata);
;;     status = H5Dclose (dset);
;;     status = H5Sclose (space);
;;     status = H5Tclose (strtype);
;;     status = H5Tclose (rsensortype);
;;     status = H5Tclose (rsensorstype);
;;     status = H5Tclose (rvehicletype);
;;     status = H5Fclose (file);

;;     return 0;
;; }
