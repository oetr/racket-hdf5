#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5ipublic.rkt")

(define hdf5-lib (ffi-lib "libhdf5_serial"))

(define-ffi-definer define-hdf5-lib-internal hdf5-lib)

(define-syntax define-hdf5
  (syntax-rules ()
    [(_ name body)
     (begin
       (provide name)
       (define-hdf5-lib-internal name body))]))


#|
 * The library's property list classes
 |#

(define+provide H5P_ROOT (get-ffi-obj 'H5P_CLS_ROOT_ID_g hdf5-lib hid_t))
(define+provide H5P_OBJECT_CREATE (get-ffi-obj 'H5P_CLS_OBJECT_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_FILE_CREATE (get-ffi-obj 'H5P_CLS_FILE_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_FILE_ACCESS (get-ffi-obj 'H5P_CLS_FILE_ACCESS_ID_g hdf5-lib hid_t))
(define+provide H5P_DATASET_CREATE (get-ffi-obj 'H5P_CLS_DATASET_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_DATASET_ACCESS (get-ffi-obj 'H5P_CLS_DATASET_ACCESS_ID_g hdf5-lib hid_t))
(define+provide H5P_DATASET_XFER (get-ffi-obj 'H5P_CLS_DATASET_XFER_ID_g hdf5-lib hid_t))
(define+provide H5P_FILE_MOUNT (get-ffi-obj 'H5P_CLS_FILE_MOUNT_ID_g hdf5-lib hid_t))
(define+provide H5P_GROUP_CREATE (get-ffi-obj 'H5P_CLS_GROUP_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_GROUP_ACCESS (get-ffi-obj 'H5P_CLS_GROUP_ACCESS_ID_g hdf5-lib hid_t))
(define+provide H5P_DATATYPE_CREATE (get-ffi-obj 'H5P_CLS_DATATYPE_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_DATATYPE_ACCESS (get-ffi-obj 'H5P_CLS_DATATYPE_ACCESS_ID_g hdf5-lib hid_t))
(define+provide H5P_STRING_CREATE (get-ffi-obj 'H5P_CLS_STRING_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_ATTRIBUTE_CREATE (get-ffi-obj 'H5P_CLS_ATTRIBUTE_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_OBJECT_COPY (get-ffi-obj 'H5P_CLS_OBJECT_COPY_ID_g hdf5-lib hid_t))
(define+provide H5P_LINK_CREATE (get-ffi-obj 'H5P_CLS_LINK_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_LINK_ACCESS (get-ffi-obj 'H5P_CLS_LINK_ACCESS_ID_g hdf5-lib hid_t))


#|
 * The library's default property lists
 |#
(define+provide H5P_FILE_CREATE_DEFAULT (get-ffi-obj 'H5P_LST_FILE_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_FILE_ACCESS_DEFAULT (get-ffi-obj 'H5P_LST_FILE_ACCESS_ID_g hdf5-lib hid_t))
(define+provide H5P_DATASET_CREATE_DEFAULT (get-ffi-obj 'H5P_LST_DATASET_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_DATASET_ACCESS_DEFAULT (get-ffi-obj 'H5P_LST_DATASET_ACCESS_ID_g hdf5-lib hid_t))
(define+provide H5P_DATASET_XFER_DEFAULT (get-ffi-obj 'H5P_LST_DATASET_XFER_ID_g hdf5-lib hid_t))
(define+provide H5P_FILE_MOUNT_DEFAULT (get-ffi-obj 'H5P_LST_FILE_MOUNT_ID_g hdf5-lib hid_t))
(define+provide H5P_GROUP_CREATE_DEFAULT (get-ffi-obj 'H5P_LST_GROUP_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_GROUP_ACCESS_DEFAULT (get-ffi-obj 'H5P_LST_GROUP_ACCESS_ID_g hdf5-lib hid_t))
(define+provide H5P_DATATYPE_CREATE_DEFAULT (get-ffi-obj 'H5P_LST_DATATYPE_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_DATATYPE_ACCESS_DEFAULT (get-ffi-obj 'H5P_LST_DATATYPE_ACCESS_ID_g hdf5-lib hid_t))
(define+provide H5P_ATTRIBUTE_CREATE_DEFAULT (get-ffi-obj 'H5P_LST_ATTRIBUTE_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_OBJECT_COPY_DEFAULT (get-ffi-obj 'H5P_LST_OBJECT_COPY_ID_g hdf5-lib hid_t))
(define+provide H5P_LINK_CREATE_DEFAULT (get-ffi-obj 'H5P_LST_LINK_CREATE_ID_g hdf5-lib hid_t))
(define+provide H5P_LINK_ACCESS_DEFAULT (get-ffi-obj 'H5P_LST_LINK_ACCESS_ID_g hdf5-lib hid_t))


;; Common creation order flags (for links in groups and attributes on objects)
(define+provide H5P_CRT_ORDER_TRACKED           #x0001)
(define+provide H5P_CRT_ORDER_INDEXED           #x0002)

;; Default value for all property list classes
(define+provide H5P_DEFAULT     0)


(define-hdf5 H5Pcreate
  (_fun (cls_id     : hid_t)
        -> (result : hid_t)))

