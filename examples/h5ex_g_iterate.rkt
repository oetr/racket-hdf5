#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit)

(require "../unsafe/hdf5.rkt")


#|
 * Define operator data structure type for H5Literate callback.
 * During recursive iteration, these structures will form a
 * linked list that can be searched for duplicate groups,
 * preventing infinite recursion.
|#

(define-cstruct _opdata
  ([recurs _uint]               ;; Recursion level.  0=root
   [*prev _opdata-pointer/null] ;; Pointer to previous opdata
   [addr haddr_t]))             ;; Group address


#|***********************************************************
  Operator function.  This function prints the name and type
  of the object passed to it.  If the object is a group, it
  is first checked against other groups in its path using
  the group_check function, then if it is not a duplicate,
  H5Literate is called for that group.  This guarantees that
  the program will not enter infinite recursion due to a
  circular path in the file.
 ************************************************************|#
(define (op_func loc_id *name *info *operator_data)
  (define status 0)
  (define return_val 0)
  (define infobuf (allocate-H5O_info_t))

  (define od (ptr-ref *operator_data _opdata)) ;; Type conversion
  ;; Number of whitespaces to prepend to output
  (define spaces (* 2 (+ 1 (opdata-recurs od))))
  #|
  * Get type of the object and display its name and type.
  * The name of the object is passed to this function by
  * the Library.
  |#
  (set! status (H5Oget_info_by_name loc_id *name H5P_DEFAULT infobuf))
  (printf "~a" (make-string spaces #\ ))     ;;Format output
  (match (H5O_info_t-type infobuf)
    ['H5O_TYPE_GROUP
     (printf "Group: ~a {\n" *name)
    #|
    * Check group address against linked list of operator
    * data structures.  We will always run the check, as the
    * reference count cannot be relied upon if there are
    * symbolic links, and H5Oget_info_by_name always follows
    * symbolic links.  Alternatively we could use H5Lget_info
    * and never recurse on groups discovered by symbolic
    * links, however it could still fail if an object's
    * reference count was manually manipulated with
    * H5Odecr_refcount.
    |#
    (if (group-check od (H5O_info_t-addr infobuf))
        (printf "~a  Warning: Loop detected!\n" (make-string spaces #\ ))
        (local ()
          #|
          * Initialize new operator data structure and
          * begin recursive iteration on the discovered
          * group.  The new opdata structure is given a
          * pointer to the current one.
          |#
          (define nextod (make-opdata (+ 1 (opdata-recurs od))
                                      od
                                      (H5O_info_t-addr infobuf)))
          (set! return_val (H5Literate_by_name loc_id *name 'H5_INDEX_NAME
                            'H5_ITER_NATIVE #f op_func nextod H5P_DEFAULT))))
        
    (printf "~a}\n" (make-string spaces #\ ))]
  ['H5O_TYPE_DATASET
   (printf "Dataset: ~a\n" *name)]
  ['H5O_TYPE_NAMED_DATATYPE
   (printf "Datatype: ~a\n" *name)]
  [_ (printf "Unknown: ~a\n" *name)])
  return_val)

#|***********************************************************

  This function recursively searches the linked list of
  opdata structures for one whose address matches
  target_addr.  Returns 1 if a match is found, and 0
  otherwise.

***********************************************************|#
;;int group_check (struct opdata *od, haddr_t target_addr)
(define (group-check od target_addr)
  (cond [(not od) 0]
        [(= target_addr (opdata-addr od)) 1] ;; Addresses match
        [(not (opdata-recurs od)) 0] ;; Root group reached with no matches
        [else (group-check (opdata-*prev od) target_addr)]))


(define filename (expand-user-path "./data/h5ex_g_iterate.h5"))

(define f (H5Fopen filename H5F_ACC_RDONLY H5P_DEFAULT))

;;(define p (malloc _H5O_info_t 'atomic))
(define infobuf (H5Oget_info f))

(define od (make-opdata 0 #f (H5O_info_t-addr infobuf)))


(H5O_info_t-fileno infobuf)
(H5O_info_t-addr infobuf)
(H5O_info_t-type infobuf)
(H5O_info_t-rc infobuf)
(H5O_info_t-atime infobuf)
(H5O_info_t-mtime infobuf)
(H5O_info_t-ctime infobuf)
(H5O_info_t-btime infobuf)
(H5O_info_t-num_attrs infobuf)
(H5O_info_t-hdr infobuf)
(H5_ih_info_t-index (car (H5O_info_t-meta_size infobuf)))

(printf "{\n")
(define status (H5Literate f 'H5_INDEX_NAME 'H5_ITER_NATIVE #f op_func
                           od))
(printf "}\n")
