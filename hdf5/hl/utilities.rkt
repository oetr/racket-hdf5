#lang racket

(require "../unsafe/h5opublic.rkt")

(provide (all-defined-out))

;; print all object names in the dataset and some of their properties
(define (hdf5-list-all fid)
  (define (iter-proc obj name info op-data)
    (define (print-val name val)
      (printf (~a "  -> " name ": " val  "\n")))
    (printf "~s:~n" name)
    (print-val "type" (H5O_info_t-type info))
    (print-val "rc" (H5O_info_t-rc info))
    (print-val "num_attrs"  (H5O_info_t-num_attrs info)))

  (H5Ovisit fid 'H5_INDEX_NAME 'H5_ITER_NATIVE iter-proc #f))
