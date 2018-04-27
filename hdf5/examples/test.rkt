#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(define test-lib (ffi-lib "./test.so"))


(define-ffi-definer define-test test-lib)

(define-test char_array
  (_fun _pointer -> _int))

(define-test array_of_char_arrays
  (_fun _pointer _uint -> _void))

(define-cstruct _point
  ([x _int]
   [y _int]
   [z _int]
   [name _string]))

(define-test allocate_points
  (_fun (n) :: 
        (n : _int)
        -> _pointer))

(define p (malloc 10))
(memset p 0 10)
(cblock->list p _ubyte 10)
(char_array p)
(cblock->list p _ubyte 10)

(define s (ctype-sizeof _ubyte))
(define rows 3)
(define cols (+ 5 0))

(define a (list "Test1 \0    Hey" "Test2" "Test123"))
(define array-ptr-ptr (list->cblock a _string))

;;(cblock->list array-ptr-ptr _string 3)

(define ptr-ptr (map (lambda (str)
                     (cast str _string _pointer))
                     (list "Test" "me" "again" "and" "again")))

(define mem (malloc 3 _string 'atomic))
(memset mem 0 (* 3 8) _byte)
(ptr-set! mem _pointer 0 (cast "B " _string _pointer))
(ptr-set! mem _pointer 1 (cast "C " _string _pointer))
(ptr-set! mem _pointer 2 (cast "D " _string _pointer))
;;(cblock->list mem _ubyte (* 3 8))

;;(cblock->list (ptr-ref mem _pointer 0) _ubyte 3)
;;(ptr-ref mem _string 0)
;;(cblock->list mem _string 5)

;;(cblock->list (list->cblock ptr-ptr _pointer) _string 1)

;; (cast (ptr-ref (list->cblock (list "abc") _string) _pointer 0)
;;       _pointer
;;       _string)

;;(cast (ptr-ref array-ptr-ptr _pointer 1) _pointer _string)
;;(cblock->list (ptr-ref array-ptr-ptr _pointer 0) _byte 6)

;;(define array-ptr-ptr (malloc (* rows cols s)))
;;(memset array-ptr-ptr 0 (* rows cols s))

;;(cblock->list array-ptr-ptr _ubyte (* rows cols))

;; (array_of_char_arrays (ptr-ref array-ptr-ptr _pointer 0) rows)
(array_of_char_arrays mem rows)


(define points-ptr (allocate_points 10))

(define points (cblock->list points-ptr _point 10))
(for/list ([point points])
  (point-name point))
