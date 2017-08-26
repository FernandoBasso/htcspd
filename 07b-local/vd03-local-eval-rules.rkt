#lang htdp/isl

(define b 1)

(+ b
   (local [(define b 2)]
     (* b b))
   b)

;; 6

(+ 1
   (local [(define b 2)]
     (* b b))
   b)

(+ 1
   (local [(define b 2)]
     (* b b))
   b)

;; renaming
(+ 1
   (local [(define b_0 2)]
     (* b_0 b_0))
   b)

;; lifting
(define b_0 2)
(+ 1
   (local [(define b_0 2)]
     (* b_0 b_0))
   b)

;; replace local with its renamed body
(+ 1
   (* b_0 b_0)
   b)

;; The rules for evaluating local expressions work to eliminate the
;; local from the program, leaving behind a program we already
;; know how to evaluate.

(+ 1
   (* 2 b_0)
   b)

(+ 1
   (* 2 2)
   b)

(+ 1
   4
   b)

(+ 1
   4
   1)

6