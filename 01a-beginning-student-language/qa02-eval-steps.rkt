#lang htdp/bsl

(define (foo a b)
  (+ (* 3 a)
     b
     (* b a)))

(foo (+ 1 1) 4)

(foo 2 4)

(+ (* 3 2)
   4
   (* 2 4))

(+ 6
   4
   (* 2 4))

(+ 6
   4
   8)

18

