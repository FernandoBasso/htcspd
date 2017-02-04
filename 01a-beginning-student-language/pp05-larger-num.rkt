#lang htdp/bsl

;
; PROBLEM:
;
; Write a function that consumes two numbers and produces the larger
; of the two.
;

(define (larger-num x y)
  (if (> x y) x y))

(larger-num 5 9)
(larger-num 11 7)
;; → 9
;; → 11

