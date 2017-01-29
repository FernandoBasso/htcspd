#lang htdp/bsl

;
; A function that will give the length of the hypotenuse of
; any right triangle.
;

(define (pythag a b)
  (sqrt (+ (sqr a) (sqr b))))

; (pythag 3 4)
; 5
