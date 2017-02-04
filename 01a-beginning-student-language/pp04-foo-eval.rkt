#lang htdp/bsl

; PROBLEM:
;
; Given the following function definition:
;
; (define (foo n)
;   (* n n))
;
; Write out the step-by-step evaluation of the expression:
;
; (foo (+ 3 4))
;
; Be sure to show every intermediate evaluation step.
;

(define (foo n)
  (* n n))

(foo (+ 3 4))

(foo 7)

(* 7 7)

49

