#lang htdp/isl

;; (listof Number) -> Number
;; produce the product of all the numbers in lon
(check-expect (product empty) 1)
(check-expect (product (list 1 2 3)) 6)
(check-expect (product (list 2.5 1 -4)) -10)


;
; This is my solution. It works but in the question on the
; site it was considered an incorrect choice.
;
#;
(define (product lon0)
  ;; acc: Number; product of the numbers seen so far.
  (local [(define (product lon acc)
            (cond [(empty? lon) 1]
                  [else
                   (* (car lon)
                      (product (cdr lon) acc))]))]
    (product lon0 1)))

;
; This was considered the correct choice.
;
(define (product lon0)
  ;; acc: Number; product of the numbers seen so far
  (local [(define (product lon acc)
            (cond [(empty? lon) acc]
                  [else
                   (product (rest lon) (* (first lon) acc))]))]
    (product lon0 1)))

;
; NOTE:
; The structure of the tail recursive product is very similar
; to that of sum, but we have * and 1 as our combination and initial
; accumulator value instead of + and 0.
;
