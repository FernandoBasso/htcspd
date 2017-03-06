#lang htdp/bsl



;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; interp. a natural number
(define N0 0)           ;0
(define N1 (add1 N0))   ;1
(define N2 (add1 N1))   ;2

#; 
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n
              (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: 0
;;  - compound: (add1 Natural)
;;  - self-reference: (sub1 n) is Natural


;
; The factorial of a natural number n is n * n - 1 * n - 2 * ...1.
; So factorial of 3 is 3 * 2 * 1 * 1.
; Let's design a function called fact to compute the factorial.
;
; - factorial of 0 is 1
; - factorial of n is n * factorial(n - 1).
;

;; Natural -> Natural
;; Compute n * n - 1 * n - 2 * ... * 1 (factorial).
(check-expect (fact 0) 1)
(check-expect (fact 3) (* 3 2 1))

;(define (fact n) 1) ;stub

(define (fact n)
  (cond [(zero? n) 1]
        [else
         (* n
            (fact (sub1 n)))]))
