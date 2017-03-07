#lang htdp/bsl

;
; Design a function called odd-from-n that consumes a natural number
; n, and produces a list of all the odd numbers from n down to 1.
;
; Note that there is a primitive function, odd?, that produces #true
; if a natural number is odd.
;

;; Natural -> ListOfNatural
;; Produce a list of odd natural numbers from n to 1.
(check-expect (odd-from-n 0) '())
(check-expect (odd-from-n 1) (cons 1 '()))
(check-expect (odd-from-n 5) (cons 5 (cons 3 (cons 1 '()))))

;(define (odd-from-n n) '()) ;stub

(define (odd-from-n n)
  (cond [(zero? n) '()]
        [else
         (if (odd? n)
             (cons n (odd-from-n (sub1 n)))
             (odd-from-n (sub1 n)))]))
