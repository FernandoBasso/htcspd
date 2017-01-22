#lang htdp/bsl

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumeber)
;; Interp: a list of numbers.
(define LON1 '())
(define LON2 (cons 5.1 '()))
(define LON3 (cons 5 (cons 19.4 (cons 19.5 '()))))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template Rules Used:
;; - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;; - self-reference (rest lon) is ListOfNumber;

;
; PROBLEM:
;
; Design a function that consumes a list of numbers and produces the
; largest number in the list. You may assume that all numbers in the
; list are greater than 0. If the list is empty, produce 0.
;

;; ListOfNumber -> Number
;; Produce the largest number in the consumed list.
(check-expect (largest '()) 0)
(check-expect (largest (cons 5 '())) 5)
(check-expect (largest (cons 5 (cons 3.1 (cons 19.4 '())))) 19.4)

;(define (largest lon) 0) ;stub

(define (largest lon)
  (cond [(empty? lon) 0]
        [else
         (if (> (first lon) (largest (rest lon)))
             (first lon)
             (largest (rest lon)))]))


