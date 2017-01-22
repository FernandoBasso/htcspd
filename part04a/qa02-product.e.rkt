#lang htdp/bsl

; Design a data definition and a function that multiplies the numbers
; in a list. Call this function `product`.

;; ListOfNumber is one of:
;; - '()
;; - (cons Number ListOfNumber)
(define LON1 '())
(define LON2 (cons 3 '()))
(define LON3 (cons -5 (cons -4 (cons 10 '()))))

(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template Rules Used:
;; - one of: 2 cases
;;   - atomic distinct: empty
;;   - (cons Number ListOfNumber)
;; - self-reference: (rest lon) is ListOfNumber

;; ListOfNumber -> Number
;; Produces the product of the numbers in the consumed list.
(check-expect (product '()) 1)
(check-expect (product (cons -3 '())) (* -3 1))
(check-expect (product (cons -5 (cons -4 (cons 10 '()))))
              (* -5 (* -4 (* 10 1))))

;(define (product lon) 0) ;stub

(define (product lon)
  (cond [(empty? lon) 1]
        [else
         (* (first lon)
            (product (rest lon)))]))
