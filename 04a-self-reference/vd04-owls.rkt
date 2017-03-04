#lang htdp/bsl

;
; PROBLEM:
;
; You've been asked to design a program having to do with all the owls
; in the owlery.
;
; (A) Design a data definition to represent the weights of all the owls.
;     For this problem call it ListOfNumber.
; (B) Design a function that consumes the weights of owls and produces
;     the total weight of all the owls.
; (C) Design a function that consumes the weights of owls and produces
;     the total number of owls.
;

;; ================================================================
;; DATA DEFINITIONS

;; ListOfNumber is one of:
;; - '()
;; - (cons Number ListOfNumber)
;; Interp. Each number in the list is an owel weight in ounces.
(define LON1 '())
(define LON2 (cons 60 (cons 42 '())))

#;
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



;; ================================================================
;; FUNCTIONS

;; ListOfNumber -> Number
;; Produce total weight of owls in consumed list of weights.
(check-expect (sum '()) 0)
(check-expect (sum (cons 60 '())) (+ 60 0))
(check-expect (sum (cons 60 (cons 42 '()))) (+ 60 (+ 42 0)))

;(define (sum lon) 0) ;stub

(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))


;; ListOfNumber -> Natural
;; Produce the total number of owls from consumed list of weights.
(check-expect (count '()) 0)
(check-expect (count (cons 60 '())) (+ 1 0))
(check-expect (count (cons 60 (cons 42 '()))) (+ 1 (+ 1 0)))

;(define (count lon) 0) ;stub

(define (count lon)
  (cond [(empty? lon) 0]
        [else
         (+ 1
            (count (rest lon)))]))
