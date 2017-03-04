#lang htdp/bsl

;; ListOfNumber is one of:
;; - '()
;; - (cons Number ListOfNumber)
;; Interp: a list of numbers.
(define LON1 '())
(define LON2 (cons 1 '()))
(define LON3 (cons 2 (cons 1 '())))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template Rules Used:
;; - one of: 2 cases
;;   - atomic distinct: empty
;;   - compound: (cons Number ListOfNumber)
;; - <the one we are yet to study>


;
; PROBLEM:
; Design a function that consumes a list of numbers and
; produces #true if that list contains a negative number.
;

;; ListOfNumber -> Boolean
;; Produce #true if a lon contains a negative number.
(check-expect (contains-negative? '()) #false)
(check-expect (contains-negative? (cons 1 '())) #false)
(check-expect (contains-negative? (cons 1 (cons -1.5 '()))) #true)

;(define (contains-negative? lon) #false) ;stub

;<took template from ListOfNumber>
(define (contains-negative? lon)
  (cond [(empty? lon) #false]
        [else
         (if (negative? (first lon))
             #true
             (contains-negative? (rest lon)))]))

