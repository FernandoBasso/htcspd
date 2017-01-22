#lang htdp/bsl

;
; PROBLEM 1:
;
; Design a data definition for a list of Number.
;

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; Interp: a list of numbers.
(define LON1 '())
(define LON2 (cons -5.3 '()))
(define LON3 (cons 13 (cons -4.1 (cons 100 '()))))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template Rules Used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Number ListOfNumber)
;; - self-reference (rest lon) is ListOfNumber

;
; PROBLEM 2:
;
; Design a function that consumes a list of numbers and doubles every
; number in the list. Call it double-all.
;

;; ListOfNumber -> ListOfNumber
;; Doubles each number in consumed list.
(check-expect (double-all '()) '())
(check-expect (double-all (cons -3.14 '()))
              (cons (* -3.14 2) '()))
(check-expect (double-all (cons 4 (cons -9.1 (cons 100 '()))))
              (cons (* 4 2)
                    (cons (* -9.1 2)
                          (cons (* 100 2) '()))))

;(define (double-all lon) '()) ;stub

(define (double-all lon)
  (cond [(empty? lon) '()]
        [else
         (cons (* (first lon) 2)
               (double-all (rest lon)))]))
