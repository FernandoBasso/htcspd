#lang htdp/bsl

;; =================
;; Data definitions:

;
; PROBLEM A:
;
; Design a data definition to represent a list of booleans. Call it ListOfBoolean.
;

;; ListOfBoolean is one of:
;; - empty
;; - (cons Boolean ListOfBoolean)
;; Interp: a list of boolean values.
(define LOB1 '())
(define LOB2 (cons #false '()))
(define LOB3 (cons #true (cons #false (cons #false '()))))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))

;; Template Rules Used
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Boolean ListOfBoolean)
;;  - self-reference: (rest lon) is ListOfBoolean

;; =================
;; Functions:

;
; PROBLEM B:
;
; Design a function that consumes a list of boolean values and produces true
; if every value in the list is true. If the list is empty, your function
; should also produce true. Call it all-true?
;

;; ListOfBoolean -> Boolean
;; Prduce #true if all elements are true. If list is empty, also produce #true.
(check-expect (all-true? '()) #true)
(check-expect (all-true? (cons #false '())) #false)
(check-expect (all-true? (cons #true (cons #true '()))) #true)
(check-expect (all-true? (cons #true (cons #true (cons #false '())))) #false)

; (define (all-true? lob) #false) ;stub

; A more beginner-like version.
; (define (all-true? lob)
;   (cond [(empty? lob) #true]
;         [else
;          (if (false? (first lob))
;              #false
;              (all-true? (rest lob)))]))

;; A more elegant and professional version.
(define (all-true? lob)
  (cond [(empty? lob) #true]
        [else
         (and (first lob)
              (all-true? (rest lob)))]))

;; Remember that `(and ...)` returns #false on the first
;; non-truthy value.
