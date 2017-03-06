#lang htdp/bsl

;
; Now that we understand how to form data definitions for abitrary-sized data we can
; look at how to design functions that "count down" on natural numbers.  Here's the
; key inight - ask yourself how many natural numbers are there?
;
; A lot, many... an arbitrary number:
;
; 0                      ;0
; (add1 0)               ;1
; (add1 (add1 0))        ;2
; (add1 (add1 (add1 0))) ;3
;
; and so on
;
; What that is saying is that (add1 <some natural>) produces a natural,
; similarly (sub1 <some natural other than 0>) produces a natural.
;
; So add1 is kind of like cons, it takes a natural and makes a bigger one
; (cons makes a longer list). And sub1 is kind of like rest it takes a natural
; (other than 0) and gives the next smallest one (rest gives shorter list).
;
;
; Consider this type comment:
;


;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... ;n                       ;Template rules wouldn't normally put this
          ;                            ;here, but we will see that we end up coming
          ;                            ;back to add it.
          (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 0
;;  - compound: (add1 Natural)
;;  - self-reference: (sub1 n) is Natural

;; ListOfNatural is one of:
;;  - empty
;;  - (cons Natural empty))
;; Interp: A list of Natural.
(define LON1 '())
(define LON2 (cons 3 '()))
(define LON3 (cons 5 (cons 2 (cons 0 '()))))

;; !!! TODO: Is this template and rules used correct‽
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) ...]
        [else
         (... (fn-for-natural (first lon))
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - reference: (first lon) is Natural
;;  - self-reference (rest lon) is ListOfNatural

;
; PROBLEMs:
;
; (A) Design a function that consumes Natural number n and produces the sum of all
;     the naturals in [0, n].
;
; (B) Design a function that consumes Natural number n and produces a list of all
;     the naturals of the form (cons n (cons n-1 ... empty)) not including 0.
;

;; Natural -> Natural
;; Produce sum of Natural[0, n].
;; Always test the base-case first.
(check-expect (sum 0) 0)
(check-expect (sum 1) 1)
;; Always test one that is at least 2-long.
(check-expect (sum 3) (+ 3 2 1 0))

;(define (sum n) 0) ;stub

(define (sum n)
  (cond [(zero? n) 0]
        [else
         (+ n
            (sum (sub1 n)))]))


;; Natural -> ListOfNatural
;; Produce (cons n (cons n - 1 ... empty)), not including zero.
(check-expect (to-list 0) '())
(check-expect (to-list 1) (cons 1 '()))
(check-expect (to-list 2) (cons 2 (cons 1 '())))

;(define (to-list n) '()) ;stub

(define (to-list n)
  (cond [(zero? n) '()]
        [else
         (cons n
               (to-list (sub1 n)))]))

