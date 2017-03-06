;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname devel.dr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; Write a function called list-from-to that takes in two numbers, the
; first greater than or equal to the sencond, and produces a list of
; each number from the first to the second.
;
; For example, (list-from-to 10 7) should produce
;
;    (cons 10 (cons 9 (cons 8 (cons 7 empty))))
;

;; Natural Natural -> ListOfNatural
;; Given n and a, n >= a, produce the list (cons n (cons n - 1 ... (cons a empty))).
(check-expect (list-from-to 5 5) (cons 5 '()))
(check-expect (list-from-to 4 1) (cons 4 (cons 3 (cons 2 (cons 1 '())))))

;(define (list-from-to n a) '()) ;stub

(define (list-from-to n a)
  (cond [(zero? (- n a)) (cons a '())]
        [else
         (cons n (list-from-to (sub1 n) a))]))

