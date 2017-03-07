#lang htdp/bsl

;
; parlor trick: an amusing trick or pastime to entertain house guests.
;

;
; PROBLEM:
;
; Your friend has just given you a new pad, and it runs a prototype version of Racket.
;
; This is great, you can make it do anything. There's just one problem, this version of
; racket doesn't include numbers as primitive data. There just are no numbers in it!
;
; But you need natural numbers to write your next program.
;
; No problem you say, because you remember the well-formed self-referential data definition
; for Natural, as well as the idea that add1 is kind of like cons, and sub1 is kind of like
; rest. Your idea is to make add1 actually be cons, and sub1 actually be rest...
;

;; NATURAL is one of:
;;  - empty
;;  - (cons "!" NATURAL)
;; Interp: a natural number, the number of "!" in the list is the number.
(define N0 empty)         ; 0
(define N1 (cons "!" N0)) ; 1
(define N2 (cons "!" N1)) ; 2
(define N3 (cons "!" N2)) ; 3
(define N4 (cons "!" N3)) ; 3
(define N5 (cons "!" N4)) ; 3
(define N6 (cons "!" N5)) ; 3
(define N7 (cons "!" N6)) ; 3
(define N8 (cons "!" N7)) ; 3
(define N9 (cons "!" N8)) ; 3

;; These are the primitives that operate on this new kind of NATURAL.

(define (ZERO? n) (empty? n))       ;; Any         -> Boolean
(define (ADD1  n) (cons "!" n))     ;; NATURAL     -> NATURAL
(define (SUB1  n) (rest n))         ;; NATURAL[>0] -> NATURAL

#;
(define (fn-for-NATURAL n)
  (cond [(ZERO? n) (...)]
        [else
         (... n
              (fn-for-NATURAL (SUB1 n)))]))

;; Template rules used:
;;  - one of: 2 casees
;;  - atomic distinct: 0
;;  - compound: (cons "!" NATURAL)
;;  - self-reference: (rest n) is NATURAL

;; NATURAL NATURAL -> NATURAL
;; Produce a + b.
(check-expect (ADD N2 N0) N2)
(check-expect (ADD N0 N3) N3)
(check-expect (ADD N3 N4) N7)

;(define (ADD a b) N0) ;stub

(define (ADD a b)
  (cond [(ZERO? b) a]
        [else
         ;; Add 1 to a, take 1 away from be until be becomes zero.
         (ADD (ADD1 a)
              (SUB1 b))]))

;; NATURAL NATURAL -> NATURAL
;; Produce a - b. Assume a is always greater than b.
(check-expect (SUB N3 N0) N3)
(check-expect (SUB N8 N5) N3)

;(define (SUB a b) N0) ;stub

(define (SUB a b)
  (cond [(ZERO? b) a]
        [else
         ;; Take 1 from b and from a everytime until b is zero.
         (SUB (SUB1 a)
              (SUB1 b))]))

;; NATURAL NATURAL -> NATURAL
;; Produce a * b.
(check-expect (TIMES N0 N2) N0)
(check-expect (TIMES N3 N0) N0)
(check-expect (TIMES N1 N9) N9)
(check-expect (TIMES N3 N2) N6)

;(define (TIMES a b) N0) ;stub

(define (TIMES a b)
  (ADD-REPEATEDLY a b N0))

;; NATURA NATURAL NATURAL -> NATURAL
;; Add b to c a times.
(check-expect (ADD-REPEATEDLY N0 N3 N4) N4)
(check-expect (ADD-REPEATEDLY N2 N1 N3) N5)
(check-expect (ADD-REPEATEDLY N3 N3 N0) N9)

;(define (ADD-REPEATEDLY a b c) N0) ;stub

;; Template on a.
(define (ADD-REPEATEDLY a b c)
  (cond [(ZERO? a) c]
        [else
         (ADD-REPEATEDLY (SUB1 a)
                         b
                         (ADD b c))]))

;; NATURAL -> NATURAL
;; Produce N * N - 1 ... 1 (factorial).
(check-expect (FACT N0) N1) ;; By definition 0! is 1.
(check-expect (FACT N1) N1) ;; By definition 1! is 1.
(check-expect (FACT N3) N6)

;(define (FACT n) N0) ;stub

(define (FACT n)
  (cond [(ZERO? n) N1]
        [else
         (TIMES n (FACT (SUB1 n)))]))

