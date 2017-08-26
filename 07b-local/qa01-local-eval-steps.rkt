#lang htdp/isl

(define (foo x)
  (local [(define (bar y) (+ x y))]
    (+ x (bar (* 2 x)))))

(list (foo 2) (foo 3))

(list
 (local [(define (bar y) (+ 2 y))]
   (+ 2 (bar (* 2 2))))
 (foo 3))

;; The three steps in one for local.
(define (bar_0 y) (+ 2 y))
(list
 (+ 2 (bar_0 (* 2 2)))
 (foo 3))

(list
 (+ 2 (bar_0 4))
 (foo 3))

(list
 (+ 2 (+ 2 4))
 (foo 3))

(list (+ 2 6) (foo 3))

(list 8 (foo 3))

(list 8
      (local [(define (bar y) (+ 3 y))]
        (+ 3 (bar (* 2 3)))))

;; the 3 steps in one:
(define (bar_1 y) (+ 3 y))
(list 8
      (+ 3 (bar_1 (* 2 3))))

(list 8
      (+ 3 (bar_1 6)))

(list 8 (+ 3 (+ 3 6)))

(list 8 (+ 3 9))

(list 8 12)
