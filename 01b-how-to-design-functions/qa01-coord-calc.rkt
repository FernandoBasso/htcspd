#lang htdp/bsl

(define WIDTH 100)
(define HEIGHT 200)

(define (half-tw x)
  (+ x (/ (- WIDTH x) 2)))

;; Number -> Number
;; Given a y coordinate, produce y coord half way
;; between it and HEIGHT.
(check-expect (half-th  0) (/ HEIGHT 2))
(check-expect (half-th 20) (+ 20 (/ (- HEIGHT 20) 2)))

(define (half-th y)
  (+ y (/ (- HEIGHT y) 2)))

