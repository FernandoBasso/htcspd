#lang htdp/bsl

;
; PROBLEM:
;
; DESIGN a function called area that consumes the length of one side
; of a square and produces the area of the square.
;
; Remember, when we say DESIGN, we mean follow the recipe.
;
; Leave behind commented out versions of the stub and template.
;

;; Number -> Number
;; Given length of one side of square, produce the area of the square.
(check-expect (area 3) (* 3 3))
(check-expect (area 3.2) (* 3.2 3.2))

;(define (area s) 0) ;stub

#;
(define (area s) ;template
  (... s))

(define (area s)
  (* s s))
