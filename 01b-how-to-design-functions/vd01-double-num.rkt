#lang htdp/bsl

;
; PROBLEM:
;
; Design a function that consumes a number and produce twice that number.
; Call your function double. Follow the HtDF recipe and show the
; stub and template.
;

;; Number -> Number
;; Produces 2 times the given number.
(check-expect (double 3) (* 2 3))
(check-expect (double 4.2) (* 2 4.2))

;(define (double n) 0) ;stub

#;
(define (double n) ;template
  (... n))

(define (double n)
  (* 2 n))

