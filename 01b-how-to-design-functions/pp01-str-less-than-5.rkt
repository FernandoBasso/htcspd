#lang htdp/bsl

;
; PROBLEM:
;
; DESIGN function that consumes a string and determines whether its
; length is less than 5.
;

;; String -> Boolean
;; Produce #true if string length is less than 5.
(check-expect (str-less-than-5? "") #true)
(check-expect (str-less-than-5? "Hey!") #true)
(check-expect (str-less-than-5? "What‽") #false)
(check-expect (str-less-than-5? "The Force Is Strong With This One!") #false)

;(define (str-less-than-5? str) #false) ;stub

#;
(define (str-less-than-5? str) ;template
  (... str))

(define (str-less-than-5? str)
  (< (string-length str) 5))

