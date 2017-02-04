#lang htdp/bsl

;
; PROBLEM:
;
; DESIGN a function called yell that consumes strings like "hello"
; and adds "!" to produce strings like "hello!".
;
; Remember, when we say DESIGN, we mean follow the recipe.
;
; Leave behind commented out versions of the stub and template.
;


;; String -> String
;; Add “!” to the end of str.
(check-expect (yell "Hello") "Hello!")
(check-expect (yell "bye") "bye!")
(check-expect (yell "!") "!!")

;(define (yell str) "")    ;stub

;(define (yell str)         ;template
;  (... str))

(define (yell str)
  (string-append str "!"))
