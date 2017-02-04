#lang htdp/bsl

;
; PROBLEM:
;
; Design a function that pluralizes a given word. (Pluralize means to
; convert the word to its plural form.) For simplicity you may assume
; that just adding s is enough to pluralize a word.
;

;; String -> String
;; Produce the given string with “s” added to the end.
(check-expect (pluralize "dog") "dogs")
(check-expect (pluralize "grass") "grasss")

;(define (pluralize str) "") ;stub

#;
(define (pluralize str) ;template
  (... str))

(define (pluralize str)
  (string-append str "s"))

