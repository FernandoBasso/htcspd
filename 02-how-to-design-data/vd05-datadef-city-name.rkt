#lang htdp/bsl

;
; Imagine that you are designing a program that, among other things,
; has information about the names of cities in its problem domain.
;
; Design a data definition to represent the name of a city.
;

;
; This is not part of the template. It was just used as part of the
; process to help understand the steps.
; | INFORMATION | DATA        |
; |-------------+-------------|
; | Vancouver   | "Vancouver" |
; | Boston      | "Boston"    |
;

;; CityName is String
;; Interp: the name of a city.
(define CN1 "Boston")
(define CN2 "Vancouver")

(define (fn-for-city-name cn)
  (... cn))

;; Template rules used:
;; - atomic non-distinct: String

