#lang htdp/bsl

;
; Using the CityName data definition below design a function
; that produces true if the given city is the best in the world.
; (You are free to decide for yourself which is the best city
; in the world.)
;

;; DATA DEFINITIONS:

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

;; FUNCTIONS:

;; CitiName -> Boolean
;; Produce #true if the consumed city is Dagobah.
(check-expect (best? "Boston") #false)
(check-expect (best? "Dagobah") #true)

;(define (best? cn) #false) ;stub

;; Took template from CityName.
(define (best? cn)
  (string=? cn "Dagobah"))

