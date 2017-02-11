#lang htdp/bsl
(require 2htdp/image)

;
; You are told that the extension you are working on can only
; accomodate icons that are at most 30 pixels high, so we would like
; to design a function that checks if an image has a height of more
; than 30 pixels.
;

;; DATA DEFINITION

;; Icon is Image
;; Interp: the image of an icon.
(define RUNNING-STICKMAN-ICON (bitmap "./imgs/015-ic-running-stick-person.jpg"))
(define STEP-ICON (bitmap "./imgs/016-ic-next-step.jpg"))

#;
(define (fn-for-icon i)
  (... i))

;; Template rules used:
;;  - atomic non-distinct: Image

;; FUNCTIONS

;; Icon -> Boolean
;; Produce #true if the given icon is more than 30 pixels high.
(check-expect (too-tall? STEP-ICON) #false)
(check-expect (too-tall? RUNNING-STICKMAN-ICON) #true)

;(define (too-tall? i) #false)

(define (too-tall? i)
  (> (image-height i) 25))

; ( . Y . )
;  \     /

