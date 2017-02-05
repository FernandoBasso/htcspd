#lang htdp/bsl
(require 2htdp/image)

;
; DESIGN a function that consumes an image and determines whether the
; image is tall.
;

;; Image -> Boolean
;; Produce true if the image is tall (height is greater than width).
(check-expect (tall? (rectangle 20 30 "solid" "red")) #true)
(check-expect (tall? (rectangle 30 20 "solid" "red")) #false)
(check-expect (tall? (rectangle 25 25 "solid" "red")) #false)

;(define (tall? img) #false) ;stub

#;
(define (tall? img) ;template
  (... img))

;(define (tall? img)
;  (if (> (image-height img) (image-width img))
;      #true
;      #false))

(define (tall? img)
  (> (image-height img) (image-width img)))

