#lang htdp/bsl
(require 2htdp/image)

;
; PROBLEM:
;
; DESIGN a function called image-area that consumes an image and produces the
; area of that image. For the area it is sufficient to just multiple the image's
; width by its height.
;

;; Image -> Natural
;; Produce image's width * height (area).
(check-expect (image-area (rectangle 2 3 "solid" "red")) (* 2 3))

;(define (image-area img) 0) ;stub

#;
(define (image-area img) ;template
  (... img))

(define (image-area img)
  (* (image-width img) (image-height img)))
