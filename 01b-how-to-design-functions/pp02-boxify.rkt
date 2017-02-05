#lang htdp/bsl
(require 2htdp/image)

;
; PROBLEM:
;
; Use the How to Design Functions (HtDF) recipe to design a function
; that consumes an image, and appears to put a box around it. Note
; that you can do this by creating an "outline" rectangle that is
; bigger than the image, and then using overlay to put it on top of
; the image. For example:
;
; (boxify (ellipse 60 30 "solid" "red")) should produce:
;
;    "./imgs/pp-boxify-output-example.jpg"
;

;; Image -> Image
;; Add a "box" around given image.
(check-expect (boxify (ellipse 60 30 "solid" "magenta"))
              (overlay (ellipse 60 30 "solid" "magenta")
                       (rectangle 70 40 "outline" "black")))

;(define (boxify img) (square 0 "solid" "white")) ;stub

#;
(define (boxify img) ;template
  (... img))

(define (boxify img)
  (overlay img
           ;; We decided to always add a 10px padding
           ;; to the containing box.
           (rectangle (+ (image-width img) 10)
                      (+ (image-height img) 10)
                      "outline"
                      "black")))

