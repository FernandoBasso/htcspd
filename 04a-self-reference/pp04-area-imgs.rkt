#lang htdp/bsl
(require 2htdp/image)

;; =================
;; Data definitions:

;
; PROBLEM A:
;
; Design a data definition to represent a list of images. Call it ListOfImage.
;

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; Interp: a list of images
(define LOI1 '())
(define LOI2 (cons (square 20 "solid" "black") '()))
(define LOI3 (cons (square 25 "solid" "black")
                   (cons (rectangle 25 30 "solid" "black")
                         (cons (circle 25 "solid" "black") '()))))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; Template Rules Used:
;; - one of: 2 cases
;;   - atomic distinct: empty
;;   - compound: (cons Image ListOfImage)
;; - self-reference: (rest loi) is ListOfImage


;; =================
;; Functions:

;
; PROBLEM B:
;
; Design a function that consumes a list of images and produces a number
; that is the sum of the areas of each image. For area, just use the image's
; width times its height.
;

;; ListOfImage -> Number
;; Produce the area of all images combined.
(check-expect (sum-areas '()) 0)
(check-expect (sum-areas (cons (square 20 "solid" "black")
                               '()))
              (+ (* 20 20) 0))
(check-expect (sum-areas
               (cons (square 20 "solid" "black")
                     (cons (circle 25 "solid" "black")
                           (cons (rectangle 20 30 "solid" "black")
                                 '()))))
              (+ (* 20 20) (* 50 50) (* 20 30)))

;(define (sum-areas loi) 0) ;stub

(define (sum-areas loi)
  (cond [(empty? loi) 0]
        [else
         (+ (* (image-width (first loi)) (image-height (first loi)))
            (sum-areas (rest loi)))]))


