#lang htdp/isl
(require 2htdp/image)

;
; PROBLEM:
;
; Use the built in version of filter to design a function called wide-only
; that consumes a list of images and produces a list containing only those
; images that are wider than they are tall.
;


(define I1 (rectangle 20 10 "solid" "red")) ;; Wide.
(define I2 (rectangle 10 20 "solid" "red")) ;; Not wide.
(define I3 (rectangle 30 30 "solid" "green")) ;; Not wide.
(define I4 (rectangle 45 44 "solid" "magenta")) ;; Wide.

;; (listof Image) -> (listof Image)
;; Produce list of images with those for which wide? produce #t.
(check-expect (wide-only empty) empty)
(check-expect (wide-only (list I2 I3)) empty)
(check-expect (wide-only (list I1 I4)) (list I1 I4))
(check-expect (wide-only (list I1 I2 I3 I4)) (list I1 I4))

(define (wide-only loi)
  (filter wide? loi))

;; Image -> Boolean
;; Produce #t if img w > img h; #f otherwise.
(define (wide? img)
  (> (image-width img) (image-height img)))

