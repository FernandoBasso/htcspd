#lang htdp/isl
(require 2htdp/image)

;; Some setup data and functions to enable more interesting examples below

(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (rectangle 30 20 "solid" "yellow"))
(define I3 (rectangle 40 50 "solid" "green"))
(define I4 (rectangle 60 50 "solid" "blue"))
(define I5 (rectangle 90 90 "solid" "orange"))

(define LOI1 (list I1 I2 I3 I4 I5))


;; NOTE: Unlike using-built-ins-starter.rkt this file does not define
;; the functions tall? wide? square? and area.


;
; PROBLEM:
;
; Complete the design of the following functions by completing the body
; which has already been templated to use a built-in abstract list function.


;; (listof Image) -> (listof Image)
;; produce list of only those images that have width >= height
(check-expect (wide-only (list I1 I2 I3 I4 I5)) (list I2 I4))

;(define (wide-only loi) empty) ;stub

(define (wide-only loi)
  (local [(define (wide? i)
            (> (image-width i)
               (image-height i)))]
    ;; filter passes the image to wide?.
    (filter wide? loi)))


;; Number (listof Image) -> (listof Image)
;; produce list of only those images in loi with width >= w
(check-expect (wider-than-only 40 LOI1) (list I4 I5))

;(define (wider-than-only w loi) empty) ;stub

(define (wider-than-only w loi)
  (local [(define (wider-than? i)
            ;; w is accessed from the parent scope. It works
            ;; because of lexical scoping.
            (> (image-width i) w))]
    (filter wider-than? loi)))

;; wider-than? is a closure: it "closes over" the value of w
;; passed to wider-than-only.

;; (listof Number) -> (listof Number)
;; produce list of each number in lon cubed
(check-expect (cube-all (list 1 2 3)) (list (* 1 1 1) (* 2  2 2) (* 3 3 3)))

;(define (cube-all lon) empty) ;stub

(define (cube-all lon)
  (local [(define (cube n)
            (* n n n))]
  (map cube lon)))


;; String (listof String) -> (listof String)
;; produce list of all elements of los prefixed by p
(check-expect (prefix-all "accio " (list "portkey" "broom"))
              (list "accio portkey" "accio broom"))

;(define (prefix-all p los) empty) ;stub

(define (prefix-all p los)
  (local [(define (string-prepend s)
            (string-append p s))]
  (map string-prepend los)))


;; From qa.
(define (rotate-all loi n)
  (local [(define (rotate-n i)
            (rotate n i))]
  (map rotate-n loi)))

