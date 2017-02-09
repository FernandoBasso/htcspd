#lang htdp/bsl
(require 2htdp/image)

;; Design a function that consumes two images and produces true if the
;; first is larger than the second.
;;
;; What do they mean by “larger”?

;; Solution starts here
;; --------------------
;; Image, Image -> Boolean
;; Produce #t if both width and height of first image are larger than
;; width and height of the second image. Rectangles only.

;;
;; TESTS
;;

;; Needs 9 tests
;;
;;     W I D T H
;;   H |                   | first img smaller | same size | first img larger |
;;   E |-------------------+-------------------+-----------+------------------|
;;   I | first img smaller | false             | false     | false            |
;;   G |                   | < <               | = <       | > <              |
;;   H |-------------------+-------------------+-----------+------------------|
;;   T | same size         | false             | false     | false            |
;;     |                   | < =               | = =       | > =              |
;;     |-------------------+-------------------+-----------+------------------|
;;     | first img larger  | false             | false     | true             |
;;     |                   | < >               | = >       | > >              |


;; < <
(check-expect (image-larger?
               (rectangle 20 25 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #false)
;; < =
(check-expect (image-larger?
               (rectangle 20 30 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #false)
;; < >
(check-expect (image-larger?
               (rectangle 20 35 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #false)
;; = <
(check-expect (image-larger?
               (rectangle 25 25 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #false)
;; = =
(check-expect (image-larger?
               (rectangle 25 30 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #false)
;; = >
(check-expect (image-larger?
               (rectangle 25 35 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #false)
;; > <
(check-expect (image-larger?
               (rectangle 30 25 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #false)
;; > =
(check-expect (image-larger?
               (rectangle 35 30 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #false)
;; > >
(check-expect (image-larger?
               (rectangle 30 35 "solid" "blue")
               (rectangle 25 30 "solid" "blue")) #true)

#;
(define (image-larger? img1 img2) #f)

#;
(define (image-larger? img1 img2)    ;template
  (... img1 img2))

(define (image-larger? img1 img2)
  (and (> (image-width img1) (image-width img2))
       (> (image-height img1) (image-height img2))))




