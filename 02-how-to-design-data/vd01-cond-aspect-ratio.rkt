#lang htdp/bsl
(require 2htdp/image)

(define IMG1 (rectangle 10 20 "solid" "red"))
(define IMG2 (rectangle 20 20 "solid" "red"))
(define IMG3 (rectangle 20 10 "solid" "red"))

;; Image -> String
;; Produces string describing shape of image: "tall", "square" or "wide".
(check-expect (aspect-ratio IMG1) "tall")
(check-expect (aspect-ratio IMG2) "square")
(check-expect (aspect-ratio IMG3) "wide")

;(define (aspect-ratio img) "")    ;stub

;(define (aspect-ratio img)        ;template
;  (... img))

#;
(define (aspect-ratio img)
  (if (> (image-height img) (image-width img))
      "tall"
      (if (= (image-height img) (image-width img))
          "square"
          "wide")))

;; The false answer is inside another if... This nesting works but
;; doesn’t fit the logic of the tests. We mean 3 “parallel” tests, not
;; nested ones... ~cond~ to the rescue.

(define (aspect-ratio img)
  (cond [(> (image-height img) (image-width img)) "tall"]
        [(= (image-height img) (image-width img)) "square"]
        [else "wide"]))


