#lang htdp/bsl

(require 2htdp/image)

;
; Design a function called decreasing-image that consumes a Natural n
; and produces an image of all the numbers from n to 0 side by side.
;
; So (decreasing-image 3) should produce 3 2 1 0 as an image (with an space
; between the numbers).
;

(define TXT-COLOR "black")
(define TXT-SIZE 24)
(define SPC (text " " TXT-SIZE TXT-COLOR))

;; Natural -> Image
;; Produce image of digits n to 0 side by side.
(check-expect (decreasing-image 0) (text "0" TXT-SIZE TXT-COLOR))
(check-expect (decreasing-image 1) (beside
                                    (text "1" TXT-SIZE TXT-COLOR)
                                    SPC
                                    (text "0" TXT-SIZE TXT-COLOR)))
(check-expect (decreasing-image 3) (beside
                                    (text "3" TXT-SIZE TXT-COLOR)
                                    SPC
                                    (text "2" TXT-SIZE TXT-COLOR)
                                    SPC
                                    (text "1" TXT-SIZE TXT-COLOR)
                                    SPC
                                    (text "0" TXT-SIZE TXT-COLOR)))

;(define (decreasing-image n) (square 0 "solid" "white")) ;stub

(define (decreasing-image n)
  (cond [(zero? n) (text (number->string n) TXT-SIZE TXT-COLOR)]
        [else
         (beside (text (number->string n) TXT-SIZE TXT-COLOR)
                 SPC
                 (decreasing-image (sub1 n)))]))

