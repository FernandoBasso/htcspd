#lang htdp/bsl
(require 2htdp/image)

(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (rectangle 20 10 "solid" "blue"))

(if (< (image-width I1) (image-height I1))
    "Image is tall."
    "Image is wide.")

(if (< (image-width I2) (image-height I2))
    "Image is tall."
    "Image is wide.")

; → "Image is tall."
; → "Image is wide."
