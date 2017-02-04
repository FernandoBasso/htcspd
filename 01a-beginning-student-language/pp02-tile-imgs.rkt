#lang htdp/bsl
(require 2htdp/image)

;
; PROBLEM:
;
; Use the DrRacket square, beside and above functions to create
; an image like this one:
;
;   "./imgs/tiles.jpg"
;
; If you prefer to be more creative feel free to do so. You can
; use other DrRacket image functions to make a more interesting
; or more attractive image.
;                                         ;

(above (beside (square 20 "solid" "blue")
               (square 20 "solid" "yellow"))
       (beside (square 20 "solid" "yellow")
               (square 20 "solid" "blue")))


