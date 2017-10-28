#lang htdp/isl+

;; Fetches `list-ref`, `take` and `drop`.
(require racket/list)
(require 2htdp/image)

;
; PROBLEM 1:
;
; In the lecture videos we designed a function to make a Sierpinski triangle fractal.
;
; Here is another geometric fractal that is made of circles rather than triangles:
;

(define IMG1 (bitmap "../imgs/027-fractal-circle.jpg"))

;
; Design a function to create this circle fractal of size n and colour c.
;


(define CUT-OFF 5)

;; Natural String -> Image
;; produce a circle fractal of size n and colour c
(check-expect (circle-fractal CUT-OFF "red")
              (circle CUT-OFF "outline" "red"))

(check-expect (circle-fractal (* CUT-OFF 2) "red")
              (local [(define inner (circle CUT-OFF "outline" "red"))]
                (overlay (beside inner inner)
                         (circle (* CUT-OFF 2) "outline" "red"))))

;
; TERMINATION ARGUMENT:
; ---------------------
;
; TRIVIAL CASE:   (<= n CUT-OFF)
; REDUCTION STEP: (/ n 2)
; ARGUMENT:       As long as CUT-OFF > 0 and n > 0, repeated division of n
;                 by 2 will eventually cause n to be less than CUT-OFF.
;

(define (circle-fractal n c)
  (cond [(<= n CUT-OFF) (circle n "outline" c)]
        [else
         (local [(define inner (circle-fractal (/ n 2) c))]
         (overlay (circle n "outline" c)
                  (beside inner inner)))]))

