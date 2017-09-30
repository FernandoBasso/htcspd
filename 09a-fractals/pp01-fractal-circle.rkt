#lang htdp/isl
(require 2htdp/image)

;
; PROBLEM :
;
; Design a function that will create the following fractal:
;

(define IMG-FRACT-CIRCLE (bitmap "../imgs/025-fractal-circle.jpg"))

;
; Each circle is surrounded by circles that are two-fifths smaller.
;
; You can build these images using the convenient beside and above functions
; if you make your actual recursive function be one that just produces the
; top leaf shape. You can then rotate that to produce the other three shapes.
;
; You don't have to use this structure if you are prepared to use more
; complex place-image functions and do some arithmetic. But the approach
; where you use the helper is simpler.
;
; Include a termination argument for your design.


;; =================
;; Constants:

(define STEP (/ 2 5))
(define UP (/ 5 2))
(define TRIVIAL-SIZE 5)

;; Number -> Image
;; Produce fractal circle with leafs on all four sides.

;; 1 larger than trivial size.
(check-expect (fractal-circle (/ TRIVIAL-SIZE STEP))
              (local [(define mid (circle (/ TRIVIAL-SIZE STEP) "solid" "blue"))
                      (define leaf (circle TRIVIAL-SIZE "solid" "blue"))]
                (above leaf
                       (beside leaf mid leaf)
                       leaf)))

;; 2 larger than trivial size.
(check-expect (fractal-circle (/ TRIVIAL-SIZE (sqr STEP)))
              (local [(define mid (circle (/ TRIVIAL-SIZE (sqr STEP)) "solid" "blue"))
                      (define submid (circle (/ TRIVIAL-SIZE STEP) "solid" "blue"))
                      (define leaf (circle TRIVIAL-SIZE "solid" "blue"))
                      (define tleaf (above leaf
                                           (beside leaf submid leaf)))]
                (above tleaf
                       (beside (rotate 90 tleaf) mid (rotate -90 tleaf))
                       (rotate 180 tleaf))))


(define (fractal-circle d)
  (local [(define tleaf (draw-leaf (* d STEP)))
          (define mid (circle d "solid" "blue"))]
    (above tleaf
           (beside (rotate 90 tleaf) mid (rotate -90 tleaf))
           (rotate 180 tleaf))))


;; Number -> Image
;; Produce top leaf.

;; No recursion, just the trivial case.
(check-expect (draw-leaf TRIVIAL-SIZE)
              (circle TRIVIAL-SIZE "solid" "blue"))

;; One recursion. Start one step larger than the trivial size.
(check-expect (draw-leaf (/ TRIVIAL-SIZE STEP))
              (local [(define mid (circle (/ TRIVIAL-SIZE STEP) "solid" "blue"))
                      (define leaf (circle TRIVIAL-SIZE "solid" "blue"))]
                (above leaf
                       (beside leaf mid leaf))))

;; Two recursions. Start two steps larger the the trivial size.
(check-expect (draw-leaf (/ TRIVIAL-SIZE (sqr STEP)))
              (local [(define mid (circle (/ TRIVIAL-SIZE (sqr STEP)) "solid" "blue"))
                      (define leaf (draw-leaf (/ TRIVIAL-SIZE STEP)))]
                (above leaf
                       (beside (rotate 90 leaf) mid (rotate -90 leaf)))))

;
; TERMINATION ARGUMENT
; --------------------
;
; TRIVIAL CASE: d <= TRIVIAL-SIZE
; REDUCTION STEP: reduce d by STEP and recurse that to produce leafs.
; ARGUMENT: reduction step makes leaves smaller on each recursio so it
;           will eventually causes d to become <= STEP.
;

(define (draw-leaf d)
  (cond [(<= d TRIVIAL-SIZE) (circle d "solid" "blue")]
        [else
         (local [(define m (circle d "solid" "blue"))
                 (define s (draw-leaf (* d STEP)))]
           (above s
                  (beside (rotate 90 s) m (rotate -90 s))))]))


; Use it like this:
; (fractal-circle 100)

