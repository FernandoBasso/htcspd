#lang htdp/isl
(require 2htdp/image)

(define CUTOFF 2)

;
; Three part termination argument.
; --------------------------------
;
; Base case:
;
;   (<= s CUTOFF)
;
;
; Reduction step:
;
;   (/ s 2)
;
;
; Argument that repeated application of reduction step will eventually
; reach the base case:
;
;   As long as the cutoff is > 0 and s starts >= 0, repeated division
;   by 2 will eventually be less than cutoff.
;


;; Number -> Image
;; Produce a Sierpinski Triangle of given size.
(check-expect (stri CUTOFF) (triangle CUTOFF "outline" "red"))
(check-expect (stri (* CUTOFF 2))
              (overlay (triangle (* 2 CUTOFF) "outline" "red")
                       (local [(define sub1 (triangle CUTOFF "outline" "red"))]
                         (above sub1
                                (beside sub1 sub1)))))

(define (stri s)
  (cond [(<= s CUTOFF) (triangle s "outline" "red")]
        [else
         (overlay (triangle s "outline" "red")
                  (local [(define sub (stri (/ s 2)))]
                    (above sub
                           (beside sub sub))))]))


;; Natural -> Image
;; Produce box with white box inside.

;; Test the TRIVIAL case.
(check-expect (scarp CUTOFF) (square CUTOFF "outline" "red"))

;; Test the case where it is "1" bigger than the cut-off point.
;; There will be one recursion here.
(check-expect (scarp (* CUTOFF 3))
              (overlay (square (* CUTOFF 3) "outline" "red")
                       (local [(define sub (square CUTOFF "outline" "red"))
                               (define blk (square CUTOFF "solid" "white"))]
                         (above (beside sub sub sub)
                                (beside sub blk sub)
                                (beside sub sub sub)))))


;
; THREE PART TERMINATION ARGUMENT FOR SCARPET
; -------------------------------------------
;
; Base case:
;
;   (<= s CUTOFF)
;
;
; Reduction step:
;
;   (/ s 3)
;
;
; Argument that repeated application of reduction step will eventually
; reach the base case:
;
;   As long as the cutoff is > 0 and s starts >= 0, repeated division
;   by 3s will eventually be less than cutoff.
;



;; Correct solution. Compare with the incorrect one.
(define (scarp s)
  (cond [(<= s CUTOFF) (square s "outline" "red")]
        [else
         (overlay (square s "outline" "red")
                  (local [(define sub (scarp (/ s 3)))
                          (define blk (square (/ s 3) "solid" "white"))]
                    (above (beside sub sub sub)
                           (beside sub blk sub)
                           (beside sub sub sub))))]))

