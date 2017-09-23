#lang htdp/isl
(require 2htdp/image)

;
; PROBLEM:
;
; Design a function that consumes a number and produces a Sierpinski
; triangle of that size. Your function should use generative recursion.
;
; One way to draw a Sierpinski triangle is to:
;
;  - start with an equilateral triangle with side length s
;
;  ;  - inside that triangle are three more Sierpinski triangles
;  ;
;  - and inside each of those... and so on
;
; So that you end up with something that looks like this:
;

 (define IMG1 (bitmap "../imgs/023-sierpinski-triangle.png"))

;
; Note that in the 2nd picture above the inner triangles are drawn in
; black and slightly smaller just to make them clear. In the real
; Sierpinski triangle they should be in the same color and of side
; length s/2. Also note that the center upside down triangle is not
; an explicit triangle, it is simply formed from the other triangles.
;

(define CUTOFF 2)

;; Number -> Image
;; Produce a Sierpinski Triangle of given size.
(check-expect (stri CUTOFF) (triangle CUTOFF "outline" "red"))
(check-expect (stri (* CUTOFF 2))
              (overlay (triangle (* 2 CUTOFF) "outline" "red")
                       (local [(define sub1 (triangle CUTOFF "outline" "red"))]
                         (above sub1
                                (beside sub1 sub1)))))
#;
(define (genrec-fn d)
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d
              (genrec-fn (next-problem d)))]))

(define (stri s)
  (cond [(<= s CUTOFF) (triangle s "outline" "red")]
        [else
         (overlay (triangle s "outline" "red")
                  (local [(define sub (stri (/ s 2)))]
                    (above sub
                           (beside sub sub))))]))


;
; PROBLEM:
;
; Design a function to produce a Sierpinski carpet of size s.
;
; Here is an example of a larger Sierpinski carpet.
;
(define IMG2 (bitmap "../imgs/024-sierpinski-carpet.png"))



; You make a square, and inside a square you put 8 copies of and
; a blank square in the middle.
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

;(define (scarp s) (square CUTOFF "outline" "red")) ;stub

;; Incorrect...
#;
(define (scarp s)
  (cond [(<= s CUTOFF) (square s "outline" "red")]
        [else
         (overlay (square s "outline" "red")
                  (local [(define sub (square (/ s 3) "outline" "red"))
                          (define blk (square (/ s 3) "solid" "white"))]
                    (above (beside sub sub sub)
                           (beside sub blk sub)
                           (beside sub sub sub)))
                  (scarp (/ s 3)))]))

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

;; TODO:
;; (scarp 150) produces a strange result, like, correct but looks
;;             like there are lines instead of boxes.
;; (scarp 200) produces a more good-looking thing. Why‽

