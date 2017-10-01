#lang htdp/isl
(require 2htdp/image)
(require 2htdp/universe)


;
; PROBLEM:
;
; A Cantor Set is another fractal with a nice simple geometry.
; The idea of a Cantor set is to have a bar (or rectangle) of
; a certain width w, then below that are two recursive calls each
; of 1/3 the width, separated by a whitespace of 1/3 the width.
;
; So this means that the
;   width of the whitespace   wc  is  (/ w 3)
;   width of recursive calls  wr  is  (/ (- w wc) 2)
;
; To make it look better a little extra whitespace is put between
; the bars.
;
;
; Here are a couple of examples (assuming a reasonable CUTOFF)
;

(define IMG-CANTOR-SET-EXAMPLES (bitmap "../imgs/026-cantor-set.png"))

;
;
; PROBLEM A:
;
; Design a function that consumes a width and produces a cantor set of
; the given width.
;
;
; PROBLEM B:
;
; Add a second parameter to your function that controls the percentage
; of the recursive call that is white each time. Calling your new function
; with a second argument of 1/3 would produce the same images as the old
; function.
;
; PROBLEM C:
;
; Now you can make a fun world program that works this way:
;   The world state should simply be the most recent x coordinate of the mouse.
;
;   The to-draw handler should just call your new cantor function with the
;   width of your MTS as its first argument and the last x coordinate of
;   the mouse divided by that width as its second argument.
;


;; A world program for running a Cantor Set.
;; The mouse position controls parameters of the fractal.

;; =========================================
;; CONSTANTS:

(define W 400)
(define H 600)
(define LIM 4)
(define BH 20) ; bar height.
(define SPC-H (/ BH 2)) ; spacing bar height.


;; =========================================
;; DATA DEFINITIONS:

;; WorldState is Number
;; interp. the last x-coordinate of the mouse
(define WS1 100)
(define WS2 4)
#;
(define (fn-for-ws ws)
  (... ws))


;; =========================================
;; FUNCTIONS:

;; WorldState -> WorldState
;; Run the interactive Cantor Set generator;
;; Call: (main 0)
;; <no testes for the main function>
(define (main ws)
  (big-bang ws
            (on-draw render)
            (on-mouse handle-mouse)))

;; WorldState -> Image
;; Render Cantor Set based on current mouse position.
(check-expect (render 100) (cantor W (/ 100 W)))
(check-expect (render 300) (cantor W (/ 300 W)))

(define (render ws)
  (cantor W (/ ws W)))

;; WorldState Integer Integer MouseEvent -> WorldState
;; update the current ws according to mouse position
(check-expect (handle-mouse 0   0 10 "move") 0)
(check-expect (handle-mouse 100 0 10 "move") 0)
(check-expect (handle-mouse  20 0 10 "button-down") 20)

; <template according to MouseEvent>
(define (handle-mouse ws x y mevt)
  (cond [(mouse=? mevt "move") x]
        [else ws]))

;; Natural Natural -> Image
;; Render Cantor Set:
;; -> first line has width w;
;; -> second line center white band has width (* w r)
(check-expect (cantor 0 0.5)
              (mkb 0))

(check-expect (cantor 32 0)
              (above (mkb 32)
                     (mks 32)
                     (mkb 32)
                     (mks 32)
                     (mkb 32)
                     (mks 32)
                     (mkb 32)))

(check-expect (cantor 32 1)
              (above (mkb 32)
                     (mks 32)
                     (mkspc 32)))

(check-expect (cantor 32 .5)
              (above (mkb 32)
                     (mks 32)
                     (beside
                      (above (mkb 8)
                             (mks 8)
                             (beside
                              (mkb 2)
                              (mkspc 4)
                              (mkb 2)))
                      (rectangle 16 BH "solid" "white")
                      (above (mkb 8)
                             (mks 8)
                             (beside
                              (mkb 2)
                              (mkspc 4)
                              (mkb 2))))))

;
; TERMINATION ARGUMENT:
; --------------------
;   trivial case: w <= CUTOFF
;   reduction step: (/ (- w (* r w)) 2)
;   argument: as long as CUTOFF is > 0, and 0 <= r < 1, repeatedly multiplying w by r
;   (the reduction step) will reduce w to eventually reach the base case.
;

; <template from generative recursion>
(define (cantor w r)
  (if (<= w LIM)
      (rectangle w BH "solid" "blue")
      (local [(define wc  (* w r))            ;width of center white band
              (define ws  (/ (- w wc) 2))     ;width of side blue bands
              (define ctr (rectangle wc BH "solid" "white"))
              (define l/r (cantor ws r))]
        (above (mkb w)
               (mks w)
               (beside l/r ctr l/r)))))

(define (mkb w)
  (rectangle w BH "solid" "blue"))

(define (mkspc w)
  (rectangle w BH "solid" "white"))

(define (mks w)
  (rectangle w SPC-H "solid" "white"))

