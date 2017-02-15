#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)


;
; PROBLEM:
;
; Use the How to Design Worlds recipe to design an interactive
; program in which a cat starts at the left edge of the display
; and then walks across the screen to the right. When the cat
; reaches the right edge it should just keep going right off
; the screen.
;
; Once your design is complete revise it to add a new feature,
; which is that pressing the space key should cause the cat to
; go back to the left edge of the screen. When you do this, go
; all the way back to your domain analysis and incorporate the
; new feature.
;
; To help you get started, here is a picture of a cat, which we
; have taken from the 2nd edition of the How to Design Programs
; book on which this course is based.
;

;; A cat that walks from left to right across the screen.

;; =========
;; CONSTANTS

(define CAT-IMG (bitmap "./imgs/017-cat.jpg"))
(define WIDTH 600)
(define HEIGHT 400)
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define SPEED 3)


;; ================
;; DATA DEFINITIONS

;; Cat is Number
;; Interp: x position of the cat in screen coords.
(define C1 0)           ;; left edgle
(define C2 (/ WIDTH 2)) ;; middle
(define C3 WIDTH)       ;; right edge

#;
(define (fn-for-cat c)
  (... c))

;; Template rules used:
;;  - atomic non-distinct: Number


;; =================
;; Functions:

;; Cat -> Cat
;; Start the world with (main 0).
(define (main c)
  (big-bang c                           ; Cat
            (on-tick   advance-cat)     ; Cat -> Cat
            (to-draw   render-cat)      ; Cat -> Image
            (on-key    handle-key)))    ; Cat KeyEvent -> Cat

;; Cat -> Cat
;; Produce the next cat, by advancing it 1 px to the right.
(check-expect (advance-cat 3) (+ 3 SPEED))

;(define (advance-cat c) 0) ;stub

;<use template from Cat>
(define (advance-cat c)
  (+ c SPEED))


;; Cat -> Image
;; Render the cat image at appropriate place on MTS.
(check-expect (render-cat 4) (place-image CAT-IMG 4 CTR-Y MTS))

;(define (render-cat c) MTS) ;stub

;<use template from Cat>
(define (render-cat c)
  (place-image CAT-IMG c CTR-Y MTS))

;; Cat KeyEvent -> Cat
;; Reset cat to left edge when space key is pressed.
(check-expect (handle-key 10 " ")  0)
(check-expect (handle-key 10 "a") 10)
(check-expect (handle-key  0 " ")  0)
(check-expect (handle-key  0 "a")  0)

;(define (handle-key c ke) 0) ;stub

#;
(define (handle-key c ke)
  (cond [(key=? ke " ") (... c)]
        [else
         (... c)]))

(define (handle-key c ke)
  (cond [(key=? ke " ") 0]
        [else c]))

