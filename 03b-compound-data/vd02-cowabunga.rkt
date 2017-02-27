#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

;; =========
;; CONSTANTS

(define WIDTH 400)
(define HEIGHT 200)

(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT))

(define RCOW (bitmap "./imgs/019-rcow.jpg"))
(define LCOW (bitmap "./imgs/020-lcow.jpg"))
(define RCOW- (rotate -2 RCOW))
(define RCOW+ (rotate  2 RCOW))
(define LCOW- (rotate -2 LCOW))
(define LCOW+ (rotate  2 LCOW))


;; ================
;; DATA DEFINITIONS
(define-struct cow (x dx))
;; Cow is (make-cow Natural[0, WIDTH] Integer)
;; Interp: (make-cow x dx) is a cow with x corrd and dx velocity
;;         x is the center of the cow
;;         x is in screen coords (pixels)
;;         dx is in pixels per tick.
(define C1 (make-cow 10  3))    ; at 10, moving left to right.
(define C2 (make-cow 20 -4))    ; at 20, moving right to left.

(define (fn-for-cow c)
  (... (cow-x c)     ; Natural[0, WIDTH]
       (cow-dx c)))  ; Integer

;; Template rules used:
;;  - compound: 2 fields.


;; =========
;; FUNCTIONS

;; Cow -> Cow
;; Called to make the cow go for a walk;
;; Start with (main (make-cow 0 3)).
;; No tests for the main function
(define (main c)
  (big-bang c
            (on-tick next-cow)                  ; Cow -> Cow
            (to-draw render-cow)                ; Cow -> Image
            (on-key  handle-key)))              ; Cow KeyEvent -> Cow

;; Cow -> Cow
;; Increase cow x by dx; bounce. of edges.

;; In the middle, moving right, just keep moving right.
(check-expect (next-cow (make-cow 20  3)) (make-cow (+ 20 3) 3))
;; In the middle, moving left, just keep moving left.
(check-expect (next-cow (make-cow 20 -3)) (make-cow (- 20 3) -3))

;; Reaches right edge. That is OK.
(check-expect (next-cow (make-cow (- WIDTH 3) 3)) (make-cow WIDTH 3))
;; Reaches left edge. That is OK.
(check-expect (next-cow (make-cow 3 -3)) (make-cow 0 -3))

;; Would pass the right edge. Nono! Turn around.
(check-expect (next-cow (make-cow (- WIDTH 2) 3)) (make-cow WIDTH -3))
;; Would pass the left edge. What the poop‽ Turn around right now!
(check-expect (next-cow (make-cow 2 -3)) (make-cow 0 3))

;(define (next-cow c) c) ;stub

; <took template from Cow>
(define (next-cow c)
  (cond
    ;; Would the cow pass right edge if it just kept going?
    [(> (+ (cow-x c) (cow-dx c)) WIDTH)
     (make-cow WIDTH (- (cow-dx c)))]
    ;; Would the cow pass the left edge if it just kept going?
    [(< (+ (cow-x c) (cow-dx c)) 0)
     (make-cow 0 (- (cow-dx c)))]
    ;; OK. Then just let the cow do whatever it was already doing.
    [else
     (make-cow (+ (cow-x c) (cow-dx c)) (cow-dx c))]))


;; Cow -> Image
;; Place appropriate cow image on MTS at (cow-x c) and CTR-Y.
(check-expect (render-cow (make-cow 99 3))
              (place-image RCOW- 99 CTR-Y MTS))
(check-expect (render-cow (make-cow 33 -3))
              (place-image LCOW- 33 CTR-Y MTS))

;(define (render-cow c) MTS) ;stub
; <took template from Cow>
(define (render-cow c)
  (place-image (choose-image c) (cow-x c) CTR-Y MTS))

;; Cow -> Image
;; Produce [L|R]COW[+|-] depending on the direction cow is going.
(check-expect (choose-image (make-cow 1  3)) RCOW-)
(check-expect (choose-image (make-cow 2  3)) RCOW+)
(check-expect (choose-image (make-cow 1 -4)) LCOW-)
(check-expect (choose-image (make-cow 2 -4)) LCOW+)

;(define (choose-image c) RCOW) ;stub

; <took template from Cow>
(define (choose-image c)
  (if (> (cow-dx c) 0)
      (if (odd? (cow-x c)) RCOW- RCOW+)
      (if (odd? (cow-x c)) LCOW- LCOW+)))


;; Cow KeyEvent -> Cow
;; Reverse direction of cow travel when space bar is pressed.
(check-expect (handle-key (make-cow 99 3) " ") (make-cow 99 -3))
(check-expect (handle-key (make-cow 50 -3) " ") (make-cow 50 3))
(check-expect (handle-key (make-cow 39 -3) "Z") (make-cow 39 -3))

;(define (handle-key c ke) c) ;stub

;<template according to KeyEvent)
(define (handle-key c ke)
  (cond [(key=? ke " ") (make-cow (cow-x c) (- (cow-dx c)))]
        [else c]))

