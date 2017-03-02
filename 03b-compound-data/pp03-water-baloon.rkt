#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

;; A simple water baloon animation.

;; ===============
;; == CONSTANTS ==
;; ===============
(define BALOON-IMG (bitmap "../imgs/021-baloon.jpg"))
(define WIDTH 600)
(define HEIGHT WIDTH)
(define CTR-Y (/ HEIGHT 2))
(define LINEAR-SPEED 3)  ; For X speed.
(define ANGULAR-SPEED 2) ; For angle of rotation speed.
(define MTS (empty-scene WIDTH HEIGHT))



;; ======================
;; == DATA DEFINITIONS ==
;; ======================

(define-struct bs (x a))
;; BaloonState is (make-baloon Number Number).
;; Interp: (make-player x a) is a baloon state with:
;;         - x position in screen coordinates (pixels);
;;         - a is the angle of rotation.
(define BS1 (make-bs 10 0))
(define BS2 (make-bs 50 230))

#;
(define (fn-for-baloon-state bs)
  (... (bs-x bs)
       (bs-a bs)))

;; Template rules used:
;; - compound: 2 fields.


;; ===============
;; == FUNCTIONS ==
;; ===============

;; BaloonState -> Baloon State
;; Start the world with (main (make-bs 0 0)).
(define (main bs)
  (big-bang bs                                 ; BaloonState
            (on-tick next-bs)                  ; BaloonState -> BaloonState
            (to-draw render-animation)         ; BaloonState -> Image
            (on-key  handle-key)))             ; BaloonState KeyEvent -> BaloonState

;; BaloonState -> BaloonState
;; Produce next baloon state with x and a values.
(check-expect (next-bs (make-bs 0 0)) (make-bs (+ 0 LINEAR-SPEED) (+ 0 ANGULAR-SPEED)))

;(define (next-bs bs) bs) ;stub

;<took template from BaloonState>
(define (next-bs bs)
  (make-bs (+ (bs-x bs) LINEAR-SPEED) (+ (bs-a bs) ANGULAR-SPEED)))

;; BaloonState Image
;; Produce baloon image on right position and angle on MTS.
(check-expect (render-animation (make-bs 0 0))
              (place-image (rotate 0 BALOON-IMG) 0 CTR-Y MTS))
(check-expect (render-animation (make-bs 50 392))
              (place-image (rotate (modulo 392 360) BALOON-IMG) 50 CTR-Y MTS))

;(define (render-animation bs) MTS) ;stub

;<took template from BaloonState>
(define (render-animation bs)
  (place-image (rotate (modulo (bs-a bs) 360) BALOON-IMG)
               (bs-x bs)
               CTR-Y
               MTS))

;; BaloonState KeyEvent -> BaloonState
;; Re-start the program when space bar is pressed.
(check-expect (handle-key (make-bs 180 200) " ") (make-bs 0 0))
(check-expect (handle-key (make-bs 95 136) "Z") (make-bs 95 136))

;(define (handle-key bs ke) bs);

; Template according to KeyEvent
(define (handle-key bs ke)
  (cond [(key=? " " ke) (make-bs 0 0)]
        [else bs]))

