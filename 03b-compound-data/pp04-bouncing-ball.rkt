#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

;
;  Draw a rectangle about 500x350 Uppon running the program, a ball with
; radius 10px will move around and bounce upon hitting the sides.
;
; Then, depeding on which arrow key is pressed reverse the direction of
; the ball accordingly unless the ball is already touching the walls
; or the direction key pressed is already in the direction that the
; ball is already heading to.
;
; DESIGN DECISION 1: When the ball touches any of the edges of the
; canvas, just invert the velocity keeping the ball still for that one
; tick. In other words, just invert sx and/or sy, but don't update x and
; y.
;
; DESIGN DECISION 2: If the ball starts with x = 0, the ball would be
; half inside the left edge because the center of the ball would be at
; x.  If the ball is moving right, no problem. Just let it dradually go
; out of the wall. But when doing collision detection, if the ball would
; pass one of the edges, then position it in such a way that only the
; ball edges touch the wall, preventing it from traversing the "walls".
;

;; ===========================
;; Constants:
(define CW 500)   ; canvas width
(define CH 400)   ; canvas height

(define MTS (empty-scene CW CH))

(define BALL-R 40)
(define BALL-R+ BALL-R)
(define BALL-R- (- BALL-R))
(define B (circle BALL-R "solid" "magenta"))


;; ============================
;; Data Definitions:
(define-struct ball (x y sx sy))
;; interp. Ball is (make-ball x y dx dy) with x,y coordinages and sx,sy velocity.
;;     x and y are the center x and y of the ball.
;;     sx and sy are in pixels for tick.

;; Top-left corner, moving right and down.
(define B1 (make-ball 0 0 3 3))
;; In the "middle", moving left and up.
(define B2 (make-ball 30 50 -3 -3))
;; In the middle, moving right and up.
(define B3 (make-ball 50 40 3 -3))
;; In the middle, moving left and down.
(define B4 (make-ball 100 100 -3 3))

;#
(define (fn-for-ball b)
  (... (ball-x b)
       (ball-y b)
       (ball-sx b)
       (ball-sy b)))

;; Template rules used:
;;  - Compound: 4 fields.

;; =======================
;; Functions:
;; Ball -> Ball
;; Start world with initial state ball, i.e. (main (make-ball BALL-R BALL-R 3 3))
(define (main b)
  (big-bang b
            (on-tick next-ball)        ; Ball -> Ball
            (on-key handle-key)        ; Ball -> Ball
            (to-draw render-ball)))    ; Ball -> Image


;; Ball -> Ball
;; Update Ball's x,y direction and sx,sy speed.
(check-expect (next-ball (make-ball 0 0 3 3)) (make-ball 3 3 3 3))
(check-expect (next-ball (make-ball 50 50 3 3)) (make-ball 53 53 3 3))
(check-expect (next-ball (make-ball CW CH 3 3)) (make-ball (- CW BALL-R) (- CH BALL-R) -3 -3))
(check-expect (next-ball (make-ball 0 0 -3 -3)) (make-ball BALL-R BALL-R 3 3))
(check-expect (next-ball (make-ball 0 70 -3 -3)) (make-ball BALL-R 67 3 -3))
(check-expect (next-ball (make-ball 70 0 -3 -3)) (make-ball 67 BALL-R -3 3))

;(define (next-ball b) b)    ;stub

; <took template from Ball>
(define (next-ball b)
  (make-ball (calc-ball-x b) (calc-ball-y b) (calc-ball-sx b) (calc-ball-sy b)))


;; Ball -> Integer
;; Calculate next postion for ball-x depending on direction and distance from edges.
(check-expect (calc-ball-x (make-ball 0 0 -3 3)) BALL-R)
(check-expect (calc-ball-x (make-ball 2 0 -3 3)) BALL-R)
(check-expect (calc-ball-x (make-ball 40 0 -3 3)) BALL-R)
(check-expect (calc-ball-x (make-ball 50 0 -3 3)) 47)
(check-expect (calc-ball-x (make-ball CW 0 3 3)) (- CW BALL-R))
(check-expect (calc-ball-x (make-ball (- CW 2) 0 3 3)) (- CW BALL-R))
(check-expect (calc-ball-x (make-ball (- CW 40) 0 3 3)) (- CW BALL-R))
(check-expect (calc-ball-x (make-ball (- CW 55) 0 3 3)) (- CW 55 -3))

;(define (calx-ball-x b) 0)    ;stub

; <took template from Ball>
(define (calc-ball-x b)
  ;; If moving left (negative speed).
  (if (< (ball-sx b) 0)
      (if (< (+ (ball-x b) (ball-sx b) BALL-R-) 0)
          BALL-R+
          (+ (ball-x b) (ball-sx b)))
      ;; Then we are moving right (positive speed).
      (if (> (+ (ball-x b) (ball-sx b) BALL-R+) CW)
          (+ CW BALL-R-)
          (+ (ball-x b) (ball-sx b)))))


;; Ball -> Integer
;; Calculate next postion for ball-y depending on direction and distance from edges.
(check-expect (calc-ball-y (make-ball 0 0 3 -3)) BALL-R)
(check-expect (calc-ball-y (make-ball 0 2 3 -3)) BALL-R)
(check-expect (calc-ball-y (make-ball 0 40 3 -3)) BALL-R)
(check-expect (calc-ball-y (make-ball 0 50 3 -3)) 47)
(check-expect (calc-ball-y (make-ball 0 CH 3 3)) (- CH BALL-R))
(check-expect (calc-ball-y (make-ball 0 (- CH 2) 3 3)) (- CH BALL-R))
(check-expect (calc-ball-y (make-ball 0 (- CH 40) 3 3)) (- CH BALL-R))
(check-expect (calc-ball-y (make-ball 0 (- CH 54) 3 3)) (- CH 54 -3))

;(define (calx-ball-y b) 0)    ;stub

; <took template from Ball>
(define (calc-ball-y b)
  ;; If moving top (negative speed).
  (if (< (ball-sy b) 0)
      (if (< (+ (ball-y b) (ball-sy b) BALL-R-) 0)
          (+ 0 BALL-R+)
          (+ (ball-y b) (ball-sy b)))
      ;; Then we are moving bottom (positive speed).
      (if (> (+ (ball-y b) (ball-sy b) BALL-R+) CH)
          (+ CH BALL-R-)
          (+ (ball-y b) (ball-sy b)))))


;; Ball -> Integer
;; Produce positive or negative x speed based on the current x/sx.
(check-expect (calc-ball-sx (make-ball 0         0 -3 3))  3)
(check-expect (calc-ball-sx (make-ball 2         0 -3 3))  3)
(check-expect (calc-ball-sx (make-ball 40        0 -3 3))  3)    ;; Remember ball radius.
(check-expect (calc-ball-sx (make-ball 50        0 -3 3)) -3)
(check-expect (calc-ball-sx (make-ball CW        0  3 3)) -3)
(check-expect (calc-ball-sx (make-ball (- CW 2)  0  3 3)) -3)
(check-expect (calc-ball-sx (make-ball (- CW 40) 0  3 3)) -3)    ;; Remember ball radius.
(check-expect (calc-ball-sx (make-ball (- CW 50) 0  3 3))  3)

;(define (calc-ball-sx b) 0)    ;stub

; <took template from Ball>
(define (calc-ball-sx b)
  (cond [(and (< (ball-sx b) 0)
              (< (+ (ball-x b) (ball-sx b) BALL-R-) 0))
         (- (ball-sx b))]
        [(and (> (ball-sx b) 0)
              (> (+ (ball-x b) (ball-sx b) BALL-R+) CW))
         (- (ball-sx b))]
        [else (ball-sx b)]))



;; Ball -> Integer
;; Produce positive or negative y speed based on the current y/sy.
(check-expect (calc-ball-sy (make-ball 0 0         3  -3))  3)
(check-expect (calc-ball-sy (make-ball 0 2         3  -3))  3)
(check-expect (calc-ball-sy (make-ball 0 40        3  -3))  3)    ;; Remember ball radius.
(check-expect (calc-ball-sy (make-ball 0 50        3  -3)) -3)
(check-expect (calc-ball-sy (make-ball 0 CH        3  3))  -3)
(check-expect (calc-ball-sy (make-ball 0 (- CH 2)  3  3))  -3)
(check-expect (calc-ball-sy (make-ball 0 (- CH 40) 3  3))  -3) ;; Remember ball radius.
(check-expect (calc-ball-sy (make-ball 0 (- CH 50) 3  3))   3)

;(define (calc-ball-sy b) 0)    ;stub

; <took template from Ball>
(define (calc-ball-sy b)
  (cond [(and (< (ball-sy b) 0)
              (< (+ (ball-y b) (ball-sy b) BALL-R-) 0))
         (- (ball-sy b))]
        [(and (> (ball-sy b) 0)
              (> (+ (ball-y b) (ball-sy b) BALL-R+) CH))
         (- (ball-sy b))]
        [else (ball-sy b)]))


;; Ball -> Ball
;; Reverse speed of ball depending on ball current speed, postion and which key was pressed.

;; If the ball is touching any wall, no mater which key is pressed or which direction the
;; ball is heading to, don't reverse the speed.
(check-expect (handle-key (make-ball 0 0 3 3) "right") (make-ball 0 0 3 3))
(check-expect (handle-key (make-ball CW 0 -3 3) "left") (make-ball CW 0 -3 3))
(check-expect (handle-key (make-ball 0 0 3 3) "down") (make-ball 0 0 3 3))
(check-expect (handle-key (make-ball CH 0 3 -3) "up") (make-ball CH 0 3 -3))

;; Change speed if ball is somewhere in the middle, and moving in a direction
;; that is contrary to the key that was pressed.
(check-expect (handle-key (make-ball 100 0 3 3) "right") (make-ball 100 0 3 3))
(check-expect (handle-key (make-ball 100 0 3 3) "left") (make-ball 100 0 -3 3))
(check-expect (handle-key (make-ball 100 0 -3 3) "right") (make-ball 100 0 3 3))
(check-expect (handle-key (make-ball 100 0 -3 3) "left") (make-ball 100 0 -3 3))
(check-expect (handle-key (make-ball 0 80 3 3) "down") (make-ball 0 80 3 3))
(check-expect (handle-key (make-ball 0 80 3 3) "up") (make-ball 0 80 3 -3))
(check-expect (handle-key (make-ball 0 80 3 -3) "down") (make-ball 0 80 3 3))
(check-expect (handle-key (make-ball 0 80 3 -3) "up") (make-ball 0 80 3 -3))

;<took template from KeyEvent>
(define (handle-key b ke)
  (cond [(and (not (touching-x? b))
              (key=? ke "right"))
         (if (< (ball-sx b) 0)
             (make-ball (ball-x b) (ball-y b) (- (ball-sx b)) (ball-sy b))
             b)]
        [(and (not (touching-x? b))
              (key=? ke "left"))
         (if (> (ball-sx b) 0)
             (make-ball (ball-x b) (ball-y b) (- (ball-sx b)) (ball-sy b))
             b)]
        [(and (not (touching-y? b))
              (key=? ke "down"))
         (if (< (ball-sy b) 0)
             (make-ball (ball-x b) (ball-y b) (ball-sx b) (- (ball-sy b)))
             b)]
        [(and (not (touching-y? b))
              (key=? ke "up"))
         (if (> (ball-sy b) 0)
             (make-ball (ball-x b) (ball-y b) (ball-sx b) (- (ball-sy b)))
             b)]
        [else b]))


;(define (handle-key b ke) b)    ;stub


;; Ball -> Boolean
;; Produce #true if ball is touching left or right sides.
(check-expect (touching-x? (make-ball 0 60 3 3)) #true)
(check-expect (touching-x? (make-ball CW 60 3 3)) #true)
(check-expect (touching-x? (make-ball 80 60 3 3)) #false)

;(define (touching-x? b) #false)    ;stub

;<took template from Ball>
(define (touching-x? b)
  (or (<= (- (ball-x b) BALL-R) 0)
      (>= (+ (ball-x b) BALL-R) CW)))

;; Ball -> Boolean
;; Produce #true if ball is touching top or bottom sides.
(check-expect (touching-y? (make-ball 60 0 3 3)) #true)
(check-expect (touching-y? (make-ball 60 CH 3 3)) #true)
(check-expect (touching-y? (make-ball 60 60 3 3)) #false)

;(define (touching-y? b) #false)    ;stub

;<took template from Ball>
(define (touching-y? b)
  (or (<= (- (ball-y b) BALL-R) 0)
      (>= (+ (ball-y b) BALL-R) CH)))


;; Ball -> Image
;; Place ball on MTS at x,y coords.

;; top-left, going right and down.
(check-expect (render-ball (make-ball 0 0 3 3))
              (place-image B 3 3 MTS))
;; bottom-right, going left and up.
(check-expect (render-ball (make-ball CW CH -3 -3))
              (place-image B (+ CW -3) (+ CH -3) MTS))
;; middle, going left and down.
(check-expect (render-ball (make-ball 50 50 -3 3))
              (place-image B 47 53 MTS))
;; middle, going right and up.
(check-expect (render-ball (make-ball 50 50 3 -3))
              (place-image B 53 47 MTS))

;(define (render-ball b) MTS)    ;stub

; <took template from Ball>
(define (render-ball b)
  (place-image B (+ (ball-x b) (ball-sx b)) (+ (ball-y b) (ball-sy b)) MTS))

