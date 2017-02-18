#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

;
; PROBLEM:
;
; Design an animation of a traffic light.
;
; Your program should show a traffic light that is red, then green,
; then yellow, then red etc. For this program, your changing world
; state data definition should be an enumeration.
;

;; =========
;; CONSTANTS

;; Radius of one light circle.
(define RADIUS 20)

;; We need some padding between lights and between the
;; lights and their containing box.
(define PAD-SIZE (/ RADIUS 4))

;; Outer box width, with space for some padding.
(define WIDTH (* (+ RADIUS PAD-SIZE) 2))

;; Radius is half the height of the light circle. We need its
;; full height (that is why we multiply it by 2), and we need
;; room for three lights, plus 4 paddings between the lights
;; and its containing box.
(define HEIGHT (+ (* (* RADIUS 3) 2) (* PAD-SIZE 4)))

(define BG-COLOR "black")

;; The surrounding box around the three lights.
(define BOX (rectangle WIDTH HEIGHT "solid" BG-COLOR))

;; The pad to space things nicely.
(define PAD (rectangle WIDTH PAD-SIZE "solid" BG-COLOR))

(define RED-ON (overlay
                (above (circle RADIUS "solid" "red")
                       PAD
                       (circle RADIUS "outline" "yellow")
                       PAD
                       (circle RADIUS "outline" "green"))
                BOX))

(define YELLOW-ON (overlay
                (above (circle RADIUS "outline" "red")
                       PAD
                       (circle RADIUS "solid" "yellow")
                       PAD
                       (circle RADIUS "outline" "green"))
                BOX))

(define GREEN-ON (overlay
                (above (circle RADIUS "outline" "red")
                       PAD
                       (circle RADIUS "outline" "yellow")
                       PAD
                       (circle RADIUS "solid" "green"))
                BOX))


;; ================
;; DATA DEFINITIONS

;; LightState is one of:
;;  - "red"
;;  - "yellow"
;;  - "green"
;; Interp: the color of a traffic light.

;; <examples are redundant for enumeration>

#;
(define (fn-for-light-state ls)
  (cond [(string=? "red" ls) (...)]
        [(string=? "yellow" ls) (...)]
        [(string=? "green" ls) (...)]))

;; Template rules used:
;;  - one of: 3 cases
;;  - atomic distinct: "red"
;;  - atomic distinct: "yellow"
;;  - atomic distinct: "green"


;; =================================
;; A SIMPLE TRAFFIC LIGHT ANIMATION!
;; =================================

;; =========
;; FUNCTIONS

;; LightState -> Image
;; Start the world with (main "red").
(define (main ls)
  (big-bang ls                                ; LightState
            (on-tick next-light 1)              ; LightState -> LightState
            (to-draw render-trafficlight)))   ; LightState -> Image


;; LightState -> LightState
;; Produce next light of traffice light.
(check-expect (next-light "red") "green")
(check-expect (next-light "green") "yellow")
(check-expect (next-light "yellow") "red")

;(define (next-light ls) "red") ;stub

;<took template from LightState>
(define (next-light ls)
  (cond [(string=? "red" ls) "green"]
        [(string=? "yellow" ls) "red"]
        [(string=? "green" ls) "yellow"]))


;; LightState -> Image
;; Render trafficlight with specific light on.
(check-expect (render-trafficlight "red") RED-ON)
(check-expect (render-trafficlight "yellow") YELLOW-ON)
(check-expect (render-trafficlight "green") GREEN-ON)

;(define (render-trafficlight ls) MTS) ;stub

;<took template from LightState>

(define (render-trafficlight ls)
  (cond [(string=? "red" ls) RED-ON]
        [(string=? "yellow" ls) YELLOW-ON]
        [(string=? "green" ls) GREEN-ON]))

