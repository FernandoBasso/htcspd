#lang htdp/bsl

;
; Design a data definition for a traffic light that can either be
; disabled, or be one of red, yellow, or green.
;

;; DATA DEFINITIONS

;; TLight is one of:
;;  - #false
;;  - "red"
;;  - "yellow"
;;  - "green"
;; Interp: - #false means the traffic light is disabled
;;         - otherwise the color of the light
(define TL1 #false)
(define TL2 "red")

(define (fn-for-tlight tl)
  (cond [(false? tl) (...)]
        [(string=? tl "red") (...)]
        [(string=? tl "yellow") (...)]
        [(string=? tl "green") (...)]))

;; Template rules used:
;;  - atomic distinct: #false
;;  - atomic distinct: "red"
;;  - atomic distinct: "yellow"
;;  - atomic distinct: "green"

