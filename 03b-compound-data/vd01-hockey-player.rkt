#lang htdp/bsl

;
; PROBLEM:
;
; Design a data definition to represent hockey players, including both
; their first and last names.
;

(define-struct player (fn ln))
;; Player is (make-player String String).
;; Interp: (make-player fn ln) is a hockey player with
;;           - fn: first name
;;           - ln: last name
(define P1 (make-player "Bobby" "Orr"))
(define P2 (make-player "Wayne" "Gretzky"))

(define (fn-for-player p)
  (... (player-fn p)      ; String
       (player-ln p)))    ; String

;; Template rules used:
;;  - compound: 2 fields





