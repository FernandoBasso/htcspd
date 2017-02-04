#lang htdp/bsl

;
; PROBLEM:
;
; Write two expressions that multiply the numbers 3, 5 and 7.
; The first should take advantage of the fact that * can accept
; more than 2 arguments.
; The second should build up the result by first multiplying
; 3 times 5 and then multiply the result of that by 7.
;

(* 3 5 7)
(* (* 3 5) 7)

;; This would also be possible:
(* 3 (* 5 7))

;; → 105
;; → 105
;; → 105
