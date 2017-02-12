#lang htdp/bsl
(require 2htdp/image)


;; DATA DEFINITIONS

;
; PROBLEM A:
;
; You are designing a program to track a rocket's journey as it descends
; 100 kilometers to Earth. You are only interested in the descent from
; 100 kilometers to touchdown. Once the rocket has landed it is done.
;
; Design a data definition to represent the rocket's remaining descent.
; Call it RocketDescent.
;

;; RocketDescent is Interval[1, 100]
;; Interp: Number of kilometers remaining for touchdown. 0 means on the ground.
(define RD1 100) ;; 100 KM above ground (start tracking).
(define RD2 50)  ;; 50 KM above ground.
(define RD3 0)   ;; 0 KM above ground. Rocket has landed.

#;
(define (fn-for-rock-descent rd)
  (... rd))

;; Template rules used:
;;  - atomic non-distinct: Integer[0, 100]

;; FUNCTIONS

;
; PROBLEM B:
;
; Design a function that will output the rocket's remaining descent distance
; in a short string that can be broadcast on Twitter.
; When the descent is over, the message should be "The rocket has landed!".
; Call your function rocket-descent-to-msg.
;

;; RocketDescent -> String
;; Produce string with descent status of the rocket.
(check-expect (rocket-descent-to-msg 100)
              "100 KM for touchdown.")
(check-expect (rocket-descent-to-msg 3)
              "3 KM for touchdown.")
(check-expect (rocket-descent-to-msg 0)
              "The rocket has landed!")

;(define (rocket-descent-to-msg rd) "") ;stub

(define (rocket-descent-to-msg rd)
  (if (= rd 0)
      "The rocket has landed!"
      (string-append (number->string rd)
                     " KM for touchdown.")))

