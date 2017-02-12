#lang htdp/bsl

;
; PROBLEM:
;
; Using the SeatNum data definition below design a function
; that produces true if the given seat number is on the aisle.
;

;; DATA DEFINITIONS:

;; SeatNum is Natural[1, 32]
;; Interp: Seat numbers in a row, 1 and 32 are aisle seats.
(define SN1  1) ;aisle
(define SN2 12) ;middle
(define SN3 32) ;aisle

#;
(define (fn-for-seat-num sn)
  (... sn))

;; Template rules used:
;;  atomic non-distinct: Natural[1, 32]

;; FUNCTIONS:

;; SeatNum -> Boolean
;; Produce #true if given seat number is on the aisle.
(check-expect (aisle? 1) #true)
(check-expect (aisle? 15) #false)
(check-expect (aisle? 32) #true)

;(define (aisle? sn) #false) ;stub

;<used template from SeatNum>
(define (aisle? sn)
  (or (= sn 1)
      (= sn 32)))

