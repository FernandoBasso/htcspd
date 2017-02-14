#lang htdp/bsl

;; Age is Natural
;; interp. the age of a person in years
(define A0 18)
(define A1 25)

#;
(define (fn-for-age a)
  (... a))

;; Template rules used:
;;  - atomic non-distinct: Natural


;; PROBLEM #1
;; ----------
;; Consider the above data definition for the age of a person.
;;
;; Design a function called teenager? that determines whether a person
;; of a particular age is a teenager (i.e., between the ages of 13 and 19).

;; Age -> Boolean
;; produce true if a person is a teenager (age between 13 and 19), false otherwise
(check-expect (teenager? 12) false)
(check-expect (teenager? 13) true)
(check-expect (teenager? 16) true)
(check-expect (teenager? 19) true)
(check-expect (teenager? 20) false)

;(define (teenager? a) false)    ;stub

;; <use template from Age>
(define (teenager? a)
  (<= 13 a 19))


;; PROBLEM #2
;; ==========
;; Design a data definition called MonthAge to represent a person's age
;; in months.

;; MonthAge is Natural
;; interp. the age of a person in months
(define AM1 23)
(define AM2 132)
(define AM3 324)

#;
(define (fn-for-month-age ma)
  (... ma))

;; Template rules used:
;;  - atomic non-distinct: Natural


;; PROBLEM #3
;; ==========
;; Design a function called months-old that takes a person's age in years
;; and yields that person's age in months.

;; Age -> MonthAge
;; produce age in months from input Age in years
(check-expect (months-old 1) 12)
(check-expect (months-old 16) (* 16 12))
(check-expect (months-old 38) (* 38 12))

;(define (months-old a) 1)    ;stub

;; <use template frm Age>
(define (months-old a)
  (* a 12))


;; PROBLEM #4
;; ==========

;; Consider a video game where you need to represent the health of your
;; character. The only thing that matters about their health is:
;;
;;   - if they are dead (which is shockingly poor health)
;;   - if they are alive then they can have 0 or more extra lives
;;
;; Design a data definition called Health to represent the health of your
;; character.
;;
;; Design a function called increase-health that allows you to increase the
;; lives of a character.  The function should only increase the lives
;; of the character if the character is not dead, otherwise the character
;; remains dead.


;; Health is one of:
;;  - false
;;  - Natural
;; interp.
;;    false   - character is dead
;;    Natural - character has zero or more extra lives
(define H1 false)    ; character is dead
(define H2 0)        ; almost dead
(define H3 5)        ; character has 5 lives


(define (fn-for-health h)
  (cond [(boolean? h) (...)]
        [else (... h)]))

;; Template rules used:
;;  - one of: two cases
;;    - atomic distinct: false
;;    - atomic non-distinct: Natural

;; Health -> Health
;; produce health + 1 if input is a Natural, false if character is dead (input is false)
(check-expect (increase-health false) false)
(check-expect (increase-health 0) 1)
(check-expect (increase-health 5) 6)

;(define (increase-health h) false)    ;stub

;; <use template from Health>
(define (increase-health h)
  (cond [(boolean? h) false]
        [else (+ h 1)]))

