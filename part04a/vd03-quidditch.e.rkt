#lang htdp/bsl

;
; PROBLEM 1:
;
; Imagine that you are designing a program that will keep track of
; your favorite Quidditch teams. (http://www.internationalquidditch.org/).
;
; Design a data definition to represent a list of Quidditch teams.
;

;; ListOfString is one of:
;; - '()
;; - (cons String ListOfString)
;; Interp. a list of strings.
(define LOS1 '())
(define LOS2 (cons "McGill" '()))
(define LOS3 (cons "UBC" (cons "McGill" '())))

#;
(define (fn-for-los los)
  (cond  [(empty? los) (...)]
         [else
          (... (first los)                  ; String
               (fn-for-los (rest los)))]))  ; ListOfString

;; Template Rules Used:
;; - one of: 2 cases
;;   - atomic distinct: '()
;;   - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString <1>

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE: This last line of the template rules was added in this
; example, and was missing from the previous ones.
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; PROBLEM 2:
;
; We want to know whether your list of favorite Quidditch teams includes
; UBC! Design a function that consumes ListOfString and produces true if
; the list includes "UBC".
                                        ;
;; ListOfString -> Boolean
;; Produce #true if los includes "UBC".
(check-expect (contains-ubc? '()) #false)
(check-expect (contains-ubc? (cons "McGill" '())) #false)
(check-expect (contains-ubc? (cons "UBC" '())) #true)
(check-expect (contains-ubc? (cons "McGill" (cons "UBC" '()))) #true)

;(define (contains-ubc? los) false) ;stub

(define (contains-ubc? los)
  (cond  [(empty? los) #false]
         [else
          (if (string=? (first los) "UBC")      ; String
              #true
              (contains-ubc? (rest los)))]))    ; ListOfString




