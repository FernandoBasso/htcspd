#lang htdp/bsl

;
; PROBLEM:
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

(define (fn-for-los los)
  (cond  [(empty? los) (...)]
         [else
          (... (first los)    ; String
               (fn-for-los (rest los)))]))  ; ListOfString

;; Template Rules Used:
;; - one of: 2 cases
;;   - atomic distinct: '()
;;   - compound: (cons String ListOfString)
;; - <(fn-for-los (rest los)) is a rule that will be seen later>

