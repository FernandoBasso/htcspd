#lang htdp/bsl

;
; PROBLEM: design a function that consumes two lists of strings and produces true
; if the first list is a prefix of the second. Prefix means that the elements of
; the first list match the elements of the second list 1 for 1, and the second list
; is at least as long as the first.
;
; For reference, the ListOfString data definition is provided below.
;

;; =================
;; Data Definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings

(define LS0 empty)
(define LS1 (cons "a" empty))
(define LS2 (cons "a" (cons "b" empty)))
(define LS3 (cons "c" (cons "b" (cons "a" empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; ==========
;; Functions:

;; ListOfString ListOfString -> Boolean
;; Produce #t if lstA is a prefix of lstB.
(check-expect (prefix=? empty empty) #t)
(check-expect (prefix=? (list "x") empty) #f)
(check-expect (prefix=? empty (list "x")) #t)

(check-expect (prefix=? (list "x") (list "x")) #t)
(check-expect (prefix=? (list "x") (list "y")) #f)

(check-expect (prefix=? (list "x" "y") (list "x" "y")) #t)
(check-expect (prefix=? (list "x" "x") (list "x" "y")) #f)

(check-expect (prefix=? (list "x") (list "x" "y")) #t)

(check-expect (prefix=? (list "x" "y" "z") (list "x" "y")) #f)

;(define (prefix=? la lb) #f) ;stub

(define (prefix=? la lb)
  (cond [(empty? la) #t]
        [(empty? lb) #f]
        ;; We know we have both lists here and they are both still conses.
        [else (and (string=? (first la) (first lb))
                   (prefix=? (rest la) (rest lb)))]))

