#lang htdp/isl+

;
; PROBLEM:
;
; Design a function that consumes a list of elements and produces the list
; consisting of only the 1st, 3rd, 5th and so on elements of its input.
;
;    (skip1 (list "a" "b" "c" "d")) should produce (list "a" "c")
;

;; (listof X) -> (listof X)
;; Produce list consisting of only 1st, 3rd, 5th... elements of lox.
;; If you skip1 of '(), what do you get back‽ You get '().
(check-expect (skip1 empty) empty)
;; Normal cases...
(check-expect (skip1 (list "a" "b" "c" "d")) (list "a" "c"))
(check-expect (skip1 (list 1 2 3 4 5 6)) (list 1 3 5))

;; We are using a CONTEXT-PRESERVING ACCUMULATOR.
(define (skip1 lox0)
  ;; acc: Natural; 1-based position of (car lox) in lox0.
  ;; (skip1 (list "a" "b" "c") 1)
  ;; (skip1 (list     "b" "c") 2)
  ;; (skip1 (list         "c") 3)
  (local [(define (skip1 lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (odd? acc)
                       (cons (car lox) (skip1 (cdr lox) (add1 acc)))
                       (skip1 (cdr lox) (add1 acc)))]))]
    (skip1 lox0 1)))

#;
(define (skip1 lox acc)
  (cond [(empty? lox) empty]
        [else
         (if (odd? <POSITION-OF-CURRENT-ELEMENT>)
             (cons (car lox) (skip1 (cdr lox)))
             (skip1 (cdr lox)))]))

