#lang htdp/isl

;
; PROBLEM:
;
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by dropping every nth element from lox.
;
; (dropn (list 1 2 3 4 5 6 7) 2) should produce (list 1 2 4 5 7)
;


;; ListOfX is one of:
;; - empty
;; - (cons x ListOfX)
;; Interp: a list of elements of type X.

(define LOX0 empty)
(define LOX1 (list 10))
(define LOX2 (list "Yoda" "Luke"))

;; Template for ListOfX.
#;
(define (fn-for-lox lox)
  (cond [(empty? lox) (...)]
        [else
         (... (car lox)
              (fn-for-lox (cdr lox)))]))

;
; Template rules used:
; - one of: 2 cases
; - atomic distinct: empty
; - compound: (cons x ListOfX)
; - self-reference: (cdr lox) is ListOfX
;


;; (listof X) (listof X)
;; Produce list with every nth element removed from the original list.
(check-expect (my-dropn empty 2) empty)
(check-expect (my-dropn (list 100 200 300) 0) empty)
(check-expect (my-dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))
(check-expect (my-dropn (list "a" "b" "c" "d" "e" "f") 1) (list "a" "c" "e"))
(check-expect (my-dropn (list "Yoda" "Luke" "Leia" "Darth" "Obiwan" "C3PO" "R2D2") 3)
              (list "Yoda" "Luke" "Leia" "Obiwan" "C3PO" "R2D2"))


; Template from ListOfX with with param and accumulator added.

(define (my-dropn lox0 n)
  ;; acc is: Natural; Number of elements already seen since its initial/reset value.
  ;; (my-dropn (list 1 2 3 4 5 6 7) 2 0) ; outer call.
  ;;
  ;; (my-dropn (list 1 2 3 4 5 6 7) 2 0) ; keep
  ;; (my-dropn (list   2 3 4 5 6 7) 2 1) ; keep
  ;; (my-dropn (list     3 4 5 6 7) 2 2) ; drop
  ;; (my-dropn (list       4 5 6 7) 2 0) ; keep
  ;; (my-dropn (list         5 6 7) 2 1) ; keep
  ;; (my-dropn (list           6 7) 2 2) ; drop
  ;; (my-dropn (list             7) 2 0) ; keep
  ;; (my-dropn (list              ) 2 1) ; keep
  (local [(define (fn-for-lox lox n acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (= n acc)
                       (fn-for-lox (cdr lox) n 0)
                       (cons (car lox)
                             (fn-for-lox (cdr lox) n (add1 acc))))]))]
    (fn-for-lox lox0 n 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Their version follows. More intelligente use of parameters, even
; needing less parameters, which makes things supposedly easier to understand.
;

;; (listof X) Natural -> (listof X)
;; produce list formed by dropping every nth element from lox
(check-expect (dropn empty 0) empty)
(check-expect (dropn (list "a" "b" "c" "d" "e" "f") 0) empty)
(check-expect (dropn (list "a" "b" "c" "d" "e" "f") 1) (list "a" "c" "e"))
(check-expect (dropn (list "a" "b" "c" "d" "e" "f") 2) (list "a" "b" "d" "e"))

;; templated according to (listof X) and accumulator

(define (dropn lox0 n)
  ;; acc: Natural; the number of elements to keep before dropping the next one
  ;; (dropn (list "a" "b" "c" "d") 2)  ;outer call
  ;;
  ;; (dropn (list "a" "b" "c" "d") 2)  ; keep
  ;; (dropn (list     "b" "c" "d") 1)  ; keep
  ;; (dropn (list         "c" "d") 0)  ; drop
  ;; (dropn (list             "d") 2)  ; keep
  ;; (dropn (list                ) 1)  ; keep
  (local [(define (dropn lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (zero? acc)
                       (dropn (rest lox) n)
                       (cons (first lox)
                             (dropn (rest lox)
                                    (sub1 acc))))]))]
    (dropn lox0 n)))

