#lang htdp/isl

;
; Assuming the use of at least one accumulator, design a function
; that consumes a list of strings, and produces the length of the
; longest string in the list.
;


;; (listof String) -> Natural
;; Produce the length of the longest list in los.
(check-expect (longest-length empty) 0)
(check-expect (longest-length (list "foo")) 3)
(check-expect (longest-length (list "foo" "xo" "y")) 3)
(check-expect (longest-length (list "y" "foo" "xo")) 3)
(check-expect (longest-length (list "xo" "y" "foo")) 3)

;(define (longest-length lon) 0); ; stub

(define (longest-length los0)
  ;; longest: Natural; length of longest string seen so far.
  (local [(define (longest-length los max-len-so-far)
            (cond [(empty? los) max-len-so-far]
                  [else
                   (local [(define this-len (string-length (car los)))]
                     (if (> this-len max-len-so-far)
                         (longest-length (cdr los) this-len)
                         (longest-length (cdr los) max-len-so-far)))]))]
    (longest-length los0 0)))

;
; `longest-length` is tail-recursive because we return from either the `if` or
; the `else` clauses directly by recursively calling `longest-length`.  The
; result of the recursive invokations of `longest-length` is not operated by
; anything else, thus, it is tail-recursive.
;

