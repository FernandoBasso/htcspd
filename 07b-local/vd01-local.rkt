#lang htdp/isl

(local [(define a 1)
        (define b 2)]
  (+ a b))
;; 3

(local [(define p "accio ")
        (define (fetch n) (string-append p n))]
  (fetch "portkey"))
;; "accio portkey"
