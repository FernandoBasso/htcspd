#lang htdp/isl

(define p "incendio ")

(local [(define p "accio ")
        (define (fetch n) (string-append p n))]
  (fetch "portkey"))
;; "accio portkey", we use the 'local p' if there is one.

(define x 1)
(define y 2)
(+ x ;; 1
   (local [(define y 3)]
     (+ x y)) ;; 1 + 3
   y) ;; 2

;; 7
