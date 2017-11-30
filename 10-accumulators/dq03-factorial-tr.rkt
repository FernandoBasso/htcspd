#lang htdp/isl

;  PROBLEM 3:
;
;  Refactor the function below to make it tail recursive.
;

;; Natural -> Natural
;; produces the factorial of the given number
;(check-expect (fact 0) 1)
;(check-expect (fact 3) 6)
;(check-expect (fact 5) 120)
#;
(define (fact n)
  (cond [(zero? n) 1]
        [else
         (* n (fact (sub1 n)))]))

(define (fact n0)
  ;; acc is Natural; the computation of the factorial so far.
  ;; acc is a "result so far" type of accumulator.
  ;; (fact 5 1) ; outer call
  ;; (fact 5 1) ; 5
  ;; (fact 4   5) ; 20
  ;; (fact 3 20) ; 60
  ;; (fact 2 60) ; 120
  ;; (fact 1 120) ; 120
  ;; (fact 0 120) ; 120
  (local [(define (fact n acc)
            ;; This takes one more recursion step.
            ;(cond [(zero? n) acc]
            (cond [(= n 1) acc]
                  [else
                   (fact (sub1 n) (* n acc))]))]
    (fact n0 1)))

