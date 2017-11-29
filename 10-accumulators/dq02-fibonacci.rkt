#lang htdp/isl

;
;  PROBLEM 2:
;
;  The Fibonacci Sequence https://en.wikipedia.org/wiki/Fibonacci_number is
;  the sequence 0, 1, 1, 2, 3, 5, 8, 13,... where the nth element is equal to
;  n-2 + n-1.
;
;  Design a function that given a list of numbers at least two elements long,
;  determines if the list obeys the fibonacci rule, n-2 + n-1 = n, for every
;  element in the list. The sequence does not have to start at zero, so for
;  example, the sequence 4, 5, 9, 14, 23 would follow the rule.
;


;; (listof Number) -> Bool
;; Produce #t if lon is a valid Fibonacci sequence; #f otherwise.
;; ASSUME: lon is at least two elements long.
(check-expect (fibonacci? (list 0 5)) #t)
(check-expect (fibonacci? (list 4 4 8)) #t)
(check-expect (fibonacci? (list 4 4 8 12)) #t)
(check-expect (fibonacci? (list 4 4 8 12 20)) #t)
(check-expect (fibonacci? (list 0 1 1 2 3 5 8 13)) #t)
(check-expect (fibonacci? (list 4 5 9 14 23)) #t)

;(define (valid? lon) #f) ; stub.

(define (fibonacci? lon0)
  ;; n-2 is Number; n - 2 element of lon.
  ;; n-1 is Number; n - 1 element of lon.
  ;;
  ;; (fibonacci? (list 4 5 9 14 23)  0  0) ; outer call.
  ;; (fibonacci? (list     9 14 23)  4  5)
  ;; (fibonacci? (list       14 23)  5  9)
  ;; (fibonacci? (list          23)  9 14)
  ;; (fibonacci? (list            ) 14 23) --> #t
  (local [(define (fibonacci? lon n-2 n-1)
            (cond [(empty? lon) #t]
                  [else
                   (if (not (= (car lon) (+ n-2 n-1)))
                       #f
                       (fibonacci? (cdr lon) n-1 (car lon)))]))]
    (fibonacci? (cdr (cdr lon0))  (car lon0) (car (cdr lon0)))))

