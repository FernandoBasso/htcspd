#lang htdp/isl

;
; PROBLEM:
;
; Design a function called average that consumes (listof Number) and produces the
; average of the numbers in the list.
;

;; ListOfNumber -> Number
;; Produce average of the numbers in lon.
(check-expect (my-avg empty) 0)
(check-expect (my-avg (list 3)) 3)
(check-expect (my-avg (list 1 2 3)) 2)
(check-expect (my-avg (list 5 10)) 7.5)
(check-expect (my-avg (list -3 2)) -0.5)
(check-expect (my-avg (list 3 -2)) 0.5)

(define (my-avg lon0)
  ;; Let's be careful if list of number is empty, then
  ;; we just return zero here to prevent division by zero
  ;; error in the inner my-avg function.
  (if (empty? lon0)
      0

      ;; sum is Number; the sum of numbers so far.
      ;; len is Natural; the number of numbers added so far.
      ;; (my-avg (list 1 2 3)) ; outer call
      ;; (my-avg (list 1 2 3) 0 1)     ;
      ;; (my-avg (list   2 3) 1 2)     ;
      ;; (my-avg (list     3) 2 3)     ;
      ;; (my-avg (list      ) 6  ;
      (local [(define (my-avg lon sum len)
                (cond [(empty? lon) (/ sum len)]
                      [else
                       (my-avg (cdr lon)
                               (+ (car lon) sum)
                               (add1 len))]))]
        (my-avg lon0 0 0))))


;
; THEIR VERSION
;

;; (listof Number) -> Number
;; Produce the average of a list of numbers
;; ASSUME: lon contains at least 1 element
(check-expect (average (list 2 3 4)) 3)

; <template from (listof Number) + 2 accumulators>
(define (average lon)
  ;; cnt: Number; how many numbers so far
  ;; sum: Number; sum of numbers so far
  ;;
  ;; (average (list 2 3 4)  0 0)
  ;; (average (list   3 4)  1 2)
  ;; (average (list     4)  2 5)
  ;; (average (list      )  3 9)
  (local[(define (average lon cnt sum)
           (cond [(empty? lon) (/ sum cnt)]
                 [else
                  (average (rest lon) (add1 cnt)
                           (+ (first lon) sum))]))]
    (average lon 0 0)))

