#lang htdp/isl

;
; PROBLEM:
;
; Hand step the evaluation of (foo 3) given the definition of foo below.
; We know that you can use the stepper to check your work - please go
; ahead and do that AFTER you try hand stepping it yourself.
;
; (define (foo n)
;   (local [(define x (* 3 n))]
;     (if (even? x)
;         n
;         (+ n (foo (sub1 n))))))


(define (foo n)
  (local [(define x (* 3 n))]
    (if (even? x)
        n
        (+ n (foo (sub1 n))))))

(foo 3)

(local [(define x (* 3 3))]
  (if (even? x)
      3
      (+ 3 (foo (sub1 3)))))

;; Three steps in one for the local.
;; (define x_0 (* 3 3)) ;; so DrRacket does not throw a fit...
(define x_0 9)
(if (even? x_0)
    3
    (+ 3 (foo (sub1 3))))

(if (even? 9)
    3
    (+ 3 (foo (sub1 3))))

(+ 3 (foo (sub1 3)))

(+ 3 (foo 2))

(+ 3 (local [(define x (* 3 2))]
       (if (even? 2)
           2
           (+ 2 (foo (sub1 2))))))
;; (define x_1 (* 3 2)) ;; so that DrRacket doesn't throw a fit.
(define x_1 6)
(+ 3
   (if (even? 2)
       2
       (+ 2 (foo (sub1 2)))))

(+ 3
   (if #t
   2
   (+ 2 (foo (sub1 2)))))

(+ 3 2)

