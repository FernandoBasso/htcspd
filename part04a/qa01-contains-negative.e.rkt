#lang htdp/bsl

;; ListOfNumber is one of:
;; - '()
;; - (cons Number ListOfNumber)
;; Interp. a list of numbers.
(define LON1 '())
(define LON2 (cons 1 '()))
(define LON3 (cons 2 (cons 1 '())))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template Rules Used:
;; - one of: 2 cases
;;   - atomic distinct: empty
;;   - compound: (cons Number ListOfNumber)
;; - <the one we are yet to study>

