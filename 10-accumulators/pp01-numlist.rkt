#lang htdp/isl+

;; (listof String) -> (listof String)
;; Append each string's position in the list to the
;; front of the string to number the list
(check-expect (number-list empty) empty)
(check-expect (number-list (list "first" "second" "third"))
              (list "1: first" "2: second" "3: third"))

;(define (number-list los) los)   ;stub

(define (number-list lon0)
  ;; acc: Natural; 1-based position of (car lon) in lon.
  ;; (number-list (list "first" "second" "third") 1)
  ;; (number-list (list         "second" "third") 2)
  ;; (number-list (list                  "third") 3)
  (local [(define (number-list los acc)
            (cond [(empty? los) empty]
                  [else
                   (cons
                    (string-append (number->string acc) ": " (car los))
                    (number-list (cdr los) (add1 acc)))]))]

    (number-list lon0 1)))

