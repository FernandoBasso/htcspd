#lang htdp/isl

;  PROBLEM 4:
;
;  Recall the data definition for Region from the Abstraction Quiz. Use a worklist
;  accumulator to design a tail recursive function that counts the number of regions
;  within and including a given region.
;  So (count-regions CANADA) should produce 7


(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

;; Template for count-region
#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))

          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))


;; Region -> Natural
;; Produce the number or regions within a region, including the given region.
(check-expect (count-region VANCOUVER) 1)
(check-expect (count-region ALBERTA) 3)
(check-expect (count-region CANADA) 7)

;(define (count-region r) 0) ; stub.

(define (count-region r)
  ;; acc is Natural; number of regions counted so far.
  ;; todo is (listof Region); List of Regions yet to be visited and counted.
  (local [(define (fn-for-region r acc todo)
            (fn-for-lor (append (region-subregions r) todo)
                        (add1 acc)))

          (define (fn-for-lor todo acc)
            (cond [(empty? todo) acc]
                  [else
                   (fn-for-region (car todo)
                                  acc
                                  (cdr todo))]))]
    (fn-for-region r 0 empty)))

