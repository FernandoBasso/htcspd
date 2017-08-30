#lang htdp/isl

(define (area r)
  (* pi (sqr r)))

(area 4) ;(* pi (sqr 4)) ;area of circle radius 4
(area 6) ;(* pi (sqr 6)) ;area of circle radius 6


;; ====================

;; ListOfString -> Boolean
;; produce true if los includes "UBC"
(check-expect (contains-ubc? empty) false)
(check-expect (contains-ubc? (cons "McGill" empty)) false)
(check-expect (contains-ubc? (cons "UBC" empty)) true)
(check-expect (contains-ubc? (cons "McGill" (cons "UBC" empty))) true)

(define (contains-ubc? los) (contains? "UBC" los))

;; ListOfString -> Boolean
;; produce true if los includes "McGill"
(check-expect (contains-mcgill? empty) false)
(check-expect (contains-mcgill? (cons "UBC" empty)) false)
(check-expect (contains-mcgill? (cons "McGill" empty)) true)
(check-expect (contains-mcgill? (cons "UBC" (cons "McGill" empty))) true)

(define (contains-mcgill? los) (contains? "McGill" los))

;; String (listof String) -> Boolean
;; Produce #t if los includes s
(check-expect (contains? "UBC" empty) #f)
(check-expect (contains? "UBC" (cons "McGill" empty)) #f)
(check-expect (contains? "UBC" (cons "UBC" empty)) #t)
(check-expect (contains? "UBC" (cons "McGill" (cons "UBC" empty))) #t)
(check-expect (contains? "UBC" (cons "UBC" (cons "McGill" empty))) #t)
(check-expect (contains? "Toronto" (cons "UBC" (cons "McGill" empty))) #f)

(define (contains? s los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) s)
             #t
             (contains? s (rest los)))]))

;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

;(define (squares lon) empty) ;stub

;<template from ListOfNumber>

(define (squares lon) (my-map sqr lon))

;; ListOfNumber -> ListOfNumber
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

;(define (square-roots lon) empty) ;stub

;<template from ListOfNumber>

(define (square-roots lon) (my-map sqrt lon))


;; Given fn and (list n0 n1 ...) produce (list (fn n0) (fn n1) ...)
;; (X -> Y) (listof X) -> (listof Y)
(check-expect (my-map sqr empty) empty)
(check-expect (my-map sqr (list 2 4)) (list 4 16))
(check-expect (my-map sqrt (list 16 9)) (list 4 3))
(check-expect (my-map abs (list -3 2 -1)) (list 3 2 1))
(check-expect (my-map string-length (list "foo" "jedi" "Linux")) (list 3 4 5))

(define (my-map fn lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fn (first lon))
               (my-map fn (rest lon)))]))

;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty) ;stub

;<template from ListOfNumber>

(define (positive-only lon) (my-filter positive? lon))


;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty) ;stub

;<template from ListOfNumber>

(define (negative-only lon) (my-filter negative? lon))

(check-expect (my-filter positive? empty) empty)
(check-expect (my-filter positive? (list 1 -2 3 -4)) (list 1 3))
(check-expect (my-filter negative? (list 1 -2 3 -4)) (list -2 -4))
(check-expect (my-filter even? (list -2 3 5 6 7 8)) (list -2 6 8))
(check-expect (my-filter odd? (list -2 -1 0 1 2)) (list -1 1))

;; Given a list, produce a list of only the elements that satisfy the predicate pred.
;; (X -> Boolean) (listof X) -> (listof X)
(define (my-filter pred lon)
  (cond [(empty? lon) empty]
        [else
         (if (pred (first lon))
             (cons (first lon)
                   (my-filter pred (rest lon)))
             (my-filter pred (rest lon)))]))
