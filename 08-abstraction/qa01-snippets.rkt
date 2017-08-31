#lang htdp/isl
(require 2htdp/image)

;
;; Suppose you want to design a function that consumes (listof Image)
;; and rotates each image by 90 degrees.
;;
;; Which built-in abstract function(s) will you use to code the body?
;

(define (rotate-all loi)
  (map rotate-90 loi))

(define (rotate-90 i)
  (rotate 90 i))

;
;; Suppose you want to design a function that consumes (listof String) and
;; combines each string in the list to create a single string.
;;
;; Which built-in abstract function(s) will you use to code the body?
;

(define (combine-los los)
  (foldr string-append "" los))

;
;; Suppose you want to design a function that consumes (listof String) produces
;; a list of the short strings. Assume short here means less than 10 characters
;; long.
;;
;; Which built-in abstract function(s) will you use to code the body?
;

(define (short-los los)
  (filter short? los))

(define (short? s)
  (< (string-length s) 10))

(short-los (list "Master Yoda" "Obiwan Kenobi"))
(short-los (list "Master Yoda!" "Leia" "Luke"))

;
;; Suppose you want to design a function that consumes a natural number, n,
;; and produces a list of even numbers up to n.
;;
;; Which built-in abstract function(s) will you use to code the body?
;
(define (evens-to n)
  (filter even? (build-list n identity)))

(evens-to 6)
;; 0 2 4 6

;
;;Suppose you want to design a function that consumes (listof Number) and
;;produces true if the list contains a negative number.
;;
;;Which built-in abstract function(s) will you use to code the body?
;
(define (contains-negative? lon)
  (ormap negative? lon))

(contains-negative? (list 3 9 -5 5))
(contains-negative? (list -1 -3))
(contains-negative? (list 1 3 9))
;; #t
;; #t
;; #f
