#lang htdp/bsl

;; ================
;; DATA DEFINITIONS


;
; PROBLEM A:
;
; Design a data definition to represent a movie, including
; title, budget, and year released.
;
; To help you to create some examples, find some interesting movie facts below:
; "Titanic" - budget: 200000000 released: 1997
; "Avatar" - budget: 237000000 released: 2009
; "The Avengers" - budget: 220000000 released: 2012
;

(define-struct movie (title budget year))
;; Movie is (make-movie String Natural Natural).
;; Interp: (make-movie title budget year) is a movie with:
;;         - title
;;         - buget in USD and
;;         - year of release.
(define M1 (make-movie "Titanic" 200 1997))
(define M2 (make-movie "Avatar" 237 2009))
(define M3 (make-movie "The Avengers" 220 2012))

#;
(define (fn-for-movie m)
  (... (movie-title m)   ; String
       (movie-budget m)  ; Natural
       (movie-year m)))  ; Natural

;; Template rules used:
;;  - compound: 3 fields


;; =========
;; FUNCTIONS

;
; PROBLEM B:
;
; You have a list of movies you want to watch, but you like to watch your
; rentals in chronological order. Design a function that consumes two movies
; and produces the title of the most recently released movie.
;
; Note that the rule for templating a function that consumes two compound data
; parameters is for the template to include all the selectors for both
; parameters.
;

;; Movie Movie -> String
;; Produce the most recently released movie.
(check-expect (most-recent M1 M2) (movie-title M2))
(check-expect (most-recent M3 M1) (movie-title M3))

;(define (most-recent m1 m2) m1);

#;
(define (fn-for-most-recent m1 m2)
  (... (movie-title m1)
       (movie-budget m1)
       (movie-year m1)
       (movie-title m2)
       (movie-budget m2)
       (movie-year m2)))

(define (most-recent m1 m2)
  (if (> (movie-year m1) (movie-year m2))
      (movie-title m1)
      (movie-title m2)))

