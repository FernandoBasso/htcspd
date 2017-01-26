#lang htdp/bsl
(require 2htdp/image)

;
; INSTRUCTIONS
; ------------
;
; Try (chart LOS1) and (chart LOS2) in the REPL.
;
; In emacs on terminal with geisier and racket mode, C-c C-k runs the
; code on the file. Then, since it produces an image, do:
;
;   M-x racket-view-last-image
;
; and an external window will display the image for you.
;

;
; PROBLEM:
;
; Eva is trying to decide where to go to university. One important factor for her is
; tuition costs. Eva is a visual thinker, and has taken Systematic Program Design,
; so she decides to design a program that will help her visualize the costs at
; different schools. She decides to start simply, knowing she can revise her design
; later.
;
; The information she has so far is the names of some schools as well as their
; international student tuition costs. She would like to be able to represent that
; information in bar charts like this one:
;
;   "./imgs/tuition-graph-example.jpg"
;
; (A) Design data definitions to represent the information Eva has.
; (B) Design a function that consumes information about schools and their
;     tuition and produces a bar chart.
; (C) Design a function that consumes information about schools and produces
;     the school with the lowest international student tuition.
;

;; CONTANTS

(define Y-SCALE 1/200)
(define FONT-SIZE 24)
(define FONT-COLOR "black")
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")

;; DATA DEFINITIONS

(define-struct school (name tuition))
;; School is (make-school String Natural)
;; Interp: name is school name; tuition is international student
;; tuition in USD.

(define S1 (make-school "School #1" 27797))
(define S2 (make-school "School #2" 23300))
(define S3 (make-school "School #3" 28500))

#;
(define (fn-for-school s)
  (... (school-name s)
       (school-tuition s)))

;; Template Rules Used:
;; - compound: (make-school String Natural)

;; ListOfSchool is one of:
;; - empty
;; - (cons School ListOfSchool)
;; Interp: a list of schools.
(define LOS1 '())
(define LOS2 (cons S1 (cons S2 (cons S3 '()))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-school (first los))
              (fn-for-los (rest los)))]))

;; Template Rules Used:
;; - one of: 2 cases
;;   - atomic distinct: empty
;;   - compound: (cons School ListOfSchool)
;; - reference: (first los) is School
;; - self-reference: (rest los) is ListOfSchool


;; FUNCTIONS

;; ListOfSchool -> Image
;; Produce bar chart showing names and tuitions of consumed schools.
(check-expect (chart '()) (square 0 "solid" "white"))
(check-expect (chart (cons (make-school "S1" 8000) '()))
              ;; This is one bar chart (next to (beside) the dummy, base case image).
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))
(check-expect (chart (cons (make-school "S2" 12000)
                           (cons (make-school "S1" 8000) '())))
              (beside/align "bottom"
                            ;; This is one bar chart next to (beside) the other bar.
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S2" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "solid" BAR-COLOR))
                            ;; This another bar chart next to (beside) the dummy, base case image.
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

;(define (chart los) (square 0 "solid" "white")) ;stub

(define (chart los)
  (cond [(empty? los) (square 0 "solid" "white")]
        [else
         (beside/align "bottom"
                       ;; `make--bar` is the natural helper. We whished for it.
                       (make-bar (first los))
                       (chart (rest los)))]))


;; School -> Image
;; Produce the bar for the given (single) school.
(check-expect (make-bar (make-school "S1" 8000))
              (overlay/align "center" "bottom"
                             (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                             (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                             (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR)))

;(define (make-bar s) (square 0 "solid" "white")) ;stub

(define (make-bar s)
  (overlay/align "center" "bottom"
                 (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" "black")
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR)))

