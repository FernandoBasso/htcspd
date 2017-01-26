#lang htdp/bsl
(require 2htdp/image)

;
; Consider the following alternative type comment for Eva's school tuition
; information program. Note that this is just a single type, with no reference,
; but it captures all the same information as the two types solution in the
; videos.
;
; (define-struct school (name tuition next))
; ;; School is one of:
; ;;  - false
; ;;  - (make-school String Natural School)
; ;; interp. an arbitrary number of schools, where for each school we have its
; ;;         name and its tuition in USD
;
; (A) Confirm for yourself that this is a well-formed self-referential data
;     definition.
;
;     Yes, it is well-formed because it has a reference and a base case.
;
; (B) Complete the data definition making sure to define all the same examples as
;     for ListOfSchool in the videos.
;
; (C) Design the chart function that consumes School. Save yourself time by
;     simply copying the tests over from the original version of chart.
;
; (D) Compare the two versions of chart. Which do you prefer? Why?
;
;  Their answer:
;
; We prefer the first solution, because chart is broken into two functions -
; one does the bar for each school, the other recurses through the list calling
; the first function and composing those results into a single chart.
;
; For that reason we prefer the first data definition, because it will force
; all functions operating  on ListOfSchool to have this 2 part structure.
;
;  My answer:
;
; I was able to split the chart function into two functions. And, to put
; it short, I think both ways are okay.
;


(define-struct school (name tuition next))
;; School is one of:
;;  - #false
;;  - (make-school String Natural School)
;; Interp: an arbitrary number of schools, where for each we have
;;         its name and its tuition in USD.
(define S1 #false)
(define S2 (make-school "School 1" 27900
                        (make-school "School 2" 23300
                                     (make-school "School 3" 28500 #false))))

#;
(define (fn-for-school s)
  (cond [(false? s) (...)]
        [else
         (... (school-name s)
              (school-tuition s)
              (fn-for-school (school-next s)))]))

;; Template Rules Used:
;; - one of: 2 cases
;;   - atomic distinct: #false
;;   - compound: (make-school String Natural School)
;; - atomic non-distinct: (school-name s) is String
;; - atomic non-distinct: (school-tuition s) is Natural
;; - self-reference: (school-next s) is School

;; Constants
(define FONT-SIZE 24)
(define FONT-COLOR "black")

(define Y-SCALE 1/200)
(define BAR-WIDTH 30)
(define BAR-COLOR "lightblue")

;; Schools -> Image
;; Produce bar chart showing names and tuitions of consumed schools.
(check-expect (chart #false) (square 0 "solid" "white"))

(check-expect (chart (make-school "S1" 8000 false))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

(check-expect (chart (make-school "S2" 12000 (make-school "S1" 8000 false)))
              (beside/align "bottom"
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S2" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 12000 Y-SCALE) "solid" BAR-COLOR))
                            (overlay/align "center" "bottom"
                                           (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                           (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                            (square 0 "solid" "white")))

;(define (chart s) (square 0 "solid" "white")) ;stub

(define (chart s)
  (cond [(false? s) (square 0 "solid" "white")]
        [else
         (beside/align "bottom"
                       (bar s)
                       (chart (school-next s)))]))

;; School -> Image
;; Produce bar for given school
(check-expect (bar (make-school "S1" 27000 #false))
              (overlay/align
               "center"
               "bottom"
               (rotate 90 (text "S1" FONT-SIZE FONT-COLOR))
               (rectangle BAR-WIDTH (* 27000 Y-SCALE) "outline" "black")
               (rectangle BAR-WIDTH (* 27000 Y-SCALE) "solid" BAR-COLOR)))

;(define (bar s) (square 0 "solid" "white")) ;stub

(define (bar s)
  (overlay/align "center" "bottom"
                 (rotate 90 (text (school-name s) FONT-SIZE FONT-COLOR))
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" "black")
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR)))

