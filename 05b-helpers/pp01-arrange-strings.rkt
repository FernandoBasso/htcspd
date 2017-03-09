#lang htdp/bsl
(require 2htdp/image)

;; =========
;; CONSTANTS
(define TEXT-SIZE 24)
(define TEXT-COLOR "mangenta")
(define BLANK (square 0 "solid" "white"))

(define S1 "Apple")
(define S2 "Sally")
(define S3 "Systematic Program Design")

;; ================
;; DATA DEFINITIONS

;; ListOfString is one of:
;;  - empty
;;  - (cons String ListOfString)
;; Interp: a list of strings.
(define LOS1 '())
(define LOS2 (cons "Foo" '()))
(define LOS3 (cons "Jedi" (cons "Linux" (cons "Lisp" '()))))


(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons String ListOfString)
;;  - self-reference: (rest los) is ListOfString

;; =========
;; FUNCTIONS

;; ListOfString -> Image
;; Layout strings vertically in alphabetical order.
;; Because arrange-strings is a function composition, the tests only
;; need to ensure that arrange-strings is composing the functions
;; properly, so no base case test is needed.
(check-expect (arrange-strings (cons "Sally" (cons "Apple" '())))
              (above/align "left"
                           (text "Apple" TEXT-SIZE TEXT-COLOR)
                           (text "Sally" TEXT-SIZE TEXT-COLOR)
                           BLANK))

;(define (arrange-strings los) BLANK) ;stub

(define (arrange-strings los)
  (layout-strings (sort-strings los)))

;; ListOfString -> Image
;; Place string images above each other in order of list.
(check-expect (layout-strings '()) BLANK)
(check-expect (layout-strings (cons "Sally" (cons "Apple" '())))
              (above/align "left"
                           (text "Sally" TEXT-SIZE TEXT-COLOR)
                           (text "Apple" TEXT-SIZE TEXT-COLOR)
                           BLANK))

;(define (layout-strings los) BLANK) ;stub

(define (layout-strings los)
  (cond [(empty? los) BLANK]
        [else
         (above/align "left"
                      (text (first los) TEXT-SIZE TEXT-COLOR)
                      (layout-strings (rest los)))]))

;; ListOfString -> ListOfString
;; Sort strings in alphabetical order.
;; !!!
(check-expect (sort-strings '()) '())
;; Test on an already sorted string to make sure we don't
;; accidentaly unsort it.
(check-expect (sort-strings (cons "Apple" (cons "Sally" '())))
              (cons "Apple" (cons "Sally" '())))
;; Test an unsorted list to make sure we really sort it.
(check-expect (sort-strings (cons "Systematic Program Design" (cons "Apple" '())))
              (cons "Apple" (cons "Systematic Program Design" '())))

;(define (sort-strings los) los) ;stub

(define (sort-strings los)
  (cond [(empty? los) '()]
        [else
         (insert-string (first los)
                        (sort-strings (rest los)))]))

;; String ListOfString -> ListOfString
;; Insert str in the correct place in the sorted list of strings.
;; ASSUME: los is already sorted.
(check-expect (insert-string S1 empty)(cons S1 empty))
(check-expect (insert-string S1 (cons S2 (cons S3 empty)))
              (cons S1 (cons S2 (cons S3 empty))))
(check-expect (insert-string S2 (cons S1 (cons S3 empty)))
              (cons S1 (cons S2 (cons S3 empty))))
(check-expect (insert-string S3 (cons S1 (cons S2 empty)))
              (cons S1 (cons S2 (cons S3 empty))))

;(define (insert-string str los) los) ;stub

(define (insert-string str los)
  (cond [(empty? los) (cons str '())]
        [else
         (if (string>=? str (first los))
             ;A1
             (cons (first los)
                   (insert-string str (rest los)))
             (cons str los))]))

