#lang htdp/bsl
(require 2htdp/image)

;
; In this problem imagine you have a bunch of pictures that you would like to
; store as data and present in different ways. We'll do a simple version of that
; here, and set the stage for a more elaborate version later.
;
; (A) Design a data definition to represent an arbitrary number of images.
;
; (B) Design a function called arrange-images that consumes an arbitrary number
; of images and lays them out left-to-right in increasing order of size.
;
; NOTE: by size means the total area of each image.
;

;; =========
;; CONSTANTS
(define BLANK (square 0 "solid" "white"))

;; FOR TESTING
(define I1 (rectangle 10 20 "solid" "blue"))
(define I2 (rectangle 20 30 "solid" "red"))
(define I3 (rectangle 30 40 "solid" "green"))

;; ================
;; DATA DEFINITIONS

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; Interp: a list of images.
(define LOI1 '())
(define LOI2 (cons (square 20 "solid" "green") '()))
(define LOI3 (cons (rectangle 10 20 "solid" "blue")
                   (cons (rectangle 20 30 "solid" "red") '())))
(define LOI4 (cons (circle 10 "solid" "magenta")
                   (cons (rectangle 10 15 "outline" "green")
                         (cons (square 15 "solid" "maroon") '()))))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Image ListOfImage)
;;  - self-reference: (rest loi) is ListOfImage

;; =========
;; FUNCTIONS

;; ListOfImage -> Image
;; Lay out images left to right in increasing order of size (size by area).
;; Testing base case isn't necessary for function composition.
;; Because arrange-images is a function composition, the tests only
;; need to ensure that arrange-strings is composing the functions
;; properly, so no base case test is needed.
(check-expect (arrange-images (cons I1 (cons I2 '())))
              (beside I1 I2 BLANK))
(check-expect (arrange-images (cons I2 (cons I1 '())))
              (beside I1 I2 BLANK))

;(define (arrange-images loi) BLANK) ;stub

(define (arrange-images loi)
  (layout-images (sort-images loi)))


;; ListOfImage -> Image
;; Place images beside each other in order of list.
(check-expect (layout-images '()) BLANK)
(check-expect (layout-images (cons I1 (cons I2 '())))
              (beside I1 I2 BLANK))

;(define (layout-images loi) BLANK) ;stub

(define (layout-images loi)
  (cond [(empty? loi) BLANK]
        [else
         (beside (first loi)
                 (layout-images (rest loi)))]))

;; ListOfImage -> ListOfImage
;; Sort images in increasing order of size (area).
(check-expect (sort-images '()) '())
;; Test a list that is already sorted to make sure we do not
;; unsort it accidentaly.
(check-expect (sort-images (cons I1 (cons I2 '())))
              (cons I1 (cons I2 '())))
;; Test on a really unsorted initial list.
(check-expect (sort-images (cons I2 (cons I1 '())))
              (cons I1 (cons I2 '())))

(check-expect (sort-images (cons I3 (cons I1 (cons I2 '()))))
              (cons I1 (cons I2 (cons I3 '()))))

;(define (sort-images loi) loi) ;stub

(define (sort-images loi)
  (cond [(empty? loi) '()]
        [else
         (insert (first loi)
                 ;; Result of the natural recursion will be
                 ;; sorted. Trust it!
                 (sort-images (rest loi)))]))


;; Image ListImage -> ListOfImage
;; Insert img in proper place in list (in increasing order of size).
;; ASSUME: lst is already sorted.

;; Insert at the beginning.
(check-expect (insert I1 '()) (cons I1 '()))

;; Insert at the beginning.
(check-expect (insert I1 (cons I2 (cons I3 '())))
              (cons I1 (cons I2 (cons I3 '()))))

;; Insert at the middle.
(check-expect (insert I2 (cons I1 (cons I3 '())))
              (cons I1 (cons I2 (cons I3 '()))))

;; Insert at the end. Oops I3 twice.
(check-expect (insert I3 (cons I1 (cons I2 '())))
              (cons I1 (cons I2 (cons I3 '()))))

;(define (insert img loi) loi) ;stub

(define (insert img loi)
  (cond [(empty? loi) (cons img '())]
        [else
         (if (larger? img (first loi))
             (cons (first loi)
                   (insert img (rest loi)))
             (cons img loi))]))

;; Image Image -> Boolean
;; Produce #true if img 1 is larger than img2 (by area).
(check-expect (larger? (rectangle 3 4 "solid" "red")
                       (rectangle 2 6 "solid" "red"))
              #false)

(check-expect (larger? (rectangle 5 4 "solid" "red")
                       (rectangle 2 6 "solid" "red"))
              #true)

(check-expect (larger? (rectangle 3 5 "solid" "red")
                       (rectangle 2 6 "solid" "red"))
              #true)

(check-expect (larger? (rectangle 3 4 "solid" "red")
                       (rectangle 5 6 "solid" "red"))
              #false)

(check-expect (larger? (rectangle 3 4 "solid" "red")
                       (rectangle 2 7 "solid" "red"))
              #false)

;(define (larger? img1 img2) #true) ;stub

(define (larger? img1 img2)
  (> (* (image-width img1) (image-height img1))
     (* (image-width img2) (image-height img2))))

;
; 1. Some named constants make code more compact.
;
; 2. Make sure to make examples of all insert cases:
;    - insert in the beginning of empty list;
;    - insert in the beginning of list with other elements;
;    - insert somewhere in the middle of the list;
;
; 3. Coding body involved knowledge domain shift from sorting
;    and inserting images into a list to comparing sizes of
;    images. This causes us to use a helper.
;

