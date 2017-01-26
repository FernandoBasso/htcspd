#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

;; ==========
;; CONSTANTS:

(define IMG-BEAR (bitmap "imgs/bear.png"))
(define WIDTH 600)  ; Width of the scene.
(define HEIGHT 700) ; Height of the scene.
(define SPEED 3)    ; Speed of rotation.
(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; DATA DEFINITIONS:

(define-struct bear (x y r))
;; Bear is (make-bear Number[0,WIDTH] Number[0,HEIGHT] Number)
;; Interp: (make-bear x y r)
;;   x is the x coord in pixels,
;;   y is the y coord in pixels,
;;   r is the angle of rotation in degrees

;; Upright bear in the upper right corner.
(define B1 (make-bear 15 15 0))
;; Head on left, feet on the right, in the middle of the scene.
(define B2 (make-bear (/ WIDTH 2) (/ HEIGHT 2) 90))

#;
(define (fn-for-bear b)
  (... (bear-x b)
       (bear-y b)
       (bear-r b)))

;; Template Rules Used:
;; - compound: 3 fields

;; ListOfBear is one off:
;;  - '()
;;  - (cons Bear ListOfBear)
(define LOB0 '())
(define LOB1 (cons B1 '()))
(define LOB2 (cons B1 (cons B2 '())))

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-bear (first lob))
              (fn-for-lob (rest lob)))]))

;; Template Rules Used:
;; - one of: 2 types
;;   - atomic distinct: '()
;;   - compound: 2 fields
;;     - reference: (first lob) is Bear
;;     - self-reference: (rest lob) is ListOfBear


;; ==========
;; FUNCTIONS:

;; ListOfBear -> ListOfBear
;; Start the world with (main '())
(define (main lob)
  (big-bang lob                        ; ListOfBear
            (on-tick spin-bears)       ; ListOfBear -> ListOfBear
            (to-draw render-bears)     ; ListOfBear -> Image
            (on-mouse handle-mouse)))  ; ListOfBear -> Integer Integer MouseEvent -> ListOfBear

;; ListOfBear -> ListOfBear
;; Spin all bears forward by SPEED degrees.
(check-expect (spin-bears '()) '())
(check-expect (spin-bears (cons (make-bear 0 0 0) '()))
              (cons (make-bear 0 0 (+ 0 SPEED)) '()))
(check-expect (spin-bears
               (cons (make-bear 0 0 0)
                     (cons (make-bear (/ WIDTH 2) (/ HEIGHT 2) 90)
                           '())))
              (cons (make-bear 0 0 (+ 0 SPEED))
                    (cons (make-bear (/ WIDTH 2) (/ HEIGHT 2) (+ 90 SPEED))
                          '())))

;(define (spin-bears lob) '()) ;stub

;; <took template from ListOfBear>
(define (spin-bears lob)
  (cond [(empty? lob) '()]
        [else
         (cons (spin-bear (first lob))
               (spin-bears (rest lob)))]))

;; Bear -> Bear
;; Spin a bear forward by SPEED degrees.
(check-expect (spin-bear (make-bear 0 0 0)) (make-bear 0 0 (+ 0 SPEED)))
(check-expect (spin-bear (make-bear (/ WIDTH 2) (/ HEIGHT 2) 90))
              (make-bear (/ WIDTH 2) (/ HEIGHT 2) (+ 90 SPEED)))

;(define (spin-bear b) b) ;stub

;; <took template from Bear>
(define (spin-bear b)
  (make-bear (bear-x b)
             (bear-y b)
             (+ (bear-r b) SPEED)))

;; ListOfBear -> Image
;; Render bears onto the empty scene.
(check-expect (render-bears '()) MTS)
(check-expect (render-bears (cons (make-bear 0 0 0) '()))
              (place-image (rotate 0 IMG-BEAR) 0 0 MTS))
(check-expect (render-bears
               (cons (make-bear 0 0 0)
                     (cons (make-bear (/ WIDTH 2) (/ HEIGHT 2) 90)
                           '())))
              (place-image (rotate 0 IMG-BEAR) 0 0
                           (place-image (rotate 90 IMG-BEAR) (/ WIDTH 2) (/ HEIGHT 2)
                                        MTS)))

;(define (render-bears lob) MTS) ;stub

(define (render-bears lob)
  (cond [(empty? lob) MTS]
        [else
         (render-bear-on (first lob)
                         (render-bears (rest lob)))]))

;; Bear Image -> Image
;; Render an image of the bear on the given image.
(check-expect (render-bear-on (make-bear 0 0 0) MTS)
              (place-image (rotate 0 IMG-BEAR) 0 0 MTS))
(check-expect (render-bear-on (make-bear (/ WIDTH 2) (/ HEIGHT 2) 90) MTS)
              (place-image (rotate 90 IMG-BEAR) (/ WIDTH 2) (/ HEIGHT 2) MTS))

;(define (render-bear-on b img) MTS) ;stub

;; <took template from Bear with added atomic parameter>
(define (render-bear-on b img)
  (place-image (rotate (modulo (bear-r b) 360) IMG-BEAR)
               (bear-x b) (bear-y b) img))

;; ListOfBear Integer Integer MouseEvent -> ListOfBear
;; On click, adds a new bear with zero roation to the list
;; at x and y coordinates.
(check-expect (handle-mouse '() 5 4 "button-down")
              (cons (make-bear 5 4 0) '()))
(check-expect (handle-mouse '() 5 4 "move") '())

;(define (handle-mouse lob x y me) '()) ;stub

;; <template according to MouseEvent large enumeration.
(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down")
         (cons (make-bear x y 0) lob)]
        [else lob]))


