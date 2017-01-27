#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

; PROBLEM:
;
; In this problem you will design another world program. In this program the changing
; information will be more complex - your type definitions will involve arbitrary
; sized data as well as the reference rule and compound data. But by doing your
; design in two phases you will be able to manage this complexity. As a whole, this problem
; will represent an excellent summary of the material covered so far in the course, and world
; programs in particular.
;
; This world is about spinning bears. The world will start with an empty screen. Clicking
; anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
; but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the
; screen, a new upright bear appears and starts spinning.
;
; So each bear has its own x and y position, as well as its angle of rotation. And there are an
; arbitrary amount of bears.
;
; To start, design a world that has only one spinning bear. Initially, the world will start
; with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
; world will replace the old bear with a new bear at the new spot. You can do this part
; with only material up through compound.
;
; Once this is working you should expand the program to include an arbitrary number of bears.
;
; Here is an image of a bear for you to use: "./imgs/bear.jpg".



;; =========
;; CONSTANTS
(define MTS-W 400)
(define MTS-H 300)
(define MTS (empty-scene MTS-W MTS-H))

(define BEAR (bitmap "imgs/bear.png"))
(define BEAR-1 BEAR)
(define BEAR-2 (rotate 90 BEAR))
(define BEAR-3 (rotate 180 BEAR))
(define BEAR-4 (rotate 270 BEAR))

(define ROTATION-SPEED 3)

;; ================
;; DATA DEFINITIONS

(define-struct bear (x y angle))
;; Interp: Bear is (make-bear Natural Natural Natural)
;;         x and y are the the center of the bear
;;         x and y are in screen coordinates (pixels)
;;         angle is the rotation in degrees

;; Examples of four possible bear angle. Tip of char represents bear head:
;;
;;      ^
;;    <   >
;;      v


;; Angle 0 means the bear is upright. ^
(define B1 (make-bear 30 30 0))

;; Angle 90 means bear's head is to the left, legs to the right. <
(define B2 (make-bear 50 70 90))

;; Angle 180 means bear's head is pointing down, legs pointing up. v
(define B3 (make-bear 50 70 180))

;; Angle 270 means bear's head is to the right, legs to the left. >
(define B4 (make-bear 80 65 270))

#;
(define (fn-for-bear b)
  (... (bear-x b)
       (bear-y b)
       (bear-angle b)))

;; Template Rules Used:
;; - compound: 3 fields
;; - atomic non-distinct: natural


;; ListOfBear is one of:
;; - empty
;; - (cons Bear ListOfBear)
(define LOB1 empty)
(define LOB2 (cons (make-bear 50 50 0) empty))
(define LOB3 (cons (make-bear 50 50 0)
                   (cons (make-bear 70 50 180)
                         (cons (make-bear 100 120 270) empty))))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-bear (first b))
              (fn-for-lob (rest lob)))]))

;; Template Rules Used:
;; - one of: two cases:
;; - atomic distinct: empty
;; - compound: (cons Bear ListOfBear)
;; - reference: (first lob) is Bear
;; - self-reference (rest lob) is ListOfBear


;; =========
;; FUNCTIONS

;; Start the program and an empty list of bears.
;; Ex: (main empty)
(define (main lob)
  (big-bang lob                            ; ListOfBear
            (on-tick next-bears 0.15)           ; ListOfBear -> Image
            (to-draw render-bears)         ; ListOfBear MouseEvent -> ListOfBear
            (on-mouse handle-mouse)))

;; ListOfBear -> ListOfBear
;; Produce next list of bear with next angle.
(check-expect (next-bears empty) empty)
(check-expect (next-bears (cons (make-bear 100 100 0) empty))
              (cons (make-bear 100 100 90) empty))
(check-expect (next-bears (cons (make-bear 100 100 0)
                                (cons (make-bear 200 150 270)
                                      (cons (make-bear 222 111 180) empty))))
              (cons (make-bear 100 100 90)
                    (cons (make-bear 200 150 0)
                          (cons (make-bear 222 111 270) empty))))

;(define (next-bears lob) empty) ;stub

(define (next-bears lob)
  (cond [(empty? lob) empty]
        [else
         (cons (make-bear (bear-x (first lob))
                          (bear-y (first lob))
                          (next-angle (first lob)))
               (next-bears (rest lob)))]))


;; ListOfBear -> Image
;; Render scene with zero or more bears in their proper positions and angles.
(check-expect (render-bears empty) MTS)

(check-expect (render-bears (cons (make-bear 50 50 0) empty))
              (place-image BEAR-1 50 50 MTS))

(check-expect (render-bears (cons (make-bear 50 50 0)
                                  (cons (make-bear 90 70 270) empty)))
              (place-images (list BEAR-1
                                  BEAR-4)
                            (list (make-posn 50 50)
                                  (make-posn 90 70))
                            MTS))


(check-expect (render-bears (cons (make-bear 50 50 0)
                                  (cons (make-bear 110 70 180)
                                        (cons (make-bear 200 180 270)
                                              (cons (make-bear 250 220 90) empty)))))
              (place-images (cons BEAR-1
                                  (cons BEAR-3
                                        (cons BEAR-4
                                              (cons BEAR-2 empty))))
                            (cons (make-posn 50 50)
                                  (cons (make-posn 110 70)
                                        (cons (make-posn 200 180)
                                              (cons (make-posn 250 220) empty))))
                            MTS))


;(define (render-bears lob) MTS) ;stub

(define (render-bears lob)
  (cond [(empty? lob) MTS]
        [else
         (place-images (image-list lob)
                       (posn-list lob)
                       MTS)]))


;; ListOfBear MouseEvent -> ListOfBear
;; Adds new Bear with proper x, y and rotation values to ListOfBear
;; upon mouse click on MTS

;; Wrong button. Don't add a new Bear to the list.
(check-expect (handle-mouse empty 50 50 "button-up") empty)

(check-expect (handle-mouse empty 50 50 "button-down")
              (cons (make-bear 50 50 0) empty))

(check-expect (handle-mouse (cons (make-bear 50 50 0)
                                  (cons (make-bear 80 70 270) empty)) 100 100 "button-down")
              (cons (make-bear 100 100 0)
                    (cons (make-bear 50 50 0)
                          (cons (make-bear 80 70 270) empty))))


;(define (handle-mouse lob mx my me) empty) ;stub

(define (handle-mouse lob mx my me)
  (cond [(mouse=? me "button-down") (cons (make-bear mx my 0) lob)]
        [else lob]))


;; Bear -> Image
;; Produce image with the right rotation based on Bear angle property.
;; Invalid angle for our purposes.
(check-expect (choose-image (make-bear 100 100 25)) (square 0 "solid" "white"))
(check-expect (choose-image (make-bear 100 100 0)) BEAR-1)
(check-expect (choose-image (make-bear 100 100 90)) BEAR-2)
(check-expect (choose-image (make-bear 100 100 180)) BEAR-3)
(check-expect (choose-image (make-bear 100 100 270)) BEAR-4)

(define (choose-image b)
  (cond [(= (bear-angle b) 0)   BEAR-1]
        [(= (bear-angle b) 90)  BEAR-2]
        [(= (bear-angle b) 180) BEAR-3]
        [(= (bear-angle b) 270) BEAR-4]
        [else (square 0 "solid" "white")]))

;(define (choose-image b) (square 0 "solid" "white"))    ;stub

;; ListOfBear -> ListOfImage
;; Produce list of images from a list of bears.
(check-expect (image-list empty) empty)
(check-expect (image-list (cons (make-bear 100 100 0)
                                (cons (make-bear 100 100 180)
                                      (cons (make-bear 100 100 270) empty))))
              (cons BEAR-1 (cons BEAR-3 (cons BEAR-4 empty))))

;(define (image-list lob) empty)    ;stub

(define (image-list lob)
  (cond [(empty? lob) empty]
        [else
         (cons (choose-image (first lob))
               (image-list (rest lob)))]))


;; ListOfBear -> ListOfPosn
;; Produce a list of posn from a list of bears.
(check-expect (posn-list empty) empty)
(check-expect (posn-list (cons (make-bear 100 100 0)
                               (cons (make-bear 89 123 180)
                                     (cons (make-bear 25 190 270) empty))))
              (cons (make-posn 100 100)
                    (cons (make-posn 89 123)
                          (cons (make-posn 25 190) empty))))

(define (posn-list lob)
  (cond [(empty? lob) empty]
        [else
         (cons (make-posn (bear-x (first lob)) (bear-y (first lob)))
               (posn-list (rest lob)))]))


;; Bear -> Natural
;; Produce the next angle for bear.
(check-expect (next-angle (make-bear 0 0 0)) 90)
(check-expect (next-angle (make-bear 0 0 90)) 180)
(check-expect (next-angle (make-bear 0 0 180)) 270)
(check-expect (next-angle (make-bear 0 0 270)) 0)

;(define (next-angle b) 0)    ;stub

(define (next-angle b)
  (cond [(= (bear-angle b) 0) 90]
        [(= (bear-angle b) 90) 180]
        [(= (bear-angle b) 180) 270]
        [(= (bear-angle b) 270) 0]))
