#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)


;
; PROBLEM:
;
; Design a simple interactive animation of rain falling down a
; screen. Wherever we click, a rain drop should be created and as time
; goes by it should fall. Over time the drops will reach the bottom of
; the screen and "fall off". You should filter these excess drops out
; of the world state - otherwise your program is continuing to tick and
; and draw them long after they are invisible.
;
; In your design pay particular attention to the helper rules. In our
; solution we use these rules to split out helpers:
;   - function composition
;   - reference
;   - knowledge domain shift
;

;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d)
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; Start rain program by evaluating (main '()).
(define (main lod)
  (big-bang lod
            ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-mouse handle-mouse)
            ; ListOfDrop -> ListOfDrop
            (on-tick  next-drops)
            ; ListOfDrop -> Image
            (to-draw  render-drops)))


;; ---------------------------------------------------------------------------------
;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; If mevt is "button-down" add a new drop at that position.

; Click at coord 10,20. Add drop to list of drops.
(check-expect (handle-mouse '() 10 20 "button-down")
              (cons (make-drop 10 20) '()))

; Enter canvas area. Don't add anything to the list of drops.
(check-expect (handle-mouse '() 10 20 "enter") '())

;(define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down")
         (cons (make-drop x y) lod)]
        [else lod]))

;; ---------------------------------------------------------------------------------
;; ListOfDrop -> ListOfDrop
;; Produce filtered and ticked list of drops.
(check-expect (next-drops
               (cons (make-drop 50 70)
                     (cons (make-drop 80 100)
                           '())))
              (cons (make-drop 50 (+ 70 SPEED))
                    (cons (make-drop 80 (+ 100 SPEED))
                          '())))

;; Checks that filter-drops is being called.
(check-expect (next-drops
               (cons (make-drop 150 (+ HEIGHT 2))
                     (cons (make-drop 100 120)
                           '())))
              (cons (make-drop 100 (+ 120 SPEED))
                    '()))

;(define (next-drops lod) lod) ; stub

(define (next-drops lod)
  (filter-drops (tick-drops lod)))

;; ---------------------------------------------------------------------------------
;; ListOfDrop -> ListOfDrop
;; Filter out drops that are already outside MTS.
(check-expect (filter-drops
               (cons (make-drop 150 (+ HEIGHT 1))
                     (cons (make-drop 100 120)
                           '())))
              (cons (make-drop 100 120)
                    '()))

;(define (filter-drops lod) lod) ;stub

(define (filter-drops lod)
  (cond [(empty? lod) '()]
        [else
         (if (off-canvas? (first lod))
             (filter-drops (rest lod))
             (cons (first lod)
                   (filter-drops (rest lod))))]))

;; ---------------------------------------------------------------------------------
;; Drop -> Boolean
;; Produce #true if drop is off the canvas.
(check-expect (off-canvas? (make-drop 50 150)) #f)
(check-expect (off-canvas? (make-drop 100 (+ HEIGHT 1))) #t)

;(define (off-canvas? d) #f) ;stub

(define (off-canvas? d)
  (> (drop-y d) HEIGHT))

;; ---------------------------------------------------------------------------------
;; ListOfDrop -> ListOfDrop
;; Advance drops y position by SPEED.
(check-expect (tick-drops
               (cons (make-drop 50 70)
                     (cons (make-drop 80 100)
                           '())))
              (cons (make-drop 50 (+ 70 SPEED))
                    (cons (make-drop 80 (+ 100 SPEED))
                          '())))

;(define (tick-drops lod) lod) ;stub

(define (tick-drops lod)
  (cond [(empty? lod) '()]
        [else
         (cons (tick-drop (first lod))
               (tick-drops (rest lod)))]))

;; ---------------------------------------------------------------------------------
;; Drop -> Drop
;; Tick drop to next y position by SPEED pixels.
(check-expect (tick-drop (make-drop 50 90))
              (make-drop 50 (+ 90 SPEED)))

;(define (tick-drop d) d) ;stub

(define (tick-drop d)
  (make-drop (drop-x d) (+ (drop-y d) SPEED)))

;; ---------------------------------------------------------------------------------
;; ListOfDrop -> Image
;; Render the drops onto MTS.
(check-expect (render-drops '()) MTS)

(check-expect (render-drops
               (cons (make-drop 50 80)
                     (cons (make-drop 120 100)
                           '())))
              (place-image DROP 50 80
                           (place-image DROP 120 100
                                        MTS)))

;(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-image DROP
                      (drop-x (first lod))
                      (drop-y (first lod))
                      (render-drops (rest lod)))]))

