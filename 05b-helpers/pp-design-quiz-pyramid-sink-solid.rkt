(require 2htdp/image)

;; ======================================================================
;; Constants
(define COOKIES (bitmap "./imgs/022-cookies.jpg"))

;; ======================================================================
;; Data Definitions

;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; Interp: a natural number.
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n   ; n is added because it's often useful
              (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 0
;;  - compound: 2 fields
;;  - self-reference: (sub1 n) is Natural


;
; PROBLEM 1:
;
; Complete the design of a function called pyramid that takes a natural
; number n and an image, and constructs an n-tall, n-wide pyramid of
; copies of that image. Ex:
;
;                                   x
;                                  x x
;                                 x x x
;

;; Natural Image -> Image
;; produce an n-wide pyramid of the given image
(check-expect (pyramid 0 COOKIES) empty-image)
(check-expect (pyramid 1 COOKIES) COOKIES)
(check-expect (pyramid 2 COOKIES) (above COOKIES
                                         (beside COOKIES COOKIES empty-image)))
(check-expect (pyramid 3 COOKIES)
              (above COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES empty-image)))

;(define (pyramid n i) empty-image) ; stub

(define (pyramid n i)
  (cond [(zero? n) empty-image]
        [else
         (above (pyramid (sub1 n) i)
                (row-images n i))]))

;; Natural Image -> Image
;; Produce row of n images side by side.
(check-expect (row-images 0 COOKIES) empty-image)
(check-expect (row-images 2 COOKIES) (beside COOKIES COOKIES))

;(define (row-images n i) empty-image) ;stub

(define (row-images n i)
  (cond [(zero? n) empty-image]
        [else
         (beside i
                 (row-images (sub1 n) i))]))

;
; PROBLEM 2:
;
; Consider a test tube filled with solid blobs and bubbles. Over time the
; solids sink to the bottom of the test tube, and as a consequence the bubbles
; percolate to the top. Let's capture this idea in BSL.
;
; Complete the design of a function that takes a list of blobs and sinks each
; solid blob by one. It's okay to assume that a solid blob sinks past any
; neighbor just below it.
;
; To assist you, we supply the relevant data definitions.
;

;; Blob is one of:
;; - "solid"
;; - "bubble"
;; Interp: a gelatinous blob, either a solid or a bubble.
;; Examples are redundant for enumerations.
#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; Template rules used:
;; - one-of: 2 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"


;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; Interp: a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty) ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob

;; ListOfBlob -> ListOfBlob
;; Produce a list of blobs that sinks the given solid blobs by one.

(check-expect (sink empty) empty)

(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))

(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))

(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))

(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))

(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))

(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble"
                                      (cons "bubble" empty)))))
              (cons "bubble"
                    (cons "solid"
                          (cons "solid"
                                (cons "bubble" empty)))))

;(define (sink lob) empty) ; stub

(define (sink lob)
  (cond [(empty? lob) '()]
        [else
         (sink-by-one (first lob) (sink (rest lob)))]))

;; Blob ListOfBlob -> ListOfBlob
;; Sink by one position if "solid".
(check-expect (sink-by-one "solid" (cons "bubble" '()))
              (cons "bubble" (cons "solid" '())))

(check-expect (sink-by-one "solid" (cons "bubble" (cons "bubble" '())))
              (cons "bubble" (cons "solid" (cons "bubble" '()))))

(check-expect (sink-by-one "solid" (cons "solid" (cons "bubble" '())))
              (cons "solid" (cons "solid" (cons "bubble" '()))))

;(define (sink-by-one b lob) '()) ;stub

(define (sink-by-one b lob)
  (cond [(empty? lob) (cons b'())]
        [else
         (if (solid? b)
             (cons (first lob) (cons b (rest lob)))
             (cons b lob))]))

;; Blob -> Boolean
;; Produce #t if b is "solid".
(check-expect (solid? "bubble") #f)
(check-expect (solid? "solid") #t)

;(define (solid? b) #f) ;stub

(define (solid? b)
  (string=? b "solid"))

