#lang htdp/asl
(require 2htdp/image)


(define IMG-GRAPH-EXAMPLES (bitmap "../imgs/029-graphs-examples.jpg"))

;; Room is (make-room String (listof Room))
;; Interp: the room's name, and list of rooms that exits lead to.
(define-struct room (name exits))

(define H1 (make-room "A" (list (make-room "B" empty))))

(define H2
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-))

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))

(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" empty)))
    -A-))

;;
;; Template:
;; ---------
;; - structural recursion
;; - encapsulate with `local`
;; - tail-recursive (because graphs can be quite large and we care
;;   about performance)
;; - a work list acc (tail-recursivenes requires some sort of accumulators)
;; - a context-preserving acc (to keep track of what rooms we have already visited)
;;

#;
(define (fn-for-house r0)
  ;; todo is (list-of Room); a worklist accumulator.
  ;; visited is (list-of String); context-preseving accumulator containing
  ;;                              names of already visited rooms.
  (local [
          ;; Room -> ???
          (define (fn-for-room r todo visited)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited))))

          ;; (list-of Room) -> ???
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (car todo)
                                (cdr todo)
                                visited)]))]
    ;; trampoline
    (fn-for-room r0 empty empty)))

;; Room -> Room
;; Produce room with most exits. In case of a draw, produce the last one visited.
(check-expect (max-exits H1) H1)
(check-expect (max-exits H2) (car (room-exits H2)))
(check-expect (max-exits
               (shared ((-A- (make-room "A" (list -B-)))
                        (-B- (make-room "B" (list -C- -A-)))
                        (-C- (make-room "C" (list -A-))))
                 -A-))
              (shared ((-A- (make-room "A" (list -B-)))
                       (-B- (make-room "B" (list -C- -A-)))
                       (-C- (make-room "C" (list -A-))))
                -B-))

(define (max-exits r0)
  ;; `todo` is (list-of Room); a worklist accumulator.
  ;; `visited` is (list-of String); context-preseving accumulator containing
  ;;                                names of already visited rooms.
  ;; `rmax` is Room; room with max exits so far.
  (local [(define (fn-for-room r todo visited rmax)
            (cond [(member (room-name r) visited)
                   (fn-for-lor todo visited rmax)]
                  [(>= (length (room-exits r)) (length (room-exits rmax)))
                   (fn-for-lor (append (room-exits r) todo)
                               (cons (room-name r) visited)
                               r)]
                  [else
                   (fn-for-lor (append (room-exits r) todo)
                               (cons (room-name r) visited)
                               rmax)]))

          (define (fn-for-lor todo visited rmax)
            (cond [(empty? todo) rmax]
                  [else
                   (fn-for-room (car todo)
                                (cdr todo)
                                visited
                                rmax)]))]
    ;; trampoline
    (fn-for-room r0 empty empty r0)))


;;
;; THEIR SOLUTION
;;
;; It is a good solution because they use a helpr to check whether
;; the current room has more exits that the room with more exits
;; so far. My own solution requires more conditionals because I failed
;; to see that a helper would make code esier and more readable.
;; I didn't stop to consider what could be improved before I decided
;; I was done with it.
;;

;; Room -> Room
;; produce the room with the most exits

(check-expect (max-exits-from H1) H1)
(check-expect (max-exits-from H2) H2)
(check-expect (max-exits-from (shared ((-A- (make-room "A" (list -B-)))
                                       (-B- (make-room "B" (list -C- -A-)))
                                       (-C- (make-room "C" (list -A-))))
                                -A-))
              (shared ((-A- (make-room "A" (list -B-)))
                       (-B- (make-room "B" (list -C- -A-)))
                       (-C- (make-room "C" (list -A-))))
                -B-))

(define (max-exits-from r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; rsf is Room; the room with the most exits of rooms seen so far
  (local [(define (fn-for-room r todo visited rsf)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (max-exits r rsf))))
          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited rsf)]))
          ; helper
          (define (max-exits r rsf)
            (if (>= (length (room-exits rsf)) (length (room-exits r)))
                rsf
                r))]
    (fn-for-room r0 empty empty r0)))
