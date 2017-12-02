#lang htdp/asl
(require 2htdp/image)

(define IMG-GRAPH-EXAMPLES (bitmap "../imgs/029-graphs-examples.jpg"))

;; Room is (make-room String (listof Room))
;; Interp: the room's name, and list of rooms that exits lead to.
(define-struct room (name exits))

(define IMG-GRAPH-H1 (bitmap "../imgs/030-graph-H1.jpg"))
(define H1 (make-room "A" (list (make-room "B" empty))))

(define IMG-GRAPH-H2 (bitmap "../imgs/031-graph-H2.jpg"))
(define H2
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-))

(define IMG-GRAPH-H3 (bitmap "../imgs/032-graph-H3.jpg"))
(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))

(define IMG-GRAPH-H4 (bitmap "../imgs/033-graph-H4.jpg"))
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

;;
;; PROBLEM:
;;
;; Write a function that produces the number of rooms reachable from a
;; given room, including that room itself.
;;

;; Room -> Natural
;; Produce the total number of rooms reachable from a given room,
;; including the room itself.

(check-expect (num-rooms H1) 2)
(check-expect (num-rooms H2) 2)
(check-expect (num-rooms H3) 3)
(check-expect (num-rooms H4) 6)

;;
;; VERSION WITH AN EXTRA ACCUMULATOR
;;
#;
(define (num-rooms r0)
  ;; `todo` is (listof Room); a worklist accumulator
  ;; `visited` is (listof String); context preserving acc, names of rooms already visited
  ;; `rsf` is Natural; number of rooms visited so far
  (local [(define (fn-for-room r todo visited rsf)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (add1 rsf))))
          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited
                                rsf)]))]
    (fn-for-room r0 empty empty 0)))


;;
;; VERSION -WITHOUT- AN EXTRA ACCUMULATOR
;; Just produce `(length visited)` at the end.
;;
(define (num-rooms r0)
  ;; `todo` is (listof Room); a worklist accumulator
  ;; `visited` is (listof String); context preserving acc, names of rooms already visited
  ;; `rsf` is Natural; number of rooms visited so far
  (local [(define (fn-for-room r todo visited)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited))))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (length visited)]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty)))

