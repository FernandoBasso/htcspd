#lang htdp/asl
(require 2htdp/image)


;;
;; PROBLEM:
;;
;; Using the following data definition, design a function that
;; consumes a room and produces the total number of rooms reachable
;; from the given room. Include the starting room itself. Your
;; function should be tail recursive, but you should not use the
;; primitive length function.
;;


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

;; Room -> Natural
;; Produces number of rooms reachable from a room, including the given room.
(check-expect (count-rooms H1) 2)
(check-expect (count-rooms H2) 2)
(check-expect (count-rooms H3) 3)
(check-expect (count-rooms H4) 6)
;; H2B exits back to H21, so, it should be able to reach 2 rooms.
(check-expect (count-rooms (car (room-exits H2))) 2)
;; H4B exits to E, which exits back to A, so, it should be able
;; to reach 6 rooms.
(check-expect (count-rooms (car (room-exits H4))) 6)

(define (count-rooms r0)
  ;; `todo` is (list-of Room); a worklist accumulator.
  ;; `visited` is (list-of String); context-preseving accumulator containing
  ;;                                names of already visited rooms.
  (local [(define (fn-for-room r todo visited count)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited count)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (add1 count))))

          (define (fn-for-lor todo visited count)
            (cond [(empty? todo) count]
                  [else
                   (fn-for-room (car todo)
                                (cdr todo)
                                visited
                                count)]))]
    ;; trampoline
    (fn-for-room r0 empty empty 0)))

