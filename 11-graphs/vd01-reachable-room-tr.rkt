#lang htdp/asl
(require 2htdp/image)

;
; PROBLEM:
;
; Imagine you are suddenly transported into a mysterious house, in which all you can
; see is the name of the room you are in, and any doors that lead OUT of the room.
; One of the things that makes the house so mysterious is that the doors only go in
; one direction. You can't see the doors that lead into the room.
;
; Here are some examples of such a house:
;

(define IMG-GRAPH-EXAMPLES (bitmap "../imgs/029-graphs-examples.jpg"))

;
;
; In computer science, we refer to such an information structure as a directed graph.
; Like trees, in directed graphs the arrows have direction. But in a graph it is
; possible to go in circles, as in the second example above. It is also possible for
; two arrows to lead into a single node, as in the fourth example.
;
;
; Design a data definition to represent such houses. Also provide example data for
; the four houses above.
;


;; `exits` could be `doors`, but `exits` make it more clear
;; that you can just “go out” through them.

;; Room is (make-room String (listof Room))
;; Interp: the room's name, and list of rooms that exits lead to.
(define-struct room (name exits))

(define IMG-GRAPH-H1 (bitmap "../imgs/030-graph-H1.jpg"))
(define H1 (make-room "A" (list (make-room "B" empty))))

(define IMG-GRAPH-H2 (bitmap "../imgs/031-graph-H2.jpg"))
(define H2
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-))

;; (length (room-exits H2))
;; (map room-name (room-exits H2))

(define IMG-GRAPH-H3 (bitmap "../imgs/032-graph-H3.jpg"))


; correct, but confusing.
#;
(define H3
  (shared ((-0- (make-room
                 "A"
                 (list (make-room
                        "B"
                        (list (make-room
                               "C"
                               (list -0-))))))))
    -0-))

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))

;; (room-name H3)
;; (length (room-exits H3))
;; (map room-name (room-exits (car (room-exits H3))))


(define IMG-GRAPH-H4 (bitmap "../imgs/033-graph-H4.jpg"))
(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" empty)))
    -A-))


;; (length (room-exits H4)) ; 2
;; (map room-name (room-exits H4)) ; (list "B" "D")

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


;
; PROBLEM:
;
; Design a function that consumes a Room and a room name, and produces #t if
; it is possible to reach a room with the given name starting at the given
; room. For example:
;
;    (reachable? H1 "A") ; -> #t
;    (reachable? H1 "B") ; -> #t
;    (reachable? H1 "C") ; -> #f
;    (reachable? H4 "F") ; -> #t
;
; But note that if you defined H4F to be the room named F in the H4 house then
; `(reachable? H4F "A")` would produce #f because it is not possible to get to
; A from F in that house (you can't go back from F and F has no exit that would
; somehow lead to A.
;

;; Room String -> Boolean
;; Produce #t if, starting at `r0`, it is possible to reach `rn`; #f otherwise.
(check-expect (reachable? H1 "A") #t)
(check-expect (reachable? H1 "B") #t)
(check-expect (reachable? H1 "C") #f)
;; Can we, from H1B, go back to A? No, we cannot.
(check-expect (reachable? (car (room-exits H1)) "A") #f)
;; What about from H4B? Yes, we can.
(check-expect (reachable? (car (room-exits H4)) "A") #t)
(check-expect (reachable? H4 "F") #t)

;(define (reachable? r0 rn) #f) ; stub

(define (reachable? r0 rn)
  ;; todo is (list-of Room); a worklist accumulator.
  ;; visited is (list-of String); context-preseving accumulator containing
  ;;                              names of already visited rooms.
  (local [(define (fn-for-room r todo visited)
            (cond [(string=? (room-name r) rn) #t]
                  [(member (room-name r) visited) (fn-for-lor todo visited)]
                  [else
                   (fn-for-lor (append (room-exits r) todo)
                               (cons (room-name r) visited))]))

          (define (fn-for-lor todo visited)
            ;; If have have no more rooms to visit, then
            ;; we couldn't reach `rn`.
            (cond [(empty? todo) #f]
                  [else
                   (fn-for-room (car todo)
                                (cdr todo)
                                visited)]))]
    ;; trampoline
    (fn-for-room r0 empty empty)))

