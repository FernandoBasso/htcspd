#lang htdp/asl

;
; PROBLEM:
;
; Using the following data definition, design a function that produces
; the room to which the greatest number of other rooms have exits (in
; the case of a tie you can produce any of the rooms in the tie).
;

;; Data Definitions:

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

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
           (-F- (make-room "F" (list))))
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
;; Produce room with which
;; No room exits to A. A exits to B so B has 1 other room exiting into it
;; and therefore B is the room with most exits to.
(check-expect (max-exits-to H1) (first (room-exits H1)))

(check-expect (max-exits-to H2)
              (shared ((-A- (make-room "A" (list -B-)))
                       (-B- (make-room "B" (list -A-))))
                -B-))

(check-expect (max-exits-to H4)
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -E-))

(define (max-exits-to r0)
  ;; `todo` is (listof Room); a worklist accumulator
  ;; `visited` is (listof String); context preserving accumulator,
  ;;                            `  names of rooms already visited
  ;; `rsf` is (listof RoomNExits); the number of exits leading to rooms so far
  (local [;; RoomNExits is (make-rne Room Natural)
          ;; interp. the number of exits recorded for a given room.
          (define-struct rne (r n))
          (define (fn-for-room r todo visited rsf-lorne)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf-lorne)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (merge-room r rsf-lorne))))

          (define (fn-for-lor todo visited rsf-lorne)
            (cond [(empty? todo) rsf-lorne]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited
                                rsf-lorne)]))

          ;; Room (list-of RoomNExits) -> (list-of RoomNExits)
          ;; add one new room to rsf
          (define (merge-room r rsf)
            (foldr merge-exit rsf (room-exits r)))

          ;; Room (list-of RoomNExits) -> (list-of RoomNExits)
          ;; add one new exit to rsf
          (define (merge-exit r lorne)
            (cond [(empty? lorne) (list (make-rne r 1))]
                  [else
                   (if (string=? (room-name r) (room-name (rne-r (first lorne))))
                       (cons (make-rne r
                                       (add1 (rne-n (first lorne))))
                             (rest lorne))
                       (cons (first lorne)
                             (merge-exit r (rest lorne))))]))

          ;; pick the room from rsf that has the most exits to it
          (define (pick-max rsf)
            (rne-r
             (foldr (λ (e1 e2)
                      (if (> (rne-n e1) (rne-n e2))
                          e1
                          e2))
                    (first rsf)
                    (rest rsf))))]
    ;; function composition
    (pick-max (fn-for-room r0 empty empty empty))))
