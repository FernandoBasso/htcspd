#lang htdp/asl

;; Fetches `list-ref`, `take` and `drop`.
(require racket/list)

;
; PROBLEM:
;
; In UBCs version of Systematic Program Design, there are often more than 800
; students taking the course in any given semester, meaning there are often
; over 40 Teaching Assistants.
;
; Designing a schedule for them by hand is hard work - luckily we've learned
; enough now to write a program to do it for us!
;
; Below are some data definitions for a simplified version of a TA schedule.
; There are some number of slots that must be filled, each represented by a
; natural number. Each TA is available for some of these slots, and has a
; maximum number of shifts they can work.
;
; Design a search program that consumes a list of TAs and a list of Slots, and
; produces all the possible schedules where each Slot is assigned to a TA, and
; no TA is working more than their maximum shifts. If no such schedules exist,
; produce false.
;

;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (list-of Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for
(define TA1 (make-ta "Harry"    1 (list 1   3   )))
(define TA2 (make-ta "Ron"      1 (list   2 3   )))
(define TA3 (make-ta "Hermione" 2 (list   2    4)))

(define TAS (list TA1 TA2 TA3))


(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;<template for generative recursion>
#;
(define (genrec-fn b)
  (cond [(trivial? b) (trivial-answer b)]
        [else
         (... b
              (genrec-fn (next-problem b)))]))


;; (list-of TA) (list-of Slot) -> (list-of Schedule)|#f
;; Produce all possible schedules or #f if no 'valid' schedules exist.
;(check-expect (all-schedules empty (list 1 2 3 4)) #f)
;(check-expect (all-schedules (list TA1 TA2 TA3) empty) #f)

;; TA1|1|(1-x-3-x)
;(check-expect (all-schedules (list TA1) (list 1 2 3 4)) #f)

;; TA1|1|(1-x-3-x)
;; TA2|1|(x-2-3-x)
;; #f because TA1 has one one shift available so this TA can either
;; work on slot 2 or 3, but not on both.

;(check-expect (all-schedules (list TA1 TA2) (list 1 2)) #f)

;; TA1|1|(1-x-3-x)
;; TA2|1|(x-2-3-x)
;; TA3|2|(x-2-x-4)
#;
(check-expect (all-schedules (list TA1 TA2 TA3) (list 1 2 3 4))
              (list (make-assignment TA1 1)
                    (make-assignment TA3 2)
                    (make-assignment TA2 3)
                    (make-assignment TA3 4)))

;; (define (all-schedules tas slots) #f) ; stub

;; (list-of TA) (list-of Slot) -> (list-of Schedule)
;; Produce all possible schedules, complete or not.
;; schedule is an empty schedule, just a list with slots to fill.

(define (all-schedules tas schedule)
  (cond [(schedule-complete? schedule) (list schedule)]
        [else
         (local [(define schedules (next-schedules tas schedule))]
           (append
            schedules
            (all-schedules tas (car schedules))
            (all-schedules tas (car (cdr schedules)))))]))


;; Schedule (list-of TA) -> (list-of Schedule)
;; Produce next schedules, filling first blank with each student.
(check-expect (next-schedules (list TA1 TA2 TA3) (list 1 2 3 4))
              (list (list (make-assignment TA1 1) 2 3 4)
                    (list (make-assignment TA2 1) 2 3 4)
                    (list (make-assignment TA3 1) 2 3 4)))

(define (next-schedules tas schedule)
  (cond [(empty? tas) empty]
        [else
         (local [(define pos (find-blank schedule))]
           (cons (fill-slot schedule pos (car tas))
                 (next-schedules (cdr tas) schedule)))]))

;; (list-of Slot) (list-of TA) -> Schedule
;; Produce a schedule with ta assigned to slot pos.
(check-expect (fill-slot (list 1 2 3) 1 TA1)
              (list 1 (make-assignment TA1 2) 3))

(define (fill-slot schedule pos ta)
  (append (take schedule pos)
          (list (make-assignment ta (add1 pos)))
          (drop schedule (add1 pos))))

;; (list-of Slot) -> Natural
;; Produce position of first non TA element in schedule.
(check-expect (find-blank (list 1 2 3)) 0)
(check-expect (find-blank (list TA1 TA2 3)) 2)

(define (find-blank schedule)
  (cond [(empty? schedule) (error "Oops, empty schedule, or no blanks...")]
        [else
         (if (number? (car schedule))
             0
             (+ 1 (find-blank (cdr schedule))))]))

;; (list-of Slot) -> Bool
;; Produce #t if slots is filled with only TAs, and no numbers are left in it.
(check-expect (schedule-complete? empty) #t)
(check-expect (schedule-complete? (list TA1 TA2 TA3)) #t)
(check-expect (schedule-complete? (list TA1 2 TA3)) #f)

(define (schedule-complete? slots)
  (cond [(empty? slots) #t]
        [(number? (car slots)) #f]
        [else (schedule-complete? (cdr slots))]))

;; (list-of Schedule) -> (list-of Schedule)
;; Produce list of only valid schedules.

;; '() because still has non-filled slots.
(check-expect (filter-valid (list (list (make-assignment TA1 1)
                                         (make-assignment TA2 2)
                                         3)))
              empty)

;; '() because TA2 has only one shift available but was assigned
;; to work on two shifts.

(check-expect (filter-valid (list (list (make-assignment TA1 1)
                                         (make-assignment TA2 2)
                                         (make-assignment TA2 3)
                                         (make-assignment TA3 4))))
              empty)

(check-expect (filter-valid (list (list (make-assignment TA1 1)
                                         (make-assignment TA3 2)
                                         (make-assignment TA2 3)
                                         (make-assignment TA3 4))))
              (list
               (list
                (make-assignment (make-ta "Harry" 1 (list 1 3)) 1)
                (make-assignment (make-ta "Hermione" 2 (list 2 4)) 2)
                (make-assignment (make-ta "Ron" 1 (list 2 3)) 3)
                (make-assignment (make-ta "Hermione" 2 (list 2 4)) 4))))

;; '() because TA C can only work on shift 1, but was assigned
;; slot 1 and 3.
(check-expect (filter-valid (list (list (make-assignment (make-ta "A" 1 (list 1)) 1)
                                         (make-assignment (make-ta "B" 1 (list 2)) 2)
                                         (make-assignment (make-ta "C" 1 (list 1)) 3))))
              empty)


;; #t because all slots have been filled and no TA has
;; worked more than their alloted shifts or shifts they
;; are not available for.
#;
(check-expect (filter-valid ()
                             #t))

;(define (filter-valid los) #f) ; stub

(define (filter-valid los)
  (cond [(empty? los) empty]
        [else
         (if (and (schedule-complete? (car los))
                  (not (overworks? (car los)))
                  (not (wrong-slot? (car los))))
             (cons (car los)
                   (filter-valid (cdr los)))
             (filter-valid (cdr los)))]))


;; Schedule -> Bool
;; Produce #t if some TA in schedule assigned to more shifts allowed.

;; #f because no TA was assigned to more than their alloted shifts.

(check-expect (overworks? (list (make-assignment (make-ta "A" 1 (list 1)) 1)
                                (make-assignment (make-ta "B" 1 (list 2)) 2)
                                (make-assignment (make-ta "C" 1 (list 1)) 3)))
              #f)

(check-expect (overworks? (list (make-assignment (make-ta "A" 1 (list 1)) 1)
                                (make-assignment (make-ta "B" 2 (list 2)) 2)
                                (make-assignment (make-ta "B" 2 (list 1)) 3)))
              #f)

(check-expect (overworks? (list (make-assignment (make-ta "A" 1 (list 1)) 1)
                                (make-assignment (make-ta "B" 1 (list 2)) 2)
                                (make-assignment (make-ta "B" 1 (list 1)) 3)))
              #t)

;(define (overworks? schedule) #f) ;stub

(define (overworks? schedule)
  ;; lcdrs gets called in cdr, lconst is always the
  ;; original schedule so we can count on the full thing.
  (local [(define (over-assigned? lcdrs lconst)
            (cond [(empty? lcdrs) #f]
                  [else
                   (let [(ta (assignment-ta (car lcdrs)))]
                     (if (> (times-assigned (ta-name ta) lconst) (ta-max ta))
                         #t
                         (over-assigned? (cdr lcdrs) lconst)))]))]
    (over-assigned? schedule schedule)))


;; String Shedule -> Natural
;; Count number of times name was used in schedule.
(define (times-assigned name schedule)
  (cond [(empty? schedule) 0]
        [else
         (if (string=? name (ta-name (assignment-ta (car schedule))))
             (+ 1 (times-assigned name (cdr schedule)))
             (times-assigned name (cdr schedule)))]))


;; Schedule -> Bool
;; Produce #t as soon as the first TA is assigned in a slot they shouldn't.

;; No TA was assigned to a slot they can't work in. Produce #f.
(check-expect (wrong-slot? (list (make-assignment TA1 1)
                                 (make-assignment TA3 2)
                                 (make-assignment TA2 3)
                                 (make-assignment TA3 4)))
              #f)

;; TA2 isn't available for slot 4, and was incorretly assigned to
;; work on slot 4. Produces #t.
(check-expect (wrong-slot? (list (make-assignment TA1 1)
                                 (make-assignment TA3 2)
                                 (make-assignment TA2 4)
                                 (make-assignment TA3 4)))
              #t)

;(defined (wrong-slot? schedule) #f) ;stub

(define (wrong-slot? schedule)
  ;; lcdrs gets called in cdr, lconst is always the
  ;; original schedule so we can count on the full thing.
  (local [(define (wrongly-assigned? lcdrs lconst)
            (cond [(empty? lcdrs) #f]
                  [else
                   (let [(ta (assignment-ta (car lcdrs)))]
                     (if (not (member (assignment-slot (car lcdrs)) (ta-avail ta)))
                         #t
                         (wrongly-assigned? (cdr lcdrs) lconst)))]))]
    (wrongly-assigned? schedule schedule)))

(define Erika (make-ta "Erika" 1 (list 1 3 7 9)))
(define Ryan (make-ta "Ryan" 1 (list 1 8 10)))
(define Reece (make-ta "Reece" 1 (list 5 6)))
(define Gordon (make-ta "Gordon" 2 (list 2 3 9)))
(define David (make-ta "David" 2 (list 2 8 9)))
(define Katie (make-ta "Katie" 1 (list 4 6)))
(define Aashish (make-ta "Aashish" 2 (list 1 10)))
(define Grant (make-ta "Grant" 2 (list 1 11)))
(define Raeanne (make-ta "Raeanne" 2 (list 1 11 12)))

(define taschedule (list 1 2 3 4 5 6 7 8 9 10 11 12))
(define tasquiz (list Erika Ryan Reece Gordon David Katie Aashish Grant Raeanne))

;; Q3
(filter-valid (all-schedules tasquiz taschedule)) ; 40951 combinations.
;; -> '(), the correct answer. Yes!

;; Q4
(define Alex (make-ta "Alex" 1 (list 7)))
(filter-valid (all-schedules (append tasquiz (list Alex)) taschedule)) ; 45046 combinations.
;; -> '(), correct again! Yes, Pinky!

;; Q5
(define Erin (make-ta "Erin" 1 (list 1 2 4 5 6 7 8 9 10 11 12)))
(filter-valid (all-schedules (append tasquiz (list Erin)) taschedule))
;; -> '(), which is incorrect. It should produce at least one valid schedule...

;;
;; TODO: come back to this problem after I go through "The Little Schemer",
;; some more relevant math, and perhaps the book HtDP (from which this course
;; was based on anyway).
;;

