#lang htdp/isl

;
; PROBLEM #1:
;
; Suppose you have roster for players on two opposing tennis teams, and
; each roster is ordered by team rank, with the best player listed first.
; When both teams play, the best players of each team play one another,
; and the second-best players play one another, and so on down the line.
; When one team has more players than the other, the lowest ranking
; players on the larger team do not play.
;
; Design a function that consumes two rosters, and produces #t if all
; players on both teams will play if the teams play each other.
; No marks will be given to a solution that do not use a cross product
; table.
;

;; Player is String
;; Interp: the name of a tennis player.
(define P0 "Maria")
(define P1 "Serena")

#;
(define (fn-for-player p)
  (... p))

;; Roster is one of:
;;  - empty
;;  - (cons Player Roster)
;; Interp: a team roster, ordered from best player to worst.
(define R0 '())
(define R1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define R2 (list "Maria" "Nadia" "Elena" "Anastasia" "Svetlana"))

#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))

(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; Interp: a match between p1 and p2 (with same team rank)
;; ASSUME: both teams have the same number of players.
(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))

#;
(define (fn-for-match m)
  (... (match-p1 m)
       (match-p2 m)))

;; ListOfMatch is one of:
;;  - empty
;;  - (cons Match ListOfMatch)
;; Interp: a list of match between one team and another.
(define LOM0 '())
(define LOM1 (list (make-match "Eugenie" "Maria")
                   (make-match "Gabriela" "Nadia")))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-match (first lom))
              (fn-for-match (rest lom)))]))

;; Roster Roster -> Boolean
;; Consumes two rosters that will play each other; produce #t if
;; all players on both teams get to play.
(check-expect (all-play? '() '()) #t)
(check-expect (all-play? '() R1) #f)
(check-expect (all-play? R1 '()) #f)
(check-expect (all-play? (list "Eugenie" "Gabriela")
                         (list "Sharon" "Aleksandra")) #t)
(check-expect (all-play? (list "Eugenie" "Gabriela" "Erika")
                         (list "Sharon" "Aleksandra")) #f)

;
; +---------------------+--------+----------------------+
; | r1                  | '()    | (cons Player Roster) |
; | r2                  |        |                      |
; |---------------------+--------+----------------------|
; | '()                 | #t (1) | #f (2)               |
; |---------------------+--------+----------------------|
; | (cons Player Roster | #f (2) | (all-play? <rests>)  |
; +---------------------+--------+----------------------+
;

(define (all-play? r1 r2)
  (cond [(and (empty? r1) (empty? r2)) #t]
        [(or (empty? r1) (empty? r2)) #f]
        [else
         (all-play? (rest r1) (rest r2))]))


;
; PROBLEM #2:
;
; Now write a function that, given two teams, produces the list
; of tennis matches that will be played.
;
; Assume that this function will only be called if the function
; you designed above produces #t. In other words, you can assume
; the two teams have the same number of players.
;

;; Roster Roster -> ListOfMatch
;; Consumes two rosters and produces the list of matches that
;; will be played between them.

(check-expect (matches '() '()) '())
(check-expect (matches (list "Eugenie" "Gabriela")
                       (list "Sharon" "Aleksandra"))
              (list (make-match "Eugenie" "Sharon")
                    (make-match "Gabriela" "Aleksandra")))

;
; | r1                   | '()        | (cons Player Roster)       |
; | r2                   |            |                            |
; |----------------------+------------+----------------------------|
; | '()                  | '() (1)    | impossible                 |
; |----------------------+------------+----------------------------|
; | (cons Player Roster) | impossible | (cons (make-match <firsts> |
; |                      |            |       (matches <rests>)))  |
; |                      |            | (#2)                       |
;

(define (matches r1 r2)
  (cond [(empty? r1) '()]
        [else
         (cons (make-match (first r1) (first r2))
               (matches (rest r1) (rest r2)))]))

