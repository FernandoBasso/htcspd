#lang htdp/isl+

;; Fetches `list-ref`, `take` and `drop`.
(require racket/list)

;
; PROBLEM 2:
;
; Below you will find some data definitions for a tic-tac-toe solver.
;
; In this problem we want you to design a function that produces all
; possible filled boards that are reachable from the current board.
;
; In actual tic-tac-toe, O and X alternate playing. For this problem
; you can disregard that. You can also assume that the players keep
; placing Xs and Os after someone has won. This means that boards that
; are completely filled with X, for example, are valid.
;
; Note: As we are looking for all possible boards, rather than a winning
; board, your function will look slightly different than the solve function
; you saw for Sudoku in the videos, or the one for tic-tac-toe in the
; lecture questions.
;

;
; http://www.se16.info/hgb/tictactoe.htm
; Answer from EDX: There are 512 possible ways to fill the empty board.
;

;; Value is one of:
;; - #f
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by #f) or has and "X" or an "O"

(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; Board is (listof Value)
;; All blank. A board is a list of 9 Values. 512 boards can be produced from
;; this initial, empty one.
;; 2 ^ 9 = 512
(define B0 (list #f #f #f
                 #f #f #f
                 #f #f #f))

;; One blank. A board where X will win. Can make two more boards out of this.
;; 2 ^ 1 = 2
(define B1 (list "X"  "X"  "O"
                 "O"  "X"  "O"
                 "X"   #f  "X"))

;; Two blanks. We can still make 4 boards out of this.
;; 2 ^ 2 = 4
(define B2 (list "X" "O" "X"
                 "O" "O" #f
                 "X" "X" #f))

;; A partly finished board. Three blanks. Can make 8 more boards out of this.
;; 2 ^ 3 = 8
(define B3 (list #f  "X" "O"
                 "O" "X" "O"
                 #f  #f  "X"))

;; A board with 5 blanks. Can make 32 more boards out of this.
;; 2 ^ 5 = 32
(define B5 (list "X" #f "O"
                 "O" #f #f
                 #f  "X" #f))

;<template for Board>
#;
(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))

;<template for generative recursion>
#;
(define (genrec-fn b)
  (cond [(trivial? b) (trivial-answer b)]
        [else
         (... b
              (genrec-fn (next-problem b)))]))

;; Board -> (listof Board)
;; Produce all possible filled boards from bd.

(check-expect (all-boards (list "X" "O" "X" "O" "X" "O" "X" #f "O"))
              (list (list "X" "O" "X" "O" "X" "O" "X" "X" "O")
                    (list "X" "O" "X" "O" "X" "O" "X" "O" "O")))

(check-expect (length (all-boards B0)) 512) ; 2 ^ 9 = 512
(check-expect (length (all-boards B1)) 2)   ; 2 ^ 1 = 2
(check-expect (length (all-boards B2)) 4)   ; 2 ^ 2 = 4
(check-expect (length (all-boards B3)) 8)   ; 2 ^ 3 = 8
(check-expect (length (all-boards B5)) 32)  ; 2 ^ 5 = 32

(define (all-boards b)
  (cond [(full? b) (list b)]
        [else
         (local [(define bds (next-boards b))]
           (append
            (all-boards (car bds))
            (all-boards (car (cdr bds)))))]))

;; (length (all-boards B0)) gives 512.
;; (length (all-boards B2)) gives 2 because B2 has only one remaining blank.
;; (length (all-boards B3)) gives 4 because B3 has two remaining blanks.


;; Board -> (listof Board)
;; Produce list with only valid boards.
(check-expect
 (filter-valid-bds
  (list
   (list "X" "X" "X" "X" "X" "O" "O" "O" "O")    ; Valid ----: 5 X's.
   (list "X" "X" "X" "X" "O" "O" "O" "O" "O")    ; Invalid --: 4 X's.
   (list "X" "O" "X" "O" "X" "O" "X" "O" "X")))  ; Valid ----: 5 X's.
 (list
  (list "X" "X" "X" "X" "X" "O" "O" "O" "O")     ; Valid ----: 5 X's. Should be kept.
  ; (list "X" "X" "X" "X" "O" "O" "O" "O" "O")   ; Invalid --: 4 X's. Should be filtered out.
  (list "X" "O" "X" "O" "X" "O" "X" "O" "X")))   ; Valid ----: 5 X's. Shold be kept.


;(define (filter-valid-bds b) b) ; stub

(define (filter-valid-bds bds)
  (cond [(empty? bds) '()]
        [else
         (if (valid-bd? (car bds))
             (cons (car bds) (filter-valid-bds (cdr bds)))
             (filter-valid-bds (cdr bds)))]))

(check-expect (length (filter-valid-bds (all-boards B0))) 126)
(check-expect (length (filter-valid-bds (all-boards B1))) 1)
(check-expect (length (filter-valid-bds (all-boards B2))) 2)
(check-expect (length (filter-valid-bds (all-boards B3))) 3)
(check-expect (length (filter-valid-bds (all-boards B5))) 10)

;; Board -> ListOfBoard
;; Given a board, make a list of boards that fill in the first #f.
(check-expect (next-boards B0)
              (list (list "X" #f #f #f #f #f #f #f #f)
                    (list "O" #f #f #f #f #f #f #f #f)))

(check-expect (next-boards (list "X" "O" #f #f #f #f #f #f #f))
              (list (list "X" "O" "X" #f #f #f #f #f #f)
                    (list "X" "O" "O" #f #f #f #f #f #f)))

(define (next-boards bd)
  (fill-with-x-o (find-blank bd) bd))

;; Board -> Pos
;; Produce the position of the first blank square.
;; ASSUME: the board has a least one blank square.
(check-expect (find-blank B0) 0)
(check-expect (find-blank B1) 7)

(define (find-blank bd)
  (cond [(empty? bd) (error "Oops! Got a board without blanks...")]
        [else
         (if (false? (car bd)) ; Val|#f
             0
             ;; Find pos relative to the rest of the board.
             ;; That is why we add 1 each time.
             (+ 1 (find-blank (cdr bd))))]))


;; Pos Char Board -> (listof Board)
;; Produce 2 boards, with blank filled with Char "X" or "O".
(check-expect (fill-with-x-o 0 (list #f #f #f #f #f #f #f #f #f))
              (list (list "X" #f #f #f #f #f #f #f #f)
                    (list "O" #f #f #f #f #f #f #f #F)))

(check-expect (fill-with-x-o 5 (list #f #f #f #f #f #f #f #f #f))
              (list (list #f #f #f #f #f "X" #f #f #f)
                    (list #f #f #f #f #f "O" #f #f #F)))

(define (fill-with-x-o p bd)
  (local [(define chars (list "X" "O"))

          (define (pick-char n chrs)
            (if (= n 0) (car chrs) (car (cdr chrs))))

          (define (build-one n)
            (fill-square bd p (pick-char n chars)))]

    (build-list 2 build-one)))


;; Board -> Bool
;; Produce #t if board is full; #f otherwise.
(check-expect (full? (list "X" #f #f #f #f #f #f #f #f)) #f)
(check-expect (full? (list "X" "O" "X" "O" "X" "O" "X" "O" #f)) #f)
(check-expect (full? (list "X" "O" "X" "O" "X" "O" "X" "O" "X")) #t)

#;
(define (full? b)
  (cond [(empty? b) #t]
        [else
         (if (false? (car b))
             #f
             (full? (rest b)))]))

(define (full? b)
  (cond [(empty? b) #t]
        [(false? (car b)) #f]
        [else (full? (rest b))]))


;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill-square B0 0 "X")
              (cons "X" (cdr B0)))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))


;
; PROBLEM 3:
;
; Now adapt your solution to filter out the boards that are impossible if
; X and O are alternating turns. You can continue to assume that they keep
; filling the board after someone has won though.
;
; You can assume X plays first, so all valid boards will have 5 Xs and 4 Os.
;
; NOTE: make sure you keep a copy of your solution from problem 2 to answer
; the questions on edX.
;


;; Board -> Natural
;; Produce the number "X"s in the board.
(check-expect (count-x (list "X" "X" "X" "X" "O" "O" "O" "O" "O")) 4)
(check-expect (count-x (list "X" "O" "X" "O" "X" "O" "X" "O" "X")) 5)
(check-expect (count-x (list "X" "X" "X" "X" "X" "O" "O" "O" "O")) 5)

;(define (valid-bd? bd) #f)

(define (count-x b)
  (cond [(empty? b) 0]
        [else
         (if (string=? (car b) "X")
             (+ 1 (count-x (cdr b)))
             (count-x (cdr b)))]))


;; Board -> Boolean
;; Produce #t if board has 5 "X"s, meaning it is valid.
(check-expect (valid-bd? (list "X" "X" "X" "X" "O" "O" "O" "O" "O")) #f)
(check-expect (valid-bd? (list "X" "O" "X" "O" "X" "O" "X" "O" "X")) #t)
(check-expect (valid-bd? (list "X" "X" "X" "X" "X" "O" "O" "O" "O")) #t)

(define (valid-bd? b)
  (= (count-x b) 5))

