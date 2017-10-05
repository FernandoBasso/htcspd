#lang htdp/isl+
(require racket/list) ;gets list-ref, take and drop

;;
;; Brute force Sudoku solver
;;
;; In Sudoku, the board is a 9x9 grid of SQUARES.
;; There are 9 ROWS and 9 COLUMNS, there are also 9
;; 3x3 BOXES.  Rows, columns and boxes are all UNITs.
;; So there are 27 units.
;;
;; The idea of the game is to fill each square with
;; a Natural[1, 9] such that no unit contains a duplicate
;; number.
;;

;; =================
;; Data definitions:


;; Val is Natural[1, 9]

;; Board is (listof Val|false)   that is 81 elements long
;; interp.
;;  Visually a board is a 9x9 array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

;; Pos is Natural[0, 80]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)
;;    - the column is (remainder p 9)


;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 9) c))  ;helpful for writing tests


;; Unit is (listof Pos) of length 9
;; interp.
;;  The position of every square in a unit. There are
;;  27 of these for the 9 rows, 9 columns and 9 boxes.


;; =================
;; Constants:

(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))

(define B false) ;B stands for blank


(define BD1
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD2
  (list 1 2 3 4 5 6 7 8 9
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD3
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))

(define BD4                ;easy
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))

(define BD4s               ;solution to 4
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))

(define BD5                ;hard
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B
        B B B B B 6 B B B
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))

(define BD5s               ;solution to 5
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))

(define BD6                ;hardest ever? (Dr Arto Inkala)
  (list B B 5 3 B B B B B
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))

(define BD7                 ; no solution
  (list 1 2 3 4 5 6 7 8 B
        B B B B B B B B 2
        B B B B B B B B 3
        B B B B B B B B 4
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))




;; Positions of all the rows, columns and boxes:

(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))

(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))

(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

(define UNITS (append ROWS COLS BOXES))




;; =================
;; Functions:

;; Board -> Board|#f
;; Produces a solution for bd; or #f if bd is unsolvable.
;; ASSUME: bd is valid
;; !!! Uncomment later.
(check-expect (solve BD4) BD4s)
(check-expect (solve BD5) BD5s)
(check-expect (solve BD7) #f)

;(define (solve bd) #f) ;stub

(define (solve bd)
  (local [;; Board -> Board|#f
          (define (solve--bd bd)
            (if (solved? bd)
                bd
                (solve--lobd (next-boards bd))))

          ;; ListOfBoard -> ???
          (define (solve--lobd lobd)
            (cond [(empty? lobd) #f]
                  [else
                   (local [(define try (solve--bd (first lobd)))] ; try first sub-board
                     (if (not (false? try))
                         try
                         (solve--lobd(rest lobd))))]))]
    (solve--bd bd)))

;; Board -> Boolean
;; Produce #t if board os solved; #f otherwise.
;; ASSUME: Board is valid, so, it is solved if it is full (because
;;         we only keep valid boards while filling them in).
(check-expect (solved? BD1) #f)
(check-expect (solved? BD2) #f)
(check-expect (solved? BD4s) #t)


;(define (solved? bd) #f) ;stub

(define (solved? bd)
  (andmap number? bd))

;; Board -> (listof Board)
;; Produce list of valid next from board.
;; Finds first empty square, fills it with Natural[1, 9]
;; and keeps only valid boards.
(check-expect (next-boards (cons 1 (rest BD1)))
              (list (cons 1 (cons 2 (cdr (cdr BD1))))
                    (cons 1 (cons 3 (cdr (cdr BD1))))
                    (cons 1 (cons 4 (cdr (cdr BD1))))
                    (cons 1 (cons 5 (cdr (cdr BD1))))
                    (cons 1 (cons 6 (cdr (cdr BD1))))
                    (cons 1 (cons 7 (cdr (cdr BD1))))
                    (cons 1 (cons 8 (cdr (cdr BD1))))
                    (cons 1 (cons 9 (cdr (cdr BD1))))))

;(define (next-boards bd) empty) ;stub

(define (next-boards bd)
  (keep-only-valid (fill-with-1-9 (find-blank bd) bd)))


;; Board -> Pos
;; Produce the position of the first blank square.
;; ASSUME: the board has a least one blank square.
(check-expect (find-blank BD1) 0)
(check-expect (find-blank (cons 2 (cdr BD1))) 1)
(check-expect (find-blank (cons 2 (cons 7 (cdr BD1)))) 2)

;(define (find-blank bd) 0) ;stub

(define (find-blank bd)
  (cond [(empty? bd) (error "The board didn't have blank space.")]
        [else
         (if (false? (car bd)) ; Val|#f
             0
             ;; Find pos relative to the rest of the board.
             ;; That is why we add 1 each time.
             (+ 1 (find-blank (cdr bd))))]))

;; Pos Board -> (listof Board)
;; Produce 9 boards, with blank filled with Natural[1,9]
(check-expect (fill-with-1-9 0 BD1)
              (list (cons 1 (cdr BD1))
                    (cons 2 (cdr BD1))
                    (cons 3 (cdr BD1))
                    (cons 4 (cdr BD1))
                    (cons 5 (cdr BD1))
                    (cons 6 (cdr BD1))
                    (cons 7 (cdr BD1))
                    (cons 8 (cdr BD1))
                    (cons 9 (cdr BD1))))

;(define (fill-with-1-9 p bd) empty) ;stub

(define (fill-with-1-9 p bd)
  (local [(define (build-one n)
            ;; n is 0 -> 8, but we need values 1 -> 9.
            (fill-square bd p (+ n 1)))]
  (build-list 9 build-one)))

;; (listof Board) -> (listof Board)
;; Produce list containing only valid boards.
(check-expect (keep-only-valid
               (list (cons 1 (cons 1 (cdr (cdr BD1))))))
              empty)

;(define (keep-only-valid lobd) empty) ;stub

(define (keep-only-valid lobd)
  (filter valid-board? lobd))

;; Board -> Boolean
;; Produce #t if board is valid, #f otherwise.
;; Valid means no unit on the board has the same value more than once.
(check-expect (valid-board? BD1) #t)
(check-expect (valid-board? BD2) #t)
(check-expect (valid-board? BD3) #t)
(check-expect (valid-board? BD4) #t)
(check-expect (valid-board? BD5) #t)
(check-expect (valid-board? (cons 2 (cdr BD2))) #f)
(check-expect (valid-board? (cons 2 (cdr BD2))) #f)
(check-expect (valid-board? (fill-square BD4 1 6)) #f)

;(define (valid-board? bd) #f) ;stub
(define (valid-board? bd)
  (local [;; (listof Unit) -> Boolean
          (define (valid-units? lou)
            (andmap valid-unit? lou))

          ;; Unit -> Boolean
          ;; Produce #t if the unit is valid (no value appears more
          ;; than once in that unit).
          (define (valid-unit? u)
            (no-duplicates?
             (keep-only-values
              (read-unit u))))

          ;; Unit -> (listof Val|#f)
          ;; Produce a list of values and #f's.
          (define (read-unit u)
            (map read-pos u))

          ;; Pos -> Val|#f
          ;; Produce content of bd at p.
          (define (read-pos p)
            (read-square bd p))

          ;; (listof Val|#f) -> (listof Val)
          ;; Produce a list with numbers only.
          (define (keep-only-values lofv)
            (filter number? lofv))

          ;; (listof Val) -> Boolean
          ;; Produce #t if no value in lov appears more than once.
          (define (no-duplicates? lov)
            (cond [(empty? lov) #t]
                  [else
                   ;; No question mark in `member`‽
                   (if (member (car lov) (cdr lov))
                       #f
                        (no-duplicates? (cdr lov)))]
                  ))]

  (valid-units? UNITS)))


;; Board Pos -> Val or false
;; Produce value at given position on board.
(check-expect (read-square BD2 (r-c->pos 0 5)) 6)
(check-expect (read-square BD3 (r-c->pos 7 0)) 8)

(define (read-square bd p)
  (list-ref bd p))


;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill-square BD1 (r-c->pos 0 0) 1)
              (cons 1 (rest BD1)))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))


;
; We could have coded read-square and fill-square 'from scratch'
; by using the functions operating on 2 one-of data rule. If we
; had, the function definitions would look like this:
;
;
; ;
; ; Function on 2 complex data: Board and Pos.
; ; We can assume that p is <= (length bd).
; ;
; ;               empty     (cons Val-or-False Board)
; ;  0             XXX         (first bd)
; ;
; ;  (add1 p)      XXX         <natural recursion>
;
;
; (define (read-square bd p)
;   (cond [(zero? p) (first bd)]
;         [else
;          (read-square (rest bd) (sub1 p))]))
;
;
;
; ;
; ; Function on 2 complex data, Board and Pos.
; ; We can assume that p is <= (length bd).
; ;
; ;               empty     (cons Val-or-False Board)
; ;  0             XXX         (cons nv (rest bd))
; ;
; ;  (add1 p)      XXX         (cons (first bd) <natural recursion>)
;
; (define (fill-square bd p nv)
;   (cond [(zero? p) (cons nv (rest bd))]
;         [else
;          (cons (first bd)
;                (fill-square (rest bd) (sub1 p) nv))]))
;

