#lang htdp/bsl

(define-struct student (name id))
;; Student is (make-student String Natural)
;; Interp: A student with a name and id.
(define S1 (make-student "Eva" 3124))
(define S2 (make-student "Ada" 7893))

#;
(define (fn-for-student s)
  (... (student-name s)
       (student-id s)))

;; ListOfStudent is one of:
;; - '()
;; - (cons Student ListOfStudent)
;; Interp: A list of students.
(define LOS1 '())
(define LOS2 (cons S1 '()))
(define LOS3 (cons S1 (cons S2 '())))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-student (first los))
              (fn-for-los (rest los)))]))

;
; Design a  function that consumes a  list of students and  produces a
; list of student cards, where each student card contains the name and
; ID of the student.  Assume a student card is simply  a string of the
; form:
;
;     "<student name> <student id>"
;
; For example:
;
; (student-cards
;  (cons
;   (make-student "John" 7893
;                 (cons
;                  (make-student "Eva" 3124)
;                  empty))))
;
; should produce
;
;    (cons "John 7893" (cons "Eva 3124" empty))
;

;; ListOfStudent -> ListOfString
;; Produces a list of student card in the form "<name> <id>".
(check-expect (student-cards '()) '())
(check-expect (student-cards (cons (make-student "Eva" 21) '()))
              (cons "Eva 21" '()))

;; The missing check-expect:
;; We also need a check-expect takes a list that is at least
;; two elements long, because it will show us errors in the
;; natural recursion.
(check-expect (student-cards
               (cons
                (make-student "Eva" 31)
                (cons (make-student "Ada" 49) '())))
              (cons "Eva 31" (cons "Ada 49" '())))

;(define (student-cards s) '()) ;stub

(define (student-cards los)
  (cond [(empty? los) '()]
        [else
         (cons (make-card (first los))
               (student-cards (rest los)))]))


;; Student -> String
;; Produce card as "<name> <id>" from given student.
(check-expect (make-card (make-student "Linus" 1)) "Linus 1")

;(define (make-card s) "") ;student

(define (make-card s)
  (string-append
   (student-name s)
   " "
   (number->string (student-id s))))

