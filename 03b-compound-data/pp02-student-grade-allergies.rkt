#lang htdp/bsl

;; ================
;; DATA DEFINITIONS

;
; PROBLEM A:
;
; Design a data definition to help a teacher organize their next field trip.
; On the trip, lunch must be provided for all students. For each student, track
; their name, their grade (from 1 to 12), and whether or not they have allergies.
;

(define-struct student (name grade allergies))
;; Student is (make-student String Natural[1, 12] Boolean)
;; Interp: (make-student name grade allergies) is a student with a
;;         - name
;;         - grade from 1 to 12
;;         - a boolean flag indicating whether they have allergies or not
(define S1 (make-student "Luke Skywalker" 3 #false))
(define S2 (make-student "Obiwan Kenobi" 12 #true))

#;
(define (fn-for-student s)
  (... (student-name s)            ; String
       (student-grade s)           ; Natural[1, 12]
       (student-allergies s)))     ; Boolean

;; Template rules used:
;;  - compound: 3 fields


;; =========
;; FUNCTIONS

;
; PROBLEM B:
;
; To plan for the field trip, if students are in grade 6 or below, the teacher
; is responsible for keeping track of their allergies. If a student has allergies,
; and is in a qualifying grade, their name should be added to a special list.
; Design a function to produce true if a student name should be added to this list.
;

;; Student -> Boolean
;; Produce #true if student is in grade 6 or bellow and have allergies.
(check-expect (add-to-list? (make-student "Obiwan Kenobi" 3 #true)) #true)
(check-expect (add-to-list? (make-student "Leia Skywalker" 2 #false)) #false)
(check-expect (add-to-list? (make-student "Ada Lovelace" 9 #false)) #false)
(check-expect (add-to-list? (make-student "Linus Torvalds" 12 #true)) #false)

;(define (add-to-list? s) #false) ;stub

;<use template from Student>
(define (add-to-list? s)
  (and (<= (student-grade s) 6) (equal? (student-allergies s) #true)))


;; Cases to test:
;;  - <= 6 and have allergies;
;;  - <= 6 and doesn't have allergies;
;;  -  > 6 and have allergies;
;; -   > 6 and doesn't have allergies;

