#lang htdp/bsl

;
; As part of designing a system to keep track of student grades, you
; are asked to design a data definition to represent the letter grade
; in a course, which is one of A, B or C.
;

;; LetterGrade is one of:
;;  - "A"
;;  - "B"
;;  - "C"
;; Interp: the letter grade in a course.
;; <examples are redundant for enumerations>.

#;
(define (frn-for-letter-grade lg)
  (cond [(string=? lg "A") (...)]
        [(string=? lg "B") (...)]
        [(string=? lg "C") (...)]))

;; Template fules used:
;;  - one of: 3 cases
;;    - atomic distinct value: "A"
;;    - atomic distinct value: "B"
;;    - atomic distinct value: "C";

