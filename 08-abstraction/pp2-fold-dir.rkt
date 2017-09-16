#lang htdp/isl
(require 2htdp/image)

;
; In this exercise you will be need to remember the following DDs
; for an image organizer.
;

;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

#;
(define (fn-for-dir d)
  (... (dir-name d)
       (fn-for-lod (dir-sub-dirs d))
       (fn-for-loi (dir-images d))))

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

#;
(define (fn-for-lod)
  (cond [(emtpy? lod) (...)]
        [else
         (... (fn-for-dir (first lod))
              (fn-for-lod (rest lod)))]))

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

#;
(define (fn-for-loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;
;                       D6
;                     /    \
;                    /      \
;                   D4      D5
;                 /    \     |
;                I1    I2    I3
;

;; =================
;; Functions:

;
; PROBLEM A:
;
; Design an abstract fold function for Dir called fold-dir.
;

#;
(define (fold-fn-for-dir d)
  (local [(define (fn-for-dir d)
            (... (dir-name d)
                 (fn-for-lod (dir-sub-dirs d))
                 (fn-for-loi (dir-images d))))

          (define (fn-for-lod)
            (cond [(emtpy? lod) (...)]
                  [else
                   (... (fn-for-dir (first lod))
                        (fn-for-lod (rest lod)))]))

          (define (fn-for-loi)
            (cond [(empty? loi) (...)]
                  [else
                   (... (first loi)
                        (fn-for-loi (rest loi)))]))
          ]))


;; (String Y Z -> X) (X Y -> Y) (Image -> Z) Y Z Dir -> X
;; The abstract function for Dir.

;; Get all dir names as a list.
(check-expect (local
                [(define (c1 name rlod rloi) (cons name rlod))]
                (fold-dir c1 append cons empty empty D6))
              (list "D6" "D4" "D5"))

;; Get all images as a list.
(check-expect (local [(define (c1 name rlod rloi) (append rlod rloi))]
                (fold-dir c1 append cons empty empty D6))
              (list I1 I2 I3))

(define (fold-dir combdir comblod combloi bdir bloi d)
  (local [
          ;; Dir -> X
          (define (fn-for-dir d)
            (combdir (dir-name d)
                     (fn-for-lod (dir-sub-dirs d))
                     (fn-for-loi (dir-images d))))

          ;; ListOfDir -> Y
          (define (fn-for-lod lod)
            (cond [(empty? lod) bdir]
                  [else
                   (comblod (fn-for-dir (first lod))
                            (fn-for-lod (rest lod)))]))

          ;; Image -> Z
          (define (fn-for-loi loi)
            (cond [(empty? loi) bloi]
                  [else
                   (combloi (first loi)
                            (fn-for-loi (rest loi)))]))
          ]
    (fn-for-dir d)))

;
; PROBLEM B:
;
; Design a function that consumes a Dir and produces the number of
; images in the directory and its sub-directories.
; Use the fold-dir abstract function.
;

;; Dir -> Natural
;; Produce number of images in Dir and sub-dirs.
(check-expect (count-images (make-dir "D0" empty empty)) 0)
(check-expect (count-images (make-dir "D0" empty (list I1 I2 I3 I1 I2))) 5)
(check-expect (count-images D6) 3)

(define (count-images dir)
  (local [
          (define (c1 name rlod rloi)
            (+ rlod rloi))

          (define (c2 rdir rlod)
            (+ rdir rlod))

          (define (c3 img rloi)
            (+ 1 rloi))]
    (fold-dir c1 c2 c3 0 0 dir)))



;
; PROBLEM C:
;
; Design a function that consumes a Dir and a String. The function looks in
; dir and all its sub-directories for a directory with the given name. If it
; finds such a directory it should produce true, if not it should produce false.
; Use the fold-dir abstract function.


;; Dir String -> Bool
;; Looks up for a dir with name n; produce #t if found, #f otherwise.
(check-expect (find-dir "D1" (make-dir "D0" empty empty)) #f)
(check-expect (find-dir "D0" (make-dir "D0" empty empty)) #t)
(check-expect (find-dir "D5" D6) #t)
(check-expect (find-dir "D11" D6) #f)

(define (find-dir n dir)
  (local [(define (c1 rn rlod rloi)
            (or (string=? n rn) rlod))
          (define (c2 rdir rlod)
            (or rdir rlod))]
    (fold-dir c1 c2 cons #f empty dir)))

;
; PROBLEM D:
;
; Is fold-dir really the best way to code the function from part C? Why or
; why not?

;
; No. Consider the case where the directory we are looking for is at the very
; root of the tree. As we have written the function in part C with fold-dir,
; even though find will produce true for the root node it will search the whole
; tree anyways.
;
; When we use fold-dir it isn't possible to implement find so that it stops
; searching as soon as it finds a directory with the right name. Instead it
; will always search the whole tree.
;
; NOTE: I copied this answer from the solution file.
;

