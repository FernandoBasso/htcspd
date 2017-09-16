#lang htdp/isl

(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))


; PROBLEM 1:
;
; Design an abstract function (including signature, purpose, and tests)
; to simplify the remove-debtors and remove-profs functions defined below.
;
; Now re-define the original remove-debtors and remove-profs functions
; to use your abstract function. Remember, the signature and tests should
; not change from the original functions.


;; (Accounts -> Boolean) -> Accounts -> Accounts
;; The abstract function to remove accounts based on the predicate.

;; Remove if id is an even number.
(check-expect (local [(define (p act)
                        (even? (node-id act)))]
                (remove-from-accounts p (make-node 1 "A" 10 #f #f)))
              (make-node 1 "A" 10 #f #f))

(define (remove-from-accounts pred? act)
  (cond [(false? act) #f]
        [else
         (if (pred? act)
             (join (remove-from-accounts pred? (node-l act))
                   (remove-from-accounts pred? (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-from-accounts pred? (node-l act))
                        (remove-from-accounts pred? (node-r act))))]))

;; Accounts -> Accounts
;; remove all accounts with a negative balance
(check-expect (remove-debtors (make-node 1 "Mr. Rogers" 22 false false))
              (make-node 1 "Mr. Rogers" 22 false false))

(check-expect (remove-debtors (make-node 14 "Mr. Impossible" -9 false false))
              #f)

(check-expect (remove-debtors
               (make-node 27 "Mr. Selatcia" 40
                          (make-node 14 "Mr. Impossible" -9 false false)
                          false))
              (make-node 27 "Mr. Selatcia" 40 false false))

(check-expect (remove-debtors
               (make-node 4 "Mrs. Doubtfire" -3
                          false
                          (make-node 7 "Mr. Natural" 13 false false)))
              (make-node 7 "Mr. Natural" 13 false false))

#;
(define (remove-debtors act)
  (cond [(false? act) false]
        [else
         (if (negative? (node-bal act))
             (join (remove-debtors (node-l act))
                   (remove-debtors (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-debtors (node-l act))
                        (remove-debtors (node-r act))))]))


(define (remove-debtors act)
  (local [(define (p act)
            (negative? (node-bal act)))]
    (remove-from-accounts p act)))


;; Accounts -> Accounts
;; Remove all professors' accounts.
(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false))
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false)) false)
(check-expect (remove-profs (make-node 67 "Mrs. Dash" 3000
                                       (make-node 9 "Prof. Booty" -60 false false)
                                       false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs
               (make-node 97 "Prof. X" 7
                          false
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))


(define (remove-profs act)
  (local [(define (p act)
            (has-prefix? "Prof." (node-name act)))]
    (remove-from-accounts p act)))


;; String String -> Boolean
;; Determine whether pre is a prefix of str.
(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))

;; Accounts Accounts -> Accounts
;; Combine two Accounts's into one
;; ASSUMPTION: all ids in act1 are less than the ids in act2
(check-expect (join ACT42 false) ACT42)
(check-expect (join false ACT42) ACT42)
(check-expect (join ACT1 ACT4)
              (make-node 4 "Mrs. Doubtfire" -3
                         ACT1
                         (make-node 7 "Mr. Natural" 13 false false)))
(check-expect (join ACT3 ACT42)
              (make-node 42 "Mr. Mom" -79
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9
                                               ACT3
                                               false)
                                    false)
                         (make-node 50 "Miss 604" 16 false false)))

(define (join act1 act2)
  (cond [(false? act2) act1]
        [else
         (make-node (node-id act2)
                    (node-name act2)
                    (node-bal act2)
                    (join act1 (node-l act2))
                    (node-r act2))]))


; PROBLEM 2:
;
; Using your new abstract function, design a function that removes from a given
; BST any account where the name of the account holder has an odd number of
; characters. Call it remove-odd-characters.


;; Accounts -> Accounts
;; Produce accounts with nodes whose name has odd number of chars removed.
(check-expect (remove-odd-chars #f) #f)
(check-expect (remove-odd-chars (make-node 1 "Prof" 100 #f #f))
              (make-node 1 "Prof" 100 #f #f))

(check-expect (remove-odd-chars (make-node 1 "Foo" 100 #f #f))
              #f)

(check-expect (remove-odd-chars
               (make-node 1 "AB" 10
                          (make-node 2 "CD" 22 #f #f)
                          (make-node 3 "EFGH" 33 #f #f)))
              (make-node 1 "AB" 10
                         (make-node 2 "CD" 22 #f #f)
                         (make-node 3 "EFGH" 33 #f #f)))

(check-expect (remove-odd-chars ACT42)
              (make-node 50 "Miss 604" 16
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9 #f #f) #f) #f))

(define (remove-odd-chars act)
  (local [(define (p act)
            (len-odd? (node-name act)))]
    (remove-from-accounts p act)))


;; String -> Bool
;; Produce true if s has an odd number of chars.
(check-expect (len-odd? "") #f)
(check-expect (len-odd? "M") #t)
(check-expect (len-odd? "Yoda") #f)
(check-expect (len-odd? "Vader") #t)

(define (len-odd? s)
  (odd? (string-length s)))

; Problem 3:
;
; Design an abstract fold function for Accounts called fold-act.
;
; Use fold-act to design a function called charge-fee that decrements
; the balance of every account in a given collection by the monthly fee of 3 CAD.


;; (Natural String Number Accounts Accounts -> X) X Accounts -> X
;; The abstract function for Accounts.
(check-expect (local [(define (fn rid rname rbal rl rr)
                        (cons rname (append rl rr)))]
                (fold-act fn empty ACT42))
              (list "Mr. Mom" "Mr. Selatcia" "Mr. Impossible" "Miss 604"))

(define (fold-act fn b act)
  (cond [(false? act) b]
        [else
         (fn (node-id act)
             (node-name act)
             (node-bal act)
             (fold-act fn b (node-l act))
             (fold-act fn b (node-r act)))]))

; PROBLEM 4:
;
; Suppose you needed to design a function to look up an account based on its ID.
; Would it be better to design the function using fold-act, or to design the
; function using the fn-for-acts template?  Briefly justify your answer.


;
; Using fn-for-acts template would be better because we are done as soon
; as we find the account (a value is returned and further searching is
; stopped). With fold-act even if we find the account
; we continue recursing up to the end of all accounts, unnecessarily.
;

