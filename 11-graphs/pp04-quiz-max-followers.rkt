#lang htdp/asl
;
; PROBLEM 1:
;
; Consider a social network similar to Twitter called Chirper. Each
; user has a name, a note about whether or not they are a verified
; user, and follows some number of people.
;
; Design a data definition for Chirper, including a template that is
; tail recursive and avoids cycles.
;
; Then design a function called most-followers which determines which
; user in a Chirper Network is followed by the most people (not the
; user that follows most people, but the user followed by most people.
;


;; User is (make-user (String Bool (list-of User))
;; A user with name, #t/#f for `ǜerified` and a list of users they follow.
(define-struct user (name verified following))

(define yoda (make-user "Master Yoda" #t empty))

(define luke
  (shared ((-luke- (make-user "Luke Skywalker" #t (list -yoda-)))
           (-yoda- (make-user "Master Yoda" #t empty))
           (-leia- (make-user "Leia (princess" #f (list -luke- -yoda-)))
           (-obi-  (make-user "Obiwan Kenobi" #t (list -luke- -yoda- -leia-))))
    -luke-))


;;
;; Template:
;; ---------
;; - structural recursion
;; - encapsulate with `local`
;; - tail-recursive (because graphs can be quite large and we care
;;   about performance)
;; - a work list acc (tail-recursivenes requires some sort of accumulators)
;; - a context-preserving acc (to keep track of what users we have already visited)
;;


;; User -> User
;; Produce user that has most followers.
;(check-expect (most-followers yoda) yoda)

(check-expect (most-followers
               (shared ((-luke- (make-user "Luke Skywalker" #t (list -yoda-)))
                        (-yoda- (make-user "Master Yoda" #t empty))
                        (-leia- (make-user "Leia (princess" #f (list -luke- -yoda-)))
                        (-obi-  (make-user "Obiwan Kenobi" #t (list -luke- -yoda- -leia-))))
                 -luke-))
              (shared ((-luke- (make-user "Luke Skywalker" #t (list -yoda-)))
                       (-yoda- (make-user "Master Yoda" #t empty))
                       (-leia- (make-user "Leia (princess" #f (list -luke- -yoda-)))
                       (-obi-  (make-user "Obiwan Kenobi" #t (list -luke- -yoda- -leia-))))
                -yoda-))

(check-expect (most-followers
               (shared ((-luke- (make-user "Luke Skywalker" #t (list -leia-)))
                        (-yoda- (make-user "Master Yoda" #t (list -leia- -luke-)))
                        (-leia- (make-user "Leia (princess" #f (list -luke- -yoda- -obi-)))
                        (-obi-  (make-user "Obiwan Kenobi" #t (list -leia-))))
                 -luke-))
              (shared ((-luke- (make-user "Luke Skywalker" #t (list -leia-)))
                       (-yoda- (make-user "Master Yoda" #t (list -leia- -luke-)))
                       (-leia- (make-user "Leia (princess" #f (list -luke- -yoda- -obi-)))
                       (-obi-  (make-user "Obiwan Kenobi" #t (list -leia-))))
                -leia-))

(define (most-followers u0)
  ;; `visited` is (list-of User); a worklist accumulator.
  ;; `visited` is (list-of String); context-preseving accumulator containing
  ;;                              names of already visited users.
  ;; `rsf` is (list-of UserNumFollowers); the number of followers to users so far.
  (local [;; UserNumFollowers is (make-unf User Natural)
          ;; Interp: the number of followers recorded for a given user.
          (define-struct unf (u n))

          (define (fn-for-user u todo visited rsf)
            (if (member (user-name u) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (user-following u) todo)
                            (cons (user-name u) visited)
                            (merge-user u rsf))))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-user (car todo)
                                (cdr todo)
                                visited
                                rsf)]))
          ;; User (list-of UserNumFollowers) -> (list-of UserNumFollowers)
          ;; add one new user to rsf
          (define (merge-user u rsf)
            (foldr merge-follower rsf (user-following u)))

          ;; Room (list-of UserNumFollowers) -> (list-of UserNumFollowers)
          ;; add one new follower to rsf
          (define (merge-follower u lounf)
            (cond [(empty? lounf) (list (make-unf u 1))]
                  [else
                   (if (string=? (user-name u) (user-name (unf-u (car lounf))))
                       (cons (make-unf u (add1 (unf-n (car lounf))))
                             (cdr lounf))
                       (cons (car lounf)
                             (merge-follower u (cdr lounf))))]))

          ;; Pick the user from rsf that has the most followers.
          (define (pick-max rsf)
            (unf-u
             (foldr (λ (e1 e2)
                      (if (> (unf-n e1) (unf-n e2))
                          e1
                          e2))
                    (car rsf)
                    (cdr rsf))))]
    ;; trampoline
    (pick-max (fn-for-user u0 empty empty empty))))


