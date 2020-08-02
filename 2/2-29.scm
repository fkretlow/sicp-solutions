; Exercise 2.29
; =============
;
; A binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of a certain length, from which hangs either a weight or
; another binary mobile. We can represent a binary mobile using compound data
; by constructing it from two branches (for example, using ``list``)::

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a ``length`` (wich must be a number) together
; with a ``structure``, which may be either a number (representing a simple
; weight) or another mobile::

(define (make-branch length structure)
  (list length structure))


; a. Write the corresponding selectors ``left-branch`` and ``right-branch``
; which return thre branches of a mobile, and ``branch-length`` and
; ``branch-structure`` which return the components of a branch.

(define (left-branch m) (car m))
(define (right-branch m) (cadr m))

(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))


; b. Using your selectors, define a procedure ``total-weight`` that returns the
; total weight of a mobile.

(define (total-weight m)
  (if (pair? m)
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))
      m))


; c. A mobile is said to be *balanced* if the torque applied by its top-left
; branch is equal to that applied by its top-right branch (that is, if the
; length of the left rod multiplied by the weight hanging from that rod is
; equal to the corresponding product for the right side) and if each of the
; submobiles hanging off its branches is balanced. Design a predicate that
; tests whether a binary mobile is balanced.

(define (balanced? m)
  (define (torque b)
    (* (branch-length b)
       (total-weight (branch-structure b))))
  (if (pair? m)
      (and (= (torque (left-branch m)) (torque (right-branch m)))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))
      #t))


; d. Suppose we change the representation of mobiles so that the constructors
; are::
;
;     (define (make-mobile left right) (cons left right))
;     (define (make-branch length structure) (cons length structure))
;
; How much do you need to change your program to convert to the new
; representation?

; -- Hardly at all. Just substitute ``cdr`` for ``cadr`` in the selectors.


; ***
; For testing::

(define m1 '((2 ((1 1) (1 1)))
             (2 ((1 1) (1 1)))))

(define m2 '((2 ((1 1) (1 2)))
             (2 ((1 1) (1 1)))))
