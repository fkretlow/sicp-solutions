; Exercise 2.63
; =============

(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))

; Each of the following two procedures converts a binary tree to a list.

(define (tree->list1 tree)
  (if (null? tree)
      '()
      (append (tree->list1 (left-branch tree))
              (cons (entry tree)
                    (tree->list1 (right-branch tree))))))

(define (tree->list2 tree)
  (define (iter tree result)
    (if (null? tree)
        result
        (iter (left-branch tree)
              (cons (entry tree)
                    (iter (right-branch tree) result)))))
  (iter tree '()))

; a) Do the two procedures produce the same result for every tree? If not, how do the
; results differ? What lists do the two procedures produce for the trees in Figure 2.16?

; Both procedures produce a list of the elements of the tree in sorted order. The trees
; in Figure 2.16 are converted to (1 3 5 7 9 11).

; b) Do the two procedures have the same order of growth in the number of steps required
; to convert a balanced tree with n elements to a list? If not, which one grows more
; slowly?

; No. The first procedure calls append on the list of the left branch, which runs in
; linear time. There are n calls. The first call walks through 1/2*n elements and
; generates two child calls. Each of these walk through 1/4*n elements, that is 1/2*n
; elements in total again. On each of the log2 n "layers" of the tree, 1/2*n steps are
; added. That gives log2(n) * 1/2n + n steps, or Theta(n log(n)).
; The second procedure only calls cons, which runs in constant time. That gives
; linear time Theta(n).
