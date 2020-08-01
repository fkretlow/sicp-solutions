; Exercise 2.28
; =============
;
; Write a procedure ``fringe`` that tahes as argument a tree (represented as a
; list) and returns a list whose elements are all the leaves of the tree
; arranged in left-to-right order. [...]


(define (fringe xs)
  (define (iter out in)
    (cond ((null? in) out)
          ((pair? (car in)) (iter (append out (fringe (car in))) (cdr in)))
          (else (iter (append out (list (car in))) (cdr in)))))
  (iter '() xs))

; Again: Is there a more efficient way? For large trees the pointer-overhead
; for doubly-linked lists must be a reasonable tradeoff for speed.
