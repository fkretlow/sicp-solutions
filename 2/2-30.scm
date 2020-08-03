; Exercise 2.30
; =============

; Define a procedure ``square-tree`` analogous to the ``square-list`` procedure
; of Exercise 2.2.1. That is, ``square-tree`` should behave as follows::

;     (square-tree '(1 (2 (3 4) 5) (6 7)))
;     (1 (4 (9 16) 25) (36 49))

; Define ``square-tree`` both directly (i.e. without using any higher-order
; procedures) and also by using ``map`` and recursion.


(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))


(define (square-tree2 tree)
  (map (lambda (tree)
         (if (pair? tree)
             (square-tree2 tree)
             (square tree)))
       tree))
