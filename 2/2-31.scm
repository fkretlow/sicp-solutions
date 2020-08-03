; Exercise 2.31
; =============
;
; Abstract your answer to Exercise 2.30 to produce a procedure ``tree-map``
; with the property that ``square-tree`` could be defined as::
;
;     (define (square-tree tree) (tree-map square tree))


(define (tree-map f tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (tree-map f (car tree))
                            (tree-map f (cdr tree))))
        (else (f tree))))
