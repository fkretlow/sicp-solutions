; Exercise 2.27
; =============
;
; Modify your ``reverse`` procedure of Exercise 2.18 to produce a
; ``deep-reverse`` procedure that takes a list as argument and returns as its
; value the list with its elements reversed and with all sublists deep-reversed
; as well. For example::
;
;     (define x (list (list 1 2) (list 3 4)))
;     x
;     ((1 2) (3 4))
;
;     (reverse x)
;     ((3 4) (1 2))
;
;     (deep-reverse x)
;     ((4 3) (2 1))


(define (deep-reverse xs)
  (define (iter a xs)
    (cond ((null? xs) a)
          ((pair? (car xs))
            (iter (cons (deep-reverse (car xs)) a)
                  (cdr xs)))
          (else
            (iter (cons (car xs) a)
                  (cdr xs)))))
  (iter '() xs))
