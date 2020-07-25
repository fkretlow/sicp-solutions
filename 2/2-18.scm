; Exercise 2.18
; =============
;
; Define a procedure ``reverse`` that takes a list as argument and returns a
; list of the same elements in reverse order::
;
;     (reverse (list 1 4 9 16 25))
;     (25 16 9 4 1)


(define (reverse l)
  (define (reverse-iter out in)
    (if (null? in)
        out
        (reverse-iter (cons (car in) out) (cdr in))))
  (reverse-iter '() l))

; It currently escapes me how to do this efficiently with linear recursion on a
; singly linked list. Of course we could use ``append`` and ``last-pair``, but
; that would be horribly inefficient because we would walk the remaining list
; to the end in every step.
