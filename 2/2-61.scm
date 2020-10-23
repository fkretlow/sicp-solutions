; Exercise 2.61
; =============
;
; Give an implementation of ``adjoin-set`` using the ordered representation. By
; analogy with ``element-of-set?`` show how to take advantage of the ordering
; to produce a procedure that requires on the average about half as many steps
; as with the unordered representation.

(define (adjoin-set x set)
  (cond ((= x (car set)) set)
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))
        (else (cons x set))))

; We only need to compare elements until we find one that is equal to or
; greater than the new value. Then we can just take the rest of the set as is.
