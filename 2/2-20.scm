; Exercise 2.20
; =============
;
; Use [dotted tail] notation to write a procedure ``same-parity`` that takes
; one or more integers and returns a list of all the arguments that have the
; same even-odd parity as the first argument. For example::
;
; (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
; (same-parity 2 3 4 5 6 7)
; (2 4 6)


; Of course ``filter`` is built in::
;
; (define (filter predicate? xs)
;   (cond ((null? xs) '())
;         ((predicate? (car xs))
;            (cons (car xs) (filter predicate? (cdr xs))))
;         (else (filter predicate? (cdr xs)))))

(define (same-parity . xs)
  (let ((predicate? (if (odd? (car xs)) odd? even?)))
    (filter predicate? xs)))
