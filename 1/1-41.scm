; Exercise 1.41
; =============
;
; Define a procedure that takes a procedure of one argument as argument and
; returns a procedure that applies the original procedure twice. For example,
; if ``inc`` is a procedure that adds 1 to its argument, then ``(double inc)``
; should be a procedure that adds 2.

(define (double f) (lambda (x) (f (f x))))

; What value is returned by ``(((double (double double)) inc) 5)``?
; 5 + 16 = 21
; ``(double double)`` is a procedure that applies its argument 4 times.
; ``(double (double double)) is a procedure that applies its argument 4 * 4 = 16 times.
