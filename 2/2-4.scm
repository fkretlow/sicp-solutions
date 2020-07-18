; Exercise 2.4
; ============
;
; Here is an alternative procedural representation of pairs. For this
; representation, verify that ``(car (cons x y))`` yields x for any objects x
; and y. What is the corresponding definition of  ``cdr``? (Hint: To verify
; that this works, make use of the substitution model of Section 1.1.5.)

; (cons, car and cdr renamed to pair, first and second to avoid name clashes
; during testing.)

(define (pair x y)
  (lambda (m) (m x y)))
(define (first p)
  (p (lambda (x y) x)))
(define (second p)
  (p (lambda (x y) y)))

; This is beautifully entangled! ``pair`` returns a procedure that takes as
; argument another procedure of two arguments: x and y. Now ``first`` applies
; the procedure that ``pair`` returned to an anonymous procedure of two
; arguments that simply returns its first argument. The small roundtrip
; ultimately reduces to x. Likewise, ``second`` applies the procedure to an
; anonymous procedure that returns its second argument. You could say that in
; this case the actual accessor methods are defined in ``first`` and
; ``second``, while ``pair`` receives and executes the methods with its
; specific data objects as arguments.
