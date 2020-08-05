; Exercise 2.34
; =============
;
; Evaluating a polynomial in x at a given value of x can be formulated as an
; accumulation. We evaluate the polynomial
;
;     2x^3 + 3x^2 + 1x + 4
;
; using a well-known algorithm called *Horner's rule*, which structures the
; computation as
;
;     (((2)x + 3)x + 1)x + 4
;
; In other words, we start with a_n, multiply by x, add a_n-1, multiply by x,
; and so on, until we reach a_0.
;
; Fill in the following template to produce a procedure that evaluates a
; polynomial using Horner's rule. Assume that the coefficients of the
; polynomial are arranged in a sequence, from a_0 to a_n.


(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
