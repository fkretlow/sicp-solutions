; Exercise 1.46
; =============
;
; Several of the numerical methods described in this chapter are instances of
; an extremely general computational strategy known as *iterative improvement*.
; Iterative improvement says that, to compute something, we start with an
; initial guess for the answer, test if the answer is good enough, and
; otherwise improve the guess and continue the process using the improved guess
; as the new guess. Write a procedure ``iterative-improve`` that takes two
; procedures as arguments: a method for telling whether a guess is good enough
; and a method for improving a guess. ``iterative-improve`` should return as
; its value a procedure that takes a guess as argument and keeps improving the
; guess until it is good enough.

(define (iterative-improve good-enough? improve)
  (define (instance guess)
    (if (good-enough? guess)
        guess
        (instance (improve guess))))
  instance)


; Rewrite the ``sqrt`` procedure of Section 1.1.7 and the ``fixed-point``
; procedure of Section 1.3.3 in terms of ``iterative-improve``.

(define tolerance 0.00001)

(define (my-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) tolerance))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve good-enough? f) first-guess))
