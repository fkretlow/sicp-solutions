; Exercise 1.36
; =============
;
; Modify ``fixed-point`` so that it prints the sequence of approximations it
; generates using the ``newline`` and ``display`` primitives shown in Exercise
; 1.22. Then find a solution to x^x = 1000 by finding a fixed point of x ->
; log(1000) / log(x). (Use Scheme's primitive ``log`` procedure, which computes
; natural logarithms.) Compare the number of steps this takes with and without
; average damping. (Note that you cannot start ``fixed-point`` with a guess of
; 1, as this would cause division by log(1) = 0.)


(define PRECISION 0.00001)

(define (fixed-point f initial-guess)
  (define (close-enough? a b) (< (abs (- a b)) PRECISION))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try initial-guess))

(define (f x) (/ (log 1000) (log x)))
(define (f-damped x)
  (define (average a b) (/ (+ a b) 2))
  (average x (f x)))

; Average damping reduces the number of steps to roughly one third!
