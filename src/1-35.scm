; Exercise 1.35
; =============
;
; Show that the golden ratio phi (Section 1.2.2) is a fixed point of the
; transformation x -> 1 + 1/x, and use this fact to compute phi by means of the
; ``fixed-point`` procedure.


(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; Let a, b, and c be any three consecutive Fibonacci numbers.
; Then the ratio between a and b is r_n = b / a.
; The next ratio is r_n+1 = c / b = (a+b) / b = 1 + a/b = 1 + 1 / r_n.
; Thus phi is a fixed point of the transformation x -> 1 + 1/x.

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
