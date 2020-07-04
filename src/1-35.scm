; Exercise 1.35
; =============

; Show that the golden ratio φ (Section 1.2.2) is a fixed point of the
; transformation ``x -> 1 + 1/x``, and use this fact to compute φ by means of the
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


; Let a, b, c and d be any four consecutive fibonacci numbers. Then ``c = a + b`` and ``c / b = (a + b) / b``.
; Also, ``d = b + c = b + a + b`` and ``d / c = (b + a + b) / (a + b) = 1 + b / (a + b) = 1 + (c / b)^-1``.
; Hence every ratio between two consecutive fibonacci numbers is equal to one plus the inverse of the previous ratio.
; Thus the golden ratio φ is a fixed point of the transformation ``x -> 1 + 1/x``.

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
