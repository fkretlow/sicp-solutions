; Exercise 1.40
; =============
;
; Define a procedure ``cubic`` that can be used together with the
; ``newtons-method`` procedure in expressions of the form ``(newtons-method
; (cubic a b c) 1)``.


; Required procedures from the text:
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))


(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newtons-method g guess)
  (define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))


; And the incredibly difficult solution:
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
