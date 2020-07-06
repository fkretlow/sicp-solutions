; Exercise 1.39
; =============

; A continued fraction representation of the tangent function was published in
; 1770 by the German mathematician J.H. Lambert:

    ; tan x = x / (1 - (x^2 / 3 - (x^2 / 5 - ...)),

; where *x* is in radians. Define a procedure ``(tan-cf x k)`` that computes an
; approximation to the tangent function based on Lambert's formula. *k*
; specifies the number of terms to compute, as in Exercise 1.37.


(define (tan-cf x k)
  (define (n i) (if (= i 1) x (* x x)))
  (define (d i) (- (* 2 i) 1.0))
  (define (iter r i)
    (if (= i 0)
        r
        (iter (/ (n i) (- (d i) r)) (- i 1))))
  (iter 0 k))
