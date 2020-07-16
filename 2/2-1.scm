; Exercise 2.1
; ============
;
; Define a better version of ``make-rat`` that handles both positive and
; negative arguments. ``make-rat`` should normalize the sign so that if the
; rational number is positive, both the numerator and the denominator ar
; positive, and if the rational number is negative, only the numerator is
; negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (* -1 n) g) (/ (* -1 d) g))
        (cons (/ n g) (/ d g)))))
