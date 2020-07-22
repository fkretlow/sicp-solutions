; 2.1.4 Extended Exercise: Interval Arithmetic
; ============================================
;
; [...]
; Alyssa's idea is to implement "interval arithmetic" as a set of arithmetic
; operations for combining "intervals" (objects that represent the range of
; possible values of an inexact quantity). The result of adding, subtracting,
; multiplying, or dividing two intervals is itself an interval, representing
; the range of the result.
;
; Alyssa postulates the existence of an abstract object called an "interval"
; that has two endpoints: a lower bound and an upper bound. She also presumes
; that, given the endpoints of the interval, she can construct the interval
; using the data constructor ``make-interval``. Alyssa first writes a procedure
; for adding two intervals. She reasons that the minimum value the sum could be
; is the sum of the two lower bounds and the maximum value it could be is the
; sum of the two upper bounds::

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Alyssa also works out the product of two intervals by finding the minimum and
; the maximum of the products of the bound and using them as the bounds of the
; resulting interval. (``min`` and ``max`` are primitives that find the minimum
; or maximum of any number of arguments.)::

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; To divide two intervals, Alyssa multiplies the first by the reciprocal of the
; second. Note that the bounds of the reciprocal interval are the reciprocal of
; the upper bound and the reciprocal of the lower bound in that order.::

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))


; Exercise 2.7
; ------------
;
; Alyssa's program is incomplete because she has not specified the
; implementation of the interval abstraction. Here is a definition of the
; interval constructor::

(define (make-interval a b) (cons a b))

; Define selectors ``upper-bound`` and ``lower-bound`` to complete the
; implementation.

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))


; Exercise 2.8
; ------------
;
; Using reasoning analogous to Alyssa's, describe how the difference of two
; intervals may be computed. Define a corresponding subtraction procedure,
; called ``sub-interval``.

; The minimum value the difference can be is the difference of the lower bound
; of the first interval and the upper bound of the second interval. The maximum
; value it can be is the difference of the upper bound of the first interval
; and the lower bound of the second interval.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


; Exercise 2.9
; ------------
;
; The *width* of an interval is half of the difference between its upper and
; lower bounds. The width is a measure of the uncertainty of the number
; specified by the interval. For some arithmetic operations the width of the
; result of combining two intervals is a function only of the widths of the
; argument intervals, whereas for others the width of the combination is not a
; function of the widths of the argument intervals. Show that the width of the
; sum (or difference) of two intervals is a function only of the widths of the
; intervals being added (or subtracted). Give examples to show that this is not
; true for multiplication or division.

; Let a, b and c be intervals such that a + b = c. Then ::
;
;     width(a) = 1/2 * (upper(a) - lower(a))
;     width(b) = 1/2 * (upper(b) - lower(b))
;
;     width(c) = 1/2 * (upper(c) - lower(c))
;              = 1/2 * (upper(a) + upper(b) - (lower(a) + lower(b)))
;              = 1/2 * ((upper(a) - lower(a)) + (upper(b) - lower(b)))
;              = width(a) + width(b)
;
; The same goes for subtraction.
;
; As for multiplication, the width of the resulting interval depends on the
; signs of the bounds of the input intervals as well. Consider three intervals
; a * b = c where a is a symmetric interval around 0, e.g. [-1,1] and b is
; entirely positive (or entirely negative). In this case width(b) doesn't
; affect width(c), only the bound of b with the greater absolute value does. ::
;
;     [-1,1] * [0,2] = [-1,1] * [1,2] = [-2,2]


; Exercise 2.10
; -------------
;
; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and
; comments that it is not clear what it means to divide by an interval that
; spans zero. Modify Alyssa's code to check for this condition and to signal an
; error if it occurs. ::

(define (div-interval x y)
  (if (= 0 (- (upper-bound y) (lower-bound y)))
      (error "division by zero-spanning interval")
      (mul-interval
        x
        (make-interval
          (/ 1.0 (upper-bound y))
          (/ 1.0 (lower-bound y))))))
