; Exercise 1.33
; =============
;
; You can obtain an even more general version of `accumulate` (Exercise 1.32)
; by introducing the notion of a *filter* on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting `filtered-accumulate` abstraction takes
; the same arguments as `accumulate`, together with an additional predicate of
; one argument that specifies the filter. Write `filtered-accumulate` as a
; procedure.

(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (predicate a) a null-value)
                (filtered-accumulate predicate
                                     combiner
                                     null-value
                                     term
                                     (next a) next b))))

; Iterative version:
(define (filtered-accumulate2 predicate combiner null-value term a next b)
  (define (iter acc a b)
    (cond ((> a b) acc)
          ((predicate a) (iter (combiner acc (term a)) (next a) b))
          (else (iter acc (next a) b))))
  (iter null-value a b))


; Helper functions for convenient testing
(define (add y) (lambda (x) (+ x y)))
(define (incr x) (+ x 1))
(define (id x) x)


; Show how to express the following using `filtered-accumulate`: a) the sum of
; the squares of the prime numbers in the interval a to b (assuming that you
; have a `prime?` predicate already written)

(define (sum-of-squared-primes a b)
  (filtered-accumulate prime? + 0 square a (add 1) b))

; b) the product of all the positive
; integers less than *n* that are relatively prime to *n* (i.e. all positive
; integers *i* < *n* such that GCD(*i*, *n*) = 1).

(define (product-of-relative-primes n)
  (define (relatively-prime? a) (= (gcd a n) 1))
  (filtered-accumulate relatively-prime? * 1 id 1 incr n))
