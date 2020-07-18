; Exercise 2.5
; ============
;
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if represent the pair a and b as the integers that
; is the product 2^a * 3^b. Give the corresponding definitions of the procedurs
; ``cons``, ``car``, and ``crd``. (Again renamed to ``pair``, ``first``,
; ``second`` to avoid name clashes.)

; 2^a is even and 3^b is odd for all nonnegative integers a and b. Count how
; many times we can divide by 2 (3) to get a (b).

(define (pair a b) (* (expt 2 a) (expt 3 b)))

(define (first p)
  (define (iter c r)
    (if (= (remainder r 2) 0)
        (iter (+ c 1) (/ r 2))
        c))
  (iter 0 p))

(define (second p)
  (define (iter c r)
    (if (= (remainder r 3) 0)
        (iter (+ c 1) (/ r 3))
        c))
  (iter 0 p))
