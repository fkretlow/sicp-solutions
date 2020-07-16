; Exercise 1.37
; =============
;
; a) An infinite *continued fraction* is an epression of the form
;
;     f = N1 / (D1 + N2 / (D2 + N3 / (D3 + ...)))

; As an example, one can show that the infinite continued fraction expansion
; with the *Ni* and the *Di* all equal to 1 produces 1/phi, where phi is the
; golden ratio (described in Section 1.2.2). One way to approximate an infinite
; continued fraction is to truncate the expansion after a given number of
; terms. Such a truncation -- a so called *k-term finite continued fraction* --
; has the form
;
;     N1 / (D1 + N2 / ... + Nk / Dk)
;
; Suppose that ``n`` and ``d`` are procedures of one argument (the term index
; *i*) that return the *Ni* and *Di* of the terms of the continued fraction.
; Define a procedure ``cont-frac`` such that evaluationg ``(cont-frac n d k)``
; computes the value of the *k*-term finite continued fraction. Check your
; procedure by approximating 1/phi using::
;
;     (cont-frac (lambda (i) 1.0)
;                (lambda (i) 1.0)
;                k)
;
; for successive values of *k*. How large must you make *k* in order to get an
; approximation that is accurate to 4 decimal places?


(define (cont-frac n d k)
  (define (helper i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (helper (+ i 1))))))
  (helper 1))

(define (phi k) (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))

; The value for phi is accurate to 4 decimal places for *k* >= 12.


; b) If your ``cont-frac`` procedure generates a recursive process, write one
; that generates an iterative process. If it generates an iterative process,
; write one that generates a recursive process.

; Bottom-up: Start with the *kth* fraction and gradually build the result on top
; of it.

(define (cont-frac2 n d k)
  (define (iter r i)
    (if (= i 0)
        r
        (iter (/ (n i) (+ (d i) r)) (- i 1))))
  (iter 0 k))
