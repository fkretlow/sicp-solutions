; Exercise 1.38
; =============
;
; In 1737, the Swiss mathematician Leonard Euler published a memoir *De
; Fractionibus Continuis*, which included a continued fraction expansion for
; *e* - 2, where *e* is the base of the natural logarithms. In this fraction,
; the *Ni* are all 1, and the *Di* are successively 1, 2, 1, 1, 4, 1, 1, 6, 1,
; 1, 8, …. Write a program that uses your ``cont-frac`` procedure from Exercise
; 1.37 to approximate *e*, based on Eulers's expansion.


; We use the iterative version of ``cont-frac``:
(define (cont-frac n d k)
  (define (iter r i)
    (if (= i 0)
        r
        (iter (/ (n i) (+ (d i) r)) (- i 1))))
  (iter 0 k))

; D(i) = 2 * (i+1) / 3   if i % 3 = 2
(define (e k)
  (define (n i) 1.0)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* 2.0 (/ (+ i 1) 3))
        1.0))
  (+ (cont-frac n d k) 2))
