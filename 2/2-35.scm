; Exercise 2.35
; =============

; Redefine ``count-leaves`` from Section 2.2.2 as an accumulation::


(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (count-leaves tree)
  (accumulate (lambda (t a)
                (+ (if (pair? t) (count-leaves t) 1)
                   a))
              0
              tree))
