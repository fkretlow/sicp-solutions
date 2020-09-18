; Exercise 2.33
; =============
;
; Fill in the missing expressions to complete the following definitions of some
; basic list-manipulation operations in terms of ``accumulate``::

; This (given) implementaion of ``accumulate`` is a *right* fold. The left fold
; would be slightly more obvious, but it wouldn't allow to replicate lists with
; ``(accumulate cons nil <list>)``::

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length2 sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
