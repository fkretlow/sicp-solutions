; Exercise 2.39
; =============
;
; Complete the following definitions of ``reverse`` (Exercise 2.18) in terms of
; ``fold-right`` and ``fold-left`` from Exercise 2.38::

(define (fold-right op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (fold-right op init (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
