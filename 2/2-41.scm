; Exercise 2.41
; =============
;
; Write a procedure to find all ordered triples of distinct positive integers
; i, j and k less than or equal to a given integer n that sum to a given
; integer s.

(define (enumerate-interval start end)
  (define (iter seq end)
    (if (< end start)
        seq
        (iter (cons end seq) (- end 1))))
  (iter '() end))

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (accumulate f init (cdr seq))
         (car seq))))

(define (flatmap f seq) (accumulate append '() (map f seq)))

; We generate unique triples by appending every number i in the range [1,n] to all
; unique pairs in the range [1,i-1].

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair) (append pair (list i)))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (sum l) (accumulate + 0 l))

(define (sum-triples n s)
  (filter (lambda (triple) (= (sum triple) s))
          (unique-triples n)))
