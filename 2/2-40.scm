; Exercise 2.40
; =============
;
; Define a procedure ``unique-pairs`` that, given an integer n, generates the
; sequence of pairs (i,j) with 1 <= j < i <= n. Use ``uniqe-pairs`` to simplify
; the definition of ``prime-sum-pairs`` given above.


; Two versions of ``enumerate-interval``. First, linear recursion.  Bad idea,
; not only because the recursive call is not in tail position (linear space for
; the recursion alone), but also because ``append`` makes this run in quadratic
; time.
;
; (define (enumerate-interval n)
;   (if (= n 0)
;       (list '())
;       (append (enumerate-interval (- n 1)) (list n))))

; Second, iterative recursion: ``iter`` is in tail position and we use ``cons`` instead of append, giving us constant space and linear time.

(define (enumerate-interval start end)
  (define (iter seq end)
    (if (< end start)
        seq
        (iter (cons end seq) (- end 1))))
  (iter '() end))

; On to ``accumulate`` and ``flatmap``::

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (accumulate f init (cdr seq))
         (car seq))))

(define (flatmap f seq) (accumulate append '() (map f seq)))

; And the actual solution::

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; We need ``prime?``, two helpers ``any`` and ``divides``.

(define (any seq)
  (if (null? seq)
      #f
      (or (car seq) (any (cdr seq)))))

; ``(accumulate or #f seq))`` would be more elegant, but ``or`` can't be used
; as an expression.

(define (divides n d) (= 0 (remainder n d)))

(define (prime? n)
  (let ((candidates
          (cons 2 (filter odd? (enumerate-interval 3 (floor (/ n 2)))))))
        (not (any (map (lambda (c) (divides n c)) candidates)))))


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? triple) (prime? (cadr (cdr triple))))

(define (prime-sum-pairs n)
  (filter prime-sum? (map make-pair-sum (unique-pairs n))))
