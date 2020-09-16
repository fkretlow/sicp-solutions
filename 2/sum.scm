; Take a sum as a list of expressions and merge all plain numbers in the list
; into a single running total.
; Return the simplified sum in the form '(+ expressions...  total).
(define (simplify-sum s)
  (define (iter number expressions rest)
    (cond ((null? rest)
           (cond ((and (= number 0) (null? expressions)) 0)
                 ((= number 0)
                  (if (null? (cdr expressions))
                      (car expressions)
                      (cons '+ expressions)))
                 ((null? expressions) number)
                 (else (cons '+ (append expressions (list number))))))
          ((number? (car rest)) (iter (+ number (car rest)) expressions (cdr rest)))
          (else (iter number (append expressions (list (car rest))) (cdr rest)))))
  (if (and (symbol? (car s)) (eq? (car s) '+))
      (iter 0 '() (cdr s))
      (iter 0 '() s)))

(define (make-sum . xs) (simplify-sum xs))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (simplify-sum (cddr s)))
