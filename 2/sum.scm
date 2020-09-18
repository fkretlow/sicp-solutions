; Take a sum as a list of expressions and merge all plain numbers in the list
; into a single running total.
; Return the simplified sum in the form '(+ expressions...  total).
(define (simplify-sum s)
  (define (iter number combinations rest)
    (cond ((null? rest)
           (cond ((and (= number 0) (null? combinations)) 0)
                 ((= number 0)
                  (if (= (length combinations) 1)
                      (car combinations)
                      (cons '+ combinations)))
                 ((null? combinations) number)
                 (else (cons '+ (append combinations (list number))))))
          ((number? (car rest)) (iter (+ number (car rest)) combinations (cdr rest)))
          ((sum? (car rest))
           (let ((result (simplify-sum (car rest))))
             (cond ((number? result) (iter (+ number result) combinations (cdr rest)))
                   (else (iter number (append combinations (list result)) (cdr rest))))))
          (else (iter number (append combinations (list (car rest))) (cdr rest)))))
  (if (and (symbol? (car s)) (eq? (car s) '+))
      (iter 0 '() (cdr s))
      (iter 0 '() s)))

(define (make-sum . xs) (simplify-sum xs))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (simplify-sum (cddr s)))
