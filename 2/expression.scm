(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (variable? x) (symbol? x))

(define (make-sum . xs) (simplify-sum xs))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (cddr s))

(define (make-product x y) (list '* x y))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (simplify-sum s)
  (define (add-variable variables v)
    (define (same-variables? p v) (eq? (multiplicand p) v))
    (define (find-variable variables v)
      (if (null? variables)
          #f
          (if (same-variables? (car variables) v)
              #t
              (find-variable (cdr variables) v))))

    (if (not (find-variable variables v))
        (append variables (list (make-product 1 v)))
        (accumulate (lambda (p all)
                      (if (same-variables? p v)
                          (cons (make-product (+ (multiplier p) 1) (multiplicand p)) all)
                          (cons p all)))
                    '()
                    variables)))

  (define (iter number variables combinations rest)
    (begin (display "number: ") (display number)
           (display ", variables: ") (display variables)
           (display ", combinations: ") (display combinations) (newline)
    (if (null? rest)
        (let ((things (filter (lambda (x) (if (number? x)
                                              (not (= 0 x))
                                              (not (null? x))))
                              (append (append variables combinations) (list number)))))
          (cond ((= (length things) 1) (car things))
                (else (cons '+ things))))
        (let ((next (simplify (car rest))))
          (cond ((number? next)
                 (iter (+ number next) variables combinations (cdr rest)))
                ((variable? next)
                 (iter number (add-variable variables next) combinations (cdr rest)))
                (else
                  (iter number variables (append combinations (list next)) (cdr rest))))))))
  (iter 0 '() '() s))


(define (simplify expr)
  (cond ((sum? expr) (simplify-sum expr))
        (else expr)))
