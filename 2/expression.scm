(load "map")

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (variable? x) (symbol? x))

(define (null-term? neutral-element term) (if (number? term) (= term neutral-element) (null? term)))

(define (make-sum . xs) (simplify-sum xs))
(define (sum? x) (and (pair? x) (equal? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (cddr s))

(define (make-product . xs) (simplify-product xs))
(define (product? x) (and (pair? x) (equal? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


(define (simplify-sum s)
  (define (absorb-sub-sums terms)
    (accumulate
      (lambda (next result)
        (if (sum? next)
            (append (cdr next) result)
            (cons next result)))
      '() terms))

  (define (merge-sum-terms number variables combinations rest)
    (if (null? rest)
        (let ((terms (filter (lambda (t) (not (null-term? 0 t)))
                       (append
                         (append (map (lambda (v) (make-product (car v) (cdr v)))
                                      (map-flatten variables))
                                 (reverse combinations))
                         (list number)))))
          (cond ((= (length terms) 1) (car terms))
                ((null? terms) 0)
                (else (cons '+ terms))))

        (let ((next (car rest)))
          (cond ((number? next)
                 (merge-sum-terms (+ number next) variables combinations (cdr rest)))
                ((or (variable? next)
                     (and (product? next)
                          (number? (multiplier next))
                          (variable? (multiplicand next))))
                 (let ((variable (if (variable? next) next (multiplicand next)))
                       (factor (if (variable? next) 1 (multiplier next))))
                   (merge-sum-terms number
                                    (map-set variables
                                             variable
                                             (+ (map-get variables variable 0) factor))
                                    combinations
                                    (cdr rest))))
                (else
                  (merge-sum-terms number variables (cons next combinations) (cdr rest)))))))

  (let ((terms (if (equal? (car s) '+) (cdr s) s)))
    (merge-sum-terms 0 (make-map) '() (absorb-sub-sums (map simplify terms)))))


(define (simplify-product p)
  (define (absorb-sub-products terms)
    (accumulate
      (lambda (next result)
        (if (product? next)
            (append (cdr next) result)
            (cons next result)))
      '() terms))

  (define (merge-product-terms number combinations rest)
    (if (null? rest)
        (if (= number 0)
            0
            (let ((terms (filter (lambda (t) (not (null-term? 1 t)))
                           (cons number (reverse combinations)))))
              (cond ((= (length terms) 1) (car terms))
                    ((null? terms) 0) ; TODO: 0 assumes we're in a sum...
                    (else (cons '* terms)))))

        (let ((next (car rest)))
          (cond ((number? next)
                 (merge-product-terms (* number next) combinations (cdr rest)))
                (else
                  (merge-product-terms number (cons next combinations) (cdr rest)))))))

  (let ((terms (if (equal? (car p) '*) (cdr p) p)))
    (merge-product-terms 1 '() (absorb-sub-products (map simplify terms)))))


(define (simplify expr)
  (cond ((sum? expr) (simplify-sum expr))
        ((product? expr) (simplify-product expr))
        (else expr)))
