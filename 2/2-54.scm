; # Exercise 2.54
;
; Two lists are said to be `equal?` if they contain equal elements arranged in
; the same order. For example
;
; ```scheme
; (equal? '(this is a list) '(this is a list))
; ```
;
; is true, ubt
;
; ```scheme
; (equal? '(this is a list) '(this (is a) list))
; ```
;
; is false. To be more precise, we can define `equal?` recursively in terms of
; the basic `eq?` equality of symbols by saying that a and b ar `equal?` if
; they are both symbols and the symbols are `eq?`, or if they are both lists
; such that `(car a)` is `equal?` to `(car b)` and `(cdr a)` is `equal?` to
; `(cdr b)`. Using this idea, implement `equal?` as a procedure.

(define (equal? a b)
  ; We could combine the first two banches, because the built-in `eq?`
  ; works on numbers.
  (cond ((and (symbol? a) (symbol? b)) (eq? a b))
        ((and (number? a) (number? b)) (= a b))
        ((and (list? a) (list? b))
         (cond ((null? a) (null? b))
               ((equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
               (else #f)))
        (else #f)))
