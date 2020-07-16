; Exercise 1.31
; =============

; a) The `sum` procedure is only the simplest of a vast number of similar abstractions
; that can be captured as higher order procedures. Write an analogous procedure called
; `product` that returns the product of the values of a function at points over a given
; range. Show how to define `factorial` in terms of `product`. Also use `product` to
; compute approximations to Pi using the formula Pi / 4 = 2*4/3*3 * 4*6/5*5 * 4*6/7*7...`.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (id x) x)
(define (add y) (lambda (x) (+ x y)))

(define (factorial b)
  (product id 1 (add 1) b))

(define (pi b)
  (define (pi-term x) (/ (* x (+ x 2))
                         (* (+ x 1) (+ x 1))))
  (* 4 (product pi-term 2.0 (add 2) b)))


; b) If your `product` procedure generates a recursive process, write one that generates
; an iterative process. If it generates an iterative process, write one that generates a
; recursive process.

(define (product2 term a next b)
  (define (iter acc a b)
    (if (> a b)
        acc
        (iter (* acc (term a)) (next a) b)))
  (iter 1 a b))
