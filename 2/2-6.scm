; Exercise 2.6
; ============
;
; In case representing pairs as procedures wasn't mind-boggling enough,
; consider that, in a language that can manipulate procedures, we can get by
; without numbers (at least insofar as nonnegative integers are concerned) by
; implementing 0 and the operation of adding 1 as::

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; This representation is known as *Church numerals*, after its inventor, Alonzo
; Church, the logician who invented the lambda calculus.
;
; Define ``one`` and ``two`` directly (not in terms of ``zero`` and ``add-1``).
; (Hint: Use substitution to evaluate ``(add-1 zero)``). Give a direct
; definition of the addition procedure ``+`` (not in terms of repeated
; application of ``add-1``).


; Solution
; --------
;
; Church numerals are less frightening than they look in Scheme syntax. The
; basic idea is that numbers are represented as chains of a single function f
; of one argument x. Zero is the action of not applying f to x at all, one is
; the action of applying f to x once, two is the action of applying f to x
; twice, yielding the chain f(f(x)) or f•f, and so on. It doesn't matter what
; x is or what f does, but if we choose x = 0 and f(x) = incr(x) = x + 1,
; zero(f)(x) = 0, one(f)(x) = 1, two(f)(x) = 2 and so on.

; What boggles the mind with the given Scheme implementation is the fact that
; there is one more level of abstraction than one would intuitively expect.
; ``one`` is a function that takes as argument our function f and returns yet
; another function that applies f to x. We can give a direct definition of
; ``one`` and ``two`` as::

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; It gets even more puzzling when we come to arithmetic operations on church
; numerals. It helps to remember what 'types' of arguments the procedure
; ``add`` must take and what it must return: The church numeral n is a
; procedure of one argument (a procedure f) and it returns another procedure of
; one argument (the base value x). The returned procedure, if applied to any x,
; applies the n-fold chain of f to x.

; Adding the church numerals m(f) = f•f•...•f (m times) and n(f) = f•f•...•f
; (n times) must yield r(f) = m(f)•n(f) = f•f•...•f (r = m*n times).

(define (add m n) (lambda (f) (lambda (x) ((m f)((n f) x)))))
