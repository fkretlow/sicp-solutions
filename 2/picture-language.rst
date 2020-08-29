========================
2.2.1 A Picture Language
========================

Exercise 2.44
-------------
Define the procedure ``up-split`` used by ``corner-split``. It is similar to ``right-split``, except that it switches the roles of ``below`` and ``beside``.

Solution
........

::

    (define (up-split painter n)
      (if (= n 0)
          painter
          (let ((smaller (up-split painter (- n 1))))
            (below painter
                   (beside smaller smaller)))))

Exercise 2.45
-------------
``right-split`` and ``up-split`` can be expressed as instances of a general splitting operation. Define a procedure ``split`` with the property that evaluating ::

    (define right-split (split beside below))
    (define up-split (split below beside))

produces procedures ``right-split`` and ``up-split`` with the same behaviors as the ones already defined.

Solution
........

::

    (define (split top-level low-level)
      (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split top-level low-level) painter (- n 1))))
              (high-level painter
                          (low-level smaller smaller))))))

Exercise 2.46
-------------
A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor ``make-vect`` and corresponding selectors ``xcor-vect`` and ``ycor-vect``. In terms of your selectors and constructor, implement procedures ``add-vect``, ``sub-vect`` and ``scale-vect`` that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar. [...]

Solution
........

::
    
    (define (make-vect xcor ycor) (cons xcor ycor))
    (define (xcor-vect v) (car v))
    (define (ycor-vect v) (cdr v))

    (define (add-vect v w)
      (make-vect (+ (xcor-vect v) (xcor-vect w))
                 (+ (ycor-vect v) (ycor-vect w))))

    (define (sub-vect v w)
      (make-vect (- (xcor-vect v) (xcor-vect w))
                 (- (ycor-vect v) (ycor-vect w))))

    (define (scale-vect v s)
      (make-vect (* (xcor-vect v) s)
                 (* (ycor-vect v) s)))

Exercise 2.47
=============

Here are two possible constructors for frames. For each constructor supply the appropriate selectors to produce an implementation for frames.

Solution
........

::

    (define (make-frame origin edge1 edge2)
      (list origin edge1 edg2))
    (define (origin-frame f) (car f))
    (define (edge1-frame f) (cadr f))
    (define (edge2-frame f) (last f))

    (define (make-frame origin edge1 edge2)
      (cons origin (cons edge1 edge2)))
    (define (origin-frame f) (car f))
    (define (edge1-frame f) (cadr f))
    (define (edge2-frame f) (cdr (cdr f)))
