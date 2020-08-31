========================
2.2.1 A Picture Language
========================

Exercise 2.44
-------------
Define the procedure ``up-split`` used by ``corner-split``. It is similar to ``right-split``, except that it switches the roles of ``below`` and ``beside``.

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
-------------

Here are two possible constructors for frames. For each constructor supply the appropriate selectors to produce an implementation for frames.

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

Exercise 2.48
-------------

A directed line segment in the plane can be represented as a pair of vectors -- the vector running from the origin to the start-point of the segment, and the vector running from the origin to the end-point of the segment. Use your vector representation from Exercise 2.46 to define a representation for segments with a constructor ``make-segment`` and selectors ``start-segment`` and ``end-segment``.

::

    (define (make-segment x1 y1 x2 y2)
      (cons (make-vector x1 y1) (make-vector x2 y2)))
    (define (start-segment s) (car s))
    (define (end-segment s) (cdr s))

Exercise 2.49
-------------

Use ``segments->painter`` to define the following primitive painters:

a. The painter that draws the outline of the designated frame. ::

    (define outline
      (segments->painter (list (make-segment 0 0 0 1)
                               (make-segment 0 1 1 1)
                               (make-segment 1 1 1 0)
                               (make-segment 1 0 0 0))))

b. The painter that draws an "X" by connecting opposite corners of the frame. ::

    (define X
      (segments->painter (list (make-segment 0 0 1 1)
                               (make-segment 0 1 1 0))))

c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame. ::

    (define diamond
      (segments->painter (list (make-segment 0 0.5 0.5 1)
                               (make-segment 0.5 1 1 0.5)
                               (make-segment 1 0.5 0.5 0)
                               (make-segment 0.5 0 0 0.5))))

d. The wave painter. -- Nah, that's too much work. Also, these are curves, not lines!


Exercise 2.50
-------------

Define the transformation ``flip-horiz``, which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.

::

    (define (fliz-horiz painter)
      (transform-painter painter
                         (make-vect 1.0 0.0)
                         (make-vect 1.0 1.0)
                         (make-vect 0.0 0.0)))

    (define (rotate180)
      (transform-painter painter
                         (make-vect 1.0 1.0)
                         (make-vect 0.0 1.0)
                         (make-vect 1.0 0.0)))

    (define (rotate270)
      (transform-painter painter
                         (make-vect 0.0 1.0)
                         (make-vect 0.0 0.0)
                         (make-vect 1.0 1.0)))


Exercise 2.51
-------------

Define the ``below`` operation for painters. ``below`` takes two painters as arguments. The resulting painter, given a frame, draws with the first painter in the bottom half of the frame and the second painter in the top. Define ``below`` in two different ways -- first by writing a procedure that is analogous to the ``beside`` procedure given above, and again in terms of beside and suitable rotations operations (from Exercise 2.50).

::

    (define (below painter1 painter2)
      (let ((split-point (make-vect 0.0 0.5)))
        (let ((paint-bottom
                (transform-painter painter1
                                   (make-vect 0.0 0.0)
                                   (make-vect 1.0 0.0)
                                   split-point))
              (paint-top
                (transform-painter painter2
                                   (split-point)
                                   (make-vect 1.0 0.5)
                                   (make-vect 0.0 1.0))))
          (lambda (frame)
            (paint-bottom frame)
            (paint-top frame)))))

    (define (below painter1 painter 2)
      (rotate90 (beside
                  (rotate270 painter1)
                  (rotate270 painter2)))) 

