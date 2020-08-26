=====================================
2.2.1 A Picture Language -- Exercises
=====================================

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
            (let ((smaller ((split top-level low-level) painter)))
              (high-level painter
                          (low-level smaller smaller))))))
