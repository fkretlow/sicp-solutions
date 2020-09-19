(define (bst-make-node key value left right) (list (cons key value) left right))
(define (bst-left-child node) (cadr node))
(define (bst-right-child node) (caddr node))
(define (bst-data node) (car node))
(define (bst-key node) (car (car node)))
(define (bst-value node) (cdr (car node)))

(define (bst-less? a b) (< a b))
(define (bst-equal? a b) (equal? a b))
(define bst-null-value 0)

(define (bst-has-right-child? node) (not (null? (bst-right-child node))))
(define (bst-has-left-child? node) (not (null? (bst-left-child node))))

(define (bst-smallest-child node)
  (if (bst-has-left-child? node)
      (bst-smallest-child (bst-left-child node))
      node))

(define (bst-greatest-child node)
  (if (bst-has-right-child? node)
      (bst-greatest-child (bst-right-child node))
      node))

(define (bst-set node key value)
  (cond ((null? node)
         (bst-make-node key value '() '()))
        ((bst-equal? key (bst-key node))
         (bst-make-node key value (bst-left-child node) (bst-right-child node)))
        ((bst-less? key (bst-key node))
         (bst-make-node (bst-key node)
                        (bst-value node)
                        (bst-set (bst-left-child node) key value)
                        (bst-right-child node)))
        (else
          (bst-make-node (bst-key node)
                         (bst-value node)
                         (bst-left-child node)
                         (bst-set (bst-right-child node) key value)))))

(define (bst-get node key)
  (cond ((null? node) bst-null-value)
        ((bst-equal? key (bst-key node)) (bst-value node))
        ((bst-less? key (bst-key node)) (bst-get (bst-left-child node) key))
        (else (bst-get (bst-right-child node) key))))

(define (bst-delete node key)
  (cond ((null? node) node)
        ((bst-equal? key (bst-key node))
         (cond ((and (bst-has-left-child? node)
                     (not (bst-has-right-child? node)))
                (bst-left-child node))
               ((and (not (bst-has-left-child? node))
                     (bst-has-right-child? node))
                (bst-right-child node))
               ((and (not (bst-has-left-child? node))
                     (not (bst-has-right-child? node)))
                '())
               (else
                 (let ((swap-node (bst-smallest-child (bst-right-child node))))
                   (bst-make-node (bst-key swap-node)
                                  (bst-value swap-node)
                                  (bst-left-child node)
                                  (bst-delete (bst-right-child node)
                                              (bst-key swap-node)))))))
        ((bst-less? key (bst-key node))
         (bst-make-node (bst-key node)
                        (bst-value node)
                        (bst-delete (bst-left-child node) key)
                        (bst-right-child node)))
        (else
          (bst-make-node (bst-key node)
                         (bst-value node)
                         (bst-left-child node)
                         (bst-delete (bst-right-child node) key)))))

(define (bst-flatten node)
  (if (null? node)
      '()
      (append (bst-flatten (bst-left-child node))
              (cons (bst-data node)
                    (bst-flatten (bst-right-child node))))))
