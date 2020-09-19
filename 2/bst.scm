; BINARY SEARCH TREE
; - (make-bst)               -> returns a new empty tree
; - (bst-set tree key value) -> sets the value associated with key and returns the modified tree
; - (bst-get tree key)       -> returns the value associated with key or bst-null-value if key doesn't exist
; - (bst-delete tree key)    -> removes the key-value pair associated with key and returns the modified tree
; - (bst-has? tree key)      -> returns #t if key exists in the tree
; - (bst-flatten tree)       -> returns a sorted list of all key-value pairs in the tree

; Change these if needed:
; 1. Define an ordering for the keys.
(define (bst-less? a b) (< a b))
(define (bst-equal? a b) (equal? a b))
; 2. Define a null-value to return if a requested key is not found.
(define bst-null-value 0)

(define (make-bst) '())
(define (_bst-make-node key value left right) (list (cons key value) left right))
(define (_bst-left-child node) (cadr node))
(define (_bst-right-child node) (caddr node))
(define (_bst-data node) (car node))
(define (_bst-key node) (car (_bst-data node)))
(define (_bst-value node) (cdr (_bst-data node)))

(define (_bst-has-right-child? node) (not (null? (_bst-right-child node))))
(define (_bst-has-left-child? node) (not (null? (_bst-left-child node))))

(define (_bst-smallest-child node)
  (if (_bst-has-left-child? node)
      (_bst-smallest-child (_bst-left-child node))
      node))

(define (_bst-greatest-child node)
  (if (_bst-has-right-child? node)
      (_bst-greatest-child (_bst-right-child node))
      node))

(define (bst-set node key value)
  (cond ((null? node)
         (_bst-make-node key value '() '()))
        ((bst-equal? key (_bst-key node))
         (_bst-make-node key value (_bst-left-child node) (_bst-right-child node)))
        ((bst-less? key (_bst-key node))
         (_bst-make-node (_bst-key node)
                        (_bst-value node)
                        (bst-set (_bst-left-child node) key value)
                        (_bst-right-child node)))
        (else
          (_bst-make-node (_bst-key node)
                         (_bst-value node)
                         (_bst-left-child node)
                         (bst-set (_bst-right-child node) key value)))))

(define (bst-get node key)
  (cond ((null? node) bst-null-value)
        ((bst-equal? key (_bst-key node)) (_bst-value node))
        ((bst-less? key (_bst-key node)) (bst-get (_bst-left-child node) key))
        (else (bst-get (_bst-right-child node) key))))

(define (bst-has? node key)
  (cond ((null? node) #f)
        ((bst-equal? key (_bst-key node)) #t)
        ((bst-less? key (_bst-key node)) (bst-get (_bst-left-child node) key))
        (else (bst-get (_bst-right-child node) key))))

(define (bst-delete node key)
  (cond ((null? node) node)
        ((bst-equal? key (_bst-key node))
         (cond ((and (_bst-has-left-child? node)
                     (not (_bst-has-right-child? node)))
                (_bst-left-child node))
               ((and (not (_bst-has-left-child? node))
                     (_bst-has-right-child? node))
                (_bst-right-child node))
               ((and (not (_bst-has-left-child? node))
                     (not (_bst-has-right-child? node)))
                '())
               (else
                 (let ((swap-node (_bst-smallest-child (_bst-right-child node))))
                   (_bst-make-node (_bst-key swap-node)
                                  (_bst-value swap-node)
                                  (_bst-left-child node)
                                  (bst-delete (_bst-right-child node)
                                              (_bst-key swap-node)))))))
        ((bst-less? key (_bst-key node))
         (_bst-make-node (_bst-key node)
                        (_bst-value node)
                        (bst-delete (_bst-left-child node) key)
                        (_bst-right-child node)))
        (else
          (_bst-make-node (_bst-key node)
                         (_bst-value node)
                         (_bst-left-child node)
                         (bst-delete (_bst-right-child node) key)))))

(define (bst-flatten node)
  (if (null? node)
      '()
      (append (bst-flatten (_bst-left-child node))
              (cons (_bst-data node)
                    (bst-flatten (_bst-right-child node))))))
