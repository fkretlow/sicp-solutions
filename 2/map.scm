; MAP
; Built in terms of a binary search tree.
; - (make-map)                 -> returns a new empty map
; - (map-set map key value)    -> sets the value associated with key and returns the modified map
; - (map-get map key default)  -> returns the value associated with key or default if key doesn't exist
; - (map-delete map key)       -> removes the key-value pair associated with key and returns the modified map
; - (map-has? map key)         -> returns #t if key exists in the map
; - (map-flatten map)          -> returns a sorted list of all key-value pairs in the map

(define (_bst-less? a b)
  (cond ((number? a) (< a b))
        ((char? a) (char<? a b))
        ((string? a) (string<? a b))
        ((symbol? a) (symbol<? a b))))

(define (_bst-equal? a b) (equal? a b))

(define (make-map) '())
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

(define (map-set node key value)
  (cond ((null? node)
         (_bst-make-node key value '() '()))
        ((_bst-equal? key (_bst-key node))
         (_bst-make-node key value (_bst-left-child node) (_bst-right-child node)))
        ((_bst-less? key (_bst-key node))
         (_bst-make-node (_bst-key node)
                         (_bst-value node)
                         (map-set (_bst-left-child node) key value)
                         (_bst-right-child node)))
        (else
          (_bst-make-node (_bst-key node)
                          (_bst-value node)
                          (_bst-left-child node)
                          (map-set (_bst-right-child node) key value)))))

(define (map-get node key default)
  (cond ((null? node) default)
        ((_bst-equal? key (_bst-key node)) (_bst-value node))
        ((_bst-less? key (_bst-key node)) (map-get (_bst-left-child node) key default))
        (else (map-get (_bst-right-child node) key default))))

(define (map-has? node key)
  (cond ((null? node) #f)
        ((_bst-equal? key (_bst-key node)) #t)
        ((_bst-less? key (_bst-key node)) (map-get (_bst-left-child node) key))
        (else (map-get (_bst-right-child node) key))))

(define (map-delete node key)
  (cond ((null? node) node)
        ((_bst-equal? key (_bst-key node))
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
                                   (map-delete (_bst-right-child node)
                                               (_bst-key swap-node)))))))
        ((_bst-less? key (_bst-key node))
         (_bst-make-node (_bst-key node)
                         (_bst-value node)
                         (map-delete (_bst-left-child node) key)
                         (_bst-right-child node)))
        (else
          (_bst-make-node (_bst-key node)
                          (_bst-value node)
                          (_bst-left-child node)
                          (map-delete (_bst-right-child node) key)))))

(define (map-flatten node)
  (if (null? node)
      '()
      (append (map-flatten (_bst-left-child node))
              (cons (_bst-data node)
                    (map-flatten (_bst-right-child node))))))
