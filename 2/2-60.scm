; Exercise 2.60
; =============
;
; We specified that a set would be represented as a list with no duplicates.
; Now suppose we allow duplicates. For instance, the set {1, 2, 3} could be
; represented as the list (2 3 2 1 3 2 2). Design procedures ``element-of-set?``
; , ``adjoin-set``, ``union-set``, and ``intersection-set`` that operate on
; this representation.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define adjoin-set cons)

(define union-set append)

(define (intersection-set set1 set2)
  (cons ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2)))

; How does the efficiency of each compare with the corresponding procedure for
; the non-duplicate representation?

; ``element-of-set?``: On the surface similar to the non-duplicate version:
; O(n). However, n is potentially much greater, so in the worst case where the
; element is not in the set, the operation will be considerably slower.
;
; ``adjoin-set``: O(1) vs. O(n), and O(1) means literally a single operation.
;
; ``union-set``: Non-duplicate: membership test for each element of the first
; set, O(mn). Duplicate: a single ``append`` in linear time O(m).
;
; ``intersection-set``: Similar to the non-duplicate version: O(mn), but with
; potentially much longer lists and much redundant checking.

; Are there applications for which you would use this representation in
; preference to the non-duplicate one?

; While applications that require a lot of insertions will benefit from the
; substantially faster ``adjoin-set`` operation, this comes at considerable
; cost for any operation that requires membership testing, and such operations
; are very much the reason why you would use a set in the first place. Also, a
; lot of insertions means probably a lot of space wasted for duplicates. This
; is not a representation that makes any sense to use.
