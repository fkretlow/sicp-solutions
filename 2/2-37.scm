; Exercise 2.37
; =============
;
; Suppose we represent vectors v = (v_i) as sequences of numbers, and matrices
; m = (m_ij) as sequences of vectors (the rows of the matrix). For example, the
; matrix ::
;
;     1 2 3 4
;     4 5 6 6
;     6 7 8 9
;
; is represented as the sequence ::
;
;     ((1 2 3 4) (4 5 6 6) (6 7 8 9))
;
; With this representation, we can use sequence operation to concisely express
; the basic matrix and vector operations. These operations (which are described
; in any book on matrix algebra) are the following:
;
; ``(dot-product v w)``
;     returns the sum Sigma_i v_i w_i
;
; ``(matrix-*-vector m v)``
;     returns the vector t, where t_i = Sigma_j m_ij v_j
;
; ``(matrix-*-matrix m n)``
;     returns the matrix p, where p_ij = Sigma_k m_ik n_kj
;
; ``(transpose m)``
;     returns the matrix n, where n_ij = m_ji
;
; We can define the dot product as ::

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing the
; other matrix operations. (The procedure ``accumulate-n`` is defined in
; Exercise 2.36.)

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r)
           (map (lambda (c)
                  (dot-product r c))
                cols))
         m)))
