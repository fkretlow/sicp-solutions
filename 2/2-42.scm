; Exercise 2.42
; =============
;
; The "eight-queens puzzle" asks how to place eight queens on a chessboard so
; that no queen is in check from any other (i.e. no two queens are in the same
; row, column, or diagonal). [...] One way to solve the puzzle is to work
; across the board, placing a queen in each column. Once we have placed k-1
; queens, we must place the kth queen in a position where it does not check any
; of the queens already on the board. We can formulate this approach
; recursively: Assume that we have already generated the sequence of all
; possible ways to place k-1 queens in the first k-1 columns of the board. For
; each of these ways, generate an extended set of positions by placing a queen
; in each row of the kth column. Now filter these, keeping only the positions
; for which the queen in the kth column is safe with respect to the other
; queens. This produces the sequence of all ways to place k queens in the first
; k columns. By continuing this process, we will produce not only one solution,
; but all solutions to the puzzle.
;
; [...]

; Basic sequence operations:

(define (enumerate-interval start end)
  (define (iter seq end)
    (if (< end start)
        seq
        (iter (cons end seq) (- end 1))))
  (iter '() end))

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (accumulate f init (cdr seq))
         (car seq))))

(define (flatmap f seq) (accumulate append '() (map f seq)))

; Data representations:

(define (make-position col row) (cons col row))
(define (get-col pos) (car pos))
(define (get-row pos) (cdr pos))

(define set-of-positions list)
(define (get-position set index)
  (if (= index 1)
      (car set)
      (get-position (cdr set) (- index 1))))

(define (add-position set pos) (append set (list pos)))

; Checking positions: Do p and q check each other?
(define (check? p q)
  (define (same-row? a b) (= (get-row a) (get-row b)))
  (define (same-col? a b) (= (get-col a) (get-col b)))
  (define (same-diag? a b) (or (= (+ (get-row a) (get-col a))
                                  (+ (get-row b) (get-col b)))
                               (= (- (get-row a) (get-col a))
                                  (- (get-row b) (get-col b)))))
  (or (same-row? p q) (same-col? p q) (same-diag? p q)))

; Is the kth position in the set in check with any preceding position?
(define (safe? set k)
  (cond ((= k 1) #t)
        ((check? (get-position set 1)
                 (get-position set k))
         #f)
        (else (safe? (cdr set) (- k 1)))))

(define empty-board '())

(define (queens board-size)
  (define (queen-cols col)
    (if (= col 0)
        (list empty-board)
        (filter
          (lambda (solution) (safe? solution col))
          (flatmap
            (lambda (prev-solution)
              (map (lambda (row)
                     (add-position prev-solution (make-position col row)))
                   (enumerate-interval 1 board-size)))
            ; display number of solutions for col - 1 columns:
            (let ((prev-solutions (queen-cols (- col 1))))
              (begin (display (- col 1)) (display ": ")
                     (display (length prev-solutions))
                     (newline)
                     prev-solutions))))))
            ; or just compute the end result
            ; (queen-cols (- col 1))))))
  (queen-cols board-size))
