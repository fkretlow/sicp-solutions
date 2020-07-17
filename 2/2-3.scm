; Exercise 2.3
; ============
;
; Implement a representation for rectangles in a plane. (Hint: You may want to
; make use of Exercise 2.2.) In terms of your constructors and selectors,
; create procedures that compute the perimeter and the area of a given
; rectangle.

; First, let's represent rectangles as a pair of width and height:

(define (make-rect w h) (cons w h))
(define (width-rect r) (car r))
(define (height-rect r) (cdr r))

(define (perimeter-rect r)
  (* 2 (+ (width-rect r)
          (height-rect r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))

; Now implement a different representation for rectangles. Can you design your
; system with suitable abstraction barriers, so that the same perimeter and
; area procedures will work using either representation?

; Now make use of Exercise 2.2 and represent rectangles as a pair of line
; segments.

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (length-segment s)
  (let ((dx (abs (- (x-point (end-segment s))
                    (x-point (start-segment s)))))
        (dy (abs (- (y-point (end-segment s))
                    (y-point (start-segment s))))))
    (sqrt (+ (square dx) (square dy)))))


(define (make-rect2 a b) (cons a b))
(define (width-rect2 r) (length-segment (car r)))
(define (height-rect2 r) (length-segment (cdr r)))

; The same perimeter and area procedures:

(define (perimeter-rect2 r)
  (* 2 (+ (width-rect2 r)
          (height-rect2 r))))

(define (area-rect2 r)
  (* (width-rect2 r) (height-rect2 r)))
