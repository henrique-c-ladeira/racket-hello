#lang racket

(require "utils.rkt")
(require "input-wrapper.rkt")

(define (subtract-dense dp1 dp2)
  (if (> (length dp1) (length dp2))
    
    (let ([dp2-padded (append dp2 (make-list (- (length dp1) (length dp2)) 0))])
      (map - dp1 dp2-padded)
    )

    (let ([dp1-padded (append dp1 (make-list (- (length dp2) (length dp1)) 0))])
      (map - dp1-padded dp2)
    )
  )
)

(define (subtract p1 p2)
 (double-input-wrapper subtract-dense p1 p2)
)

(display (subtract `(4 8 12) `(2 4) ))
(newline)
(display (subtract `(4 8 12) `((2 0) (4 1) (6 2)) ))
(newline)
(display (subtract `((4 0) (8 1) (12 2)) `(2 4 6) ))
(newline)
(display (subtract `((4 0) (8 1) (12 2)) `((2 0) (4 1) (6 2)) ))