#lang racket

(require "utils.rkt")
(require "input-wrapper.rkt")

(define (add-dense dp1 dp2)
  (let* 
    (
      [len1 (length dp1)]
      [len2 (length dp2)]
      [max-len (max len1 len2)]
      [padded-dp1 (append dp1 (make-list (- max-len len1) 0))]
      [padded-dp2 (append dp2 (make-list (- max-len len2) 0))]
    )
    (map + padded-dp1 padded-dp2)
  )
)

(define (add p1 p2)
 (double-input-wrapper add-dense p1 p2)
)

(display (add `(4 8 12) `(2 4) ))
(newline)
(display (add `(4 8 12) `((2 0) (4 1) (6 2)) ))
(newline)
(display (add `((4 0) (8 1) (12 2)) `(2 4 6) ))
(newline)
(display (add `((4 0) (8 1) (12 2)) `((2 0) (4 1) (6 2)) ))