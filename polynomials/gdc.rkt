#lang racket

(require "utils.rkt")
(require "input-wrapper.rkt")

(define (gdc-dense a b)
  `(1)
)

(define (gdc a b)
  (double-input-wrapper gdc-dense a b)
)

(display (gdc `(4 8 12) `(2 4 6) ))
(newline)
(display (gdc `(4 8 12) `((2 0) (4 1) (6 2)) ))
(newline)
(display (gdc `((4 0) (8 1) (12 2)) `(2 4 6) ))
(newline)
(display (gdc `((4 0) (8 1) (12 2)) `((2 0) (4 1) (6 2)) ))