#lang racket

(require "utils.rkt")
(require "input-wrapper.rkt")

(define (derivative-dense p)
  (cdr 
    (map 
      (lambda (number idx) 
        (* number idx)
      )
      p
      (build-list (length p) identity)
    )
  )
)

(define (derivative p)
  (single-input-wrapper derivative-dense p)
)

(display (derivative `(3 4 6 7)))
(newline)
(display (derivative `((3 0) (4 1) (6 2) (7 3))))