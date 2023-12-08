#lang racket

(require "utils.rkt")

; Provide functions to other files
(provide single-input-wrapper)
(provide double-input-wrapper)

; Wrapper to function with 1 input
(define (single-input-wrapper fcn arg)
  (if (is-dense? arg)
    (fcn arg)
    (to-sparse (fcn (to-dense arg)))
  )
)

; Wrapper to function with 2 inputs
(define (double-input-wrapper fcn arg1 arg2)
  (if (and (is-dense? arg1) (is-dense? arg2))

    (fcn arg1 arg2)

    (if (is-dense? arg1)
      (fcn arg1 (to-dense arg2))

      (if (is-dense? arg2)
        (fcn (to-dense arg1) arg2)
        
        (to-sparse (fcn (to-dense arg1) (to-dense arg2)))
      )
    )
  )
)
