#lang racket

(define (tree-size x)
  (if (list? (car x))
    (if (null? (car x))
      0
      (if (null? (cdr x))
        (tree-size (car x))
        (+ (tree-size (car x)) (tree-size (cdr x)))
      )
    )
    (if (null? x)
      0
      (if (null? (cdr x))
        1
        (+ 1 (tree-size (cdr x)))
      )
    )
  )
)

(define (prune-tree x level)
  (if (null? x)
    `()
    (if (list? (car x))
      (cons 
        (prune-tree (car x) level) 
        (prune-tree (cdr x) level)
      )
      (if (= level 1)
        (list (car x) `() `())
        (cons (car x) (prune-tree (cdr x) (- level 1)))
      )
    )
  )
)

(define (count-tree x target)
  (if (null? x)
      0
    (if (list? (car x))
      (
        + 
        (count-tree (car x) target) 
        (count-tree (cdr x) target)
      )
      (if (= (car x) target)
        (+ 1 (count-tree (cdr x) target))
        (count-tree (cdr x) target)
      )
    )
  )
)

(println 
  (tree-size `(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())))
)

(define level 2)
(println 
  (prune-tree `(1 (2 (3 () ()) (4 () ())) (15 (3 () ()) ()) (16 () ())) level)
)

(define target 15)
(println 
  (count-tree `(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())) target)
)
