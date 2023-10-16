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

(define target 15)
(println 
  (count-tree `(1 (2 (3 () ()) (4 () ())) (15 () ()) (16 () ())) target)
)
