#lang racket

;;; (listcount x) -> returns the number of elements in x
;;; listcount is the same as length
(define (listcount x)
  (if (null? x) 0 (+ 1 (listcount (cdr x)))))

;;; (isMember? x y) -> #t if y contains x; #f otherwise
(define (isMember? x y)
  (if (null? y) (if (null? x) #t #f)
  (if (equal? x (car y)) #t (isMember? x (cdr y)))))


;;; (customAppend x y) -> 
;;; returns a new list consisting of the elements of x followed by y
(define (customAppend x y)
  (if (null? x)
    (list y)
    (cons (car x) (customAppend (cdr x) y))
  )
)

;;; (customUnion x y) -> return a list that has the elements of x,
;;;  followed by the elements of y that are not found in x
(define (customUnion x y)
  (if (isMember? (car y) x)
    (if (null? (cdr y))
      x
      (customUnion x (cdr y))
    )
    (customUnion (customAppend x (car y)) (cdr y))
  )
)

(println (isMember? `(4 5 (6)) `(1 2 3 (4 5 (6)))))
(println #t)

(println (isMember? `4 `(1 2 3 (4 5 (6)))))
(println #f)

(println (customAppend `(1 2 3) `4))
(println `(1 2 3 4))

(println (customAppend `(1 2 3) `(4 5 6)))
(println `(1 2 3 (4 5 6)))

(println (customUnion `(1 3 5 7 9) `(1 2 3 4 5 6 7)))
(println `(1 3 5 7 9 2 4 6))