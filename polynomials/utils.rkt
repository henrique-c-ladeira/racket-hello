#lang racket

; Provide functions to other files
(provide is-dense?)
(provide to-sparse)
(provide to-dense)

; Helper Functions
(define (zero? n) (= n 0))

; Check if the polynomial is in dense representation
(define (is-dense? p)
  (and (list? p) (not (pair? (car p)))))

; Check if the polynomial is in sparse representation
(define (is-sparse? p)
  (and (list? p) (pair? (car p))))

; Convert to dense representation
(define (to-dense p)
  (if (is-dense? p)
      p
      (let ((max-power (if (null? p) 0 (apply max (map cadr p)))))
        (let loop ((p p) (result '()) (i 0))
          (cond
           ((null? p) (append (reverse result) (make-list (- (+ max-power 1) i) 0)))
           ((= i (cadr (car p)))
            (loop (cdr p) (cons (car (car p)) result) (+ i 1)))
           (else
            (loop p (cons 0 result) (+ i 1))))))))
; Convert to sparse representation
(define (to-sparse p)
  (if (is-sparse? p) p
      (let loop ((p p) (result '()) (i 0))
        (if (null? p) (reverse result)
            (let ((coeff (car p)))
              (loop (cdr p) (if (zero? coeff) result (cons (list coeff i) result)) (+ i 1)))))))

; Degree of the polynomial
(define (degree p)
  (if (is-zero? p) -inf.0
      (if (is-dense? p) (- (length p) 1)
          (cadar (reverse p)))))

; Check if the polynomial is zero
(define (is-zero? poly)
  (zero? (length (filter (lambda (x) (not (= x 0))) (to-dense poly)))))

; Get the coefficient of x^k
(define (coeff p k)
  (if (is-dense? p)
      (if (< k (length p)) (list-ref p k) 0)
      (let ((term (assoc k p)))
        (if term (cdr term) 0))))

; Evaluate the polynomial at x = k
(define (eval p k)
  (if (is-dense? p)
      (let loop ((p p) (result 0) (i 0))
        (if (null? p) result
            (loop (cdr p) (+ result (* (car p) (expt k i))) (+ i 1))))
      (foldl (lambda (term result)
               (+ result (* (car term) (expt k (cadr term)))))
             0 p)))

; Helper Functions (Assuming previous definitions for is-dense?, is-sparse?, etc.)

; This is a helper function for multiply
(define (update-list-at lst index new-value)
  (define (loop lst i acc)
    (cond
      [(null? lst) (reverse acc)]
      [(= i index) (loop (cdr lst) (+ i 1) (cons new-value acc))]
      [else (loop (cdr lst) (+ i 1) (cons (car lst) acc))]))
  (loop lst 0 '()))

; Polynomial addition / Works I think but check if the math is correct
(define (add p1 p2)
  (let ([dp1 (to-dense p1)]
        [dp2 (to-dense p2)])
    (let* ([len1 (length dp1)]
           [len2 (length dp2)]
           [max-len (max len1 len2)]
           [padded-dp1 (append dp1 (make-list (- max-len len1) 0))]
           [padded-dp2 (append dp2 (make-list (- max-len len2) 0))])
      (map + padded-dp1 padded-dp2))))
; Polynomial subtraction / Works I think but check if the math is correct
(define (subtract p1 p2)
  (let ([dp1 (to-dense p1)]
        [dp2 (to-dense p2)])
    (if (> (length dp1) (length dp2))
        (let ([dp2-padded (append dp2 (make-list (- (length dp1) (length dp2)) 0))])
          (map - dp1 dp2-padded))
        (let ([dp1-padded (append dp1 (make-list (- (length dp2) (length dp1)) 0))])
          (map - dp1-padded dp2)))))

; Polynomial multiplication / Works I think but check if the math is correct
(define (multiply p1 p2)
  (let ([dp1 (to-dense p1)]
        [dp2 (to-dense p2)])
    (let ([result (make-list (+ (length dp1) (length dp2) -1) 0)])
      (for ([i (in-range (length dp1))])
        (for ([j (in-range (length dp2))])
          (let ([index (+ i j)])
            (set! result (update-list-at result index (+ (list-ref result index) (* (list-ref dp1 i) (list-ref dp2 j))))))))
      result)))
;this doesn't work
(define (gcd p1 p2)
  (if (is-zero? p2) p1
      (gcd p2 (remainder p1 p2))))




