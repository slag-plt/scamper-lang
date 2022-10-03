(define length
  (lambda (l)
    (match l
      [null 0]
      [(cons _ tail) (+ 1 (length tail))])))

(length (list 0 0 0 0 0))

(length null)

(length (list 0 0 0 0 0 0 0 0 0 0))

(define append
  (lambda (l1 l2)
    (match l1
      [null l2]
      [(cons head tail) (cons head (append tail l2))])))

(append (list 1 2 3) (list 4 5 6))

(define intersperse
  (lambda (x l)
    (match l
      [null null]
      [(cons _ null) l]
      [(cons x1 (cons x2 tail)) (cons x1 (cons x (intersperse x (cons x2 tail))))])))

(intersperse "," (list "a" "b" "c"))