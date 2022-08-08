(define list-length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (list-length (cdr l))))))

(list-length null)

(list-length (cons 9 null))

(list-length (cons 9 (cons 9 (cons 9 (cons 9 (cons 9 null))))))

(list-length (cons "a" (cons "b" (cons "c" (cons "d" (cons "e" null))))))