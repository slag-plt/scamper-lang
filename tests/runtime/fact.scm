(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(fact 0)

(fact 5)