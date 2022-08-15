(define fact
  (lambda (n)
    (if (zero? n)
        1 ; base case
        (* n (fact (- n 1)))))) ; recursive case

(fact 0)

(fact 5)