(define empty (vector))

empty

(vector? empty)

(vector-length empty)

(define non-empty (vector 1 2 3 4 5))

non-empty

(vector? non-empty)

(vector-length non-empty)

(vector-ref non-empty 2)

(vector-ref non-empty 4)

(vector-map (lambda (x) (+ x 1)) non-empty)

(define range-test (vector-range 0 35 5))

range-test

(vector-filter (lambda (x) (= (remainder x 3) 0)) range-test)

(vector-map * (vector 1 2 3) (vector 4 5 6))

(define append-result (vector-append (vector 1 2) (vector 3 4 5) (vector 6 7 8 9) (vector 10)))

(vector-length append-result)

append-result

(vector-append)

(vector-append (vector) (vector 1 2) (vector) (vector 3) (vector) (vector 4 5))