(define sample-vector (vector "alpha" "beta" "gamma" "delta" "epsilon"))
sample-vector
(vector-set! sample-vector 2 "zeta")
sample-vector
(vector-set! sample-vector 0 "foo")
sample-vector
(vector-set! sample-vector 2 -38.72)
sample-vector
(vector-fill! sample-vector "woot")
sample-vector