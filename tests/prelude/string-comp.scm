(define s1 "hello world!")
(define s2 "hello zoo!")

(string=? s1 s1)
(string=? s1 s2)
(string=? s2 s1)
(string=? s2 s2)

(string<? s1 s1)
(string<? s1 s2)
(string<? s2 s1)
(string<? s2 s2)

(string>? s1 s1)
(string>? s1 s2)
(string>? s2 s1)
(string>? s2 s2)

(string<=? s1 s1)
(string<=? s1 s2)
(string<=? s2 s1)
(string<=? s2 s2)

(string>=? s1 s1)
(string>=? s1 s2)
(string>=? s2 s1)
(string>=? s2 s2)

(define s3 "HEllo World!")
(define s4 "heLLo zoo!")

(string-ci=? s3 s3)
(string-ci=? s3 s4)
(string-ci=? s4 s3)
(string-ci=? s4 s4)

(string-ci<? s3 s3)
(string-ci<? s3 s4)
(string-ci<? s4 s3)
(string-ci<? s4 s4)

(string-ci>? s3 s3)
(string-ci>? s3 s4)
(string-ci>? s4 s3)
(string-ci>? s4 s4)

(string-ci<=? s3 s3)
(string-ci<=? s3 s4)
(string-ci<=? s4 s3)
(string-ci<=? s4 s4)

(string-ci>=? s3 s3)
(string-ci>=? s3 s4)
(string-ci>=? s4 s3)
(string-ci>=? s4 s4)