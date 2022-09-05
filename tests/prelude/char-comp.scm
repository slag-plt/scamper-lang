(define c1 #\c)
(define c2 #\f)

(char=? c1 c1)
(char=? c1 c2)
(char=? c2 c1)
(char=? c2 c2)

(char<? c1 c1)
(char<? c1 c2)
(char<? c2 c1)
(char<? c2 c2)

(char>? c1 c1)
(char>? c1 c2)
(char>? c2 c1)
(char>? c2 c2)

(char<=? c1 c1)
(char<=? c1 c2)
(char<=? c2 c1)
(char<=? c2 c2)

(char>=? c1 c1)
(char>=? c1 c2)
(char>=? c2 c1)
(char>=? c2 c2)

(define c3 #\A)
(define c4 #\d)

(char-ci=? c3 c3)
(char-ci=? c3 c4)
(char-ci=? c4 c3)
(char-ci=? c4 c4)

(char-ci<? c3 c3)
(char-ci<? c3 c4)
(char-ci<? c4 c3)
(char-ci<? c4 c4)

(char-ci>? c3 c3)
(char-ci>? c3 c4)
(char-ci>? c4 c3)
(char-ci>? c4 c4)

(char-ci<=? c3 c3)
(char-ci<=? c3 c4)
(char-ci<=? c4 c3)
(char-ci<=? c4 c4)

(char-ci>=? c3 c3)
(char-ci>=? c3 c4)
(char-ci>=? c4 c3)
(char-ci>=? c4 c4)