(apply string-length (list "HelloWorld"))
(apply list (list "HelloWorld" "HelloWorld" "HelloWorld"))
(apply string-split (list "HelloWorld" "l"))
(apply + (list 1 2 3 4 5 6 7 8 9 10))
(apply * (list 9 5 10))
(map string-length (list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld"))
(map procedure? (list string-length list + -) )
(map car (list (pair 2 4) (pair "a" "b") (pair "first" "second")))
(map cdr (list (pair 2 4) (pair "a" "b") (pair "first" "second")))