(define inc
  (lambda (x) (+ x 1)))

(inc 1)

((compose inc) 1)

((compose inc inc inc inc inc) 1)

(|> 1 inc)

(|> 1 inc inc inc inc inc)

(|> "hello"
    string->list
    (lambda (l) (filter (lambda (c) (not (char=? c #\l))) l))
    list->string)

((compose length
          (lambda (l) (filter (lambda (n) (even? n)) l)))
 (range 10))

(define string-reverse (o list->string reverse string->list))

(string-reverse "hello")