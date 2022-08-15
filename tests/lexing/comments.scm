(define ; a line comment
  foo (lambda;another line comment)
    (x #| a block comment
    
    
|#) (+; more comments
#|y|#x;42
1));hah
)

(foo 10)