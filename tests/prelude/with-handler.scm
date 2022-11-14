(with-handler
  (lambda (err) (string-append "This is the error that was generated: " err))
  (lambda (x y z) (+ x y z))
  1 2 3)

(with-handler
  (lambda (err) (string-append "This is the error that was generated: " err))
  (lambda (x y z) (error "oh no, an error!"))
  1 2 3)
