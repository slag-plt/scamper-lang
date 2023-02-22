(and (error "hello")
     #f)

(and #f
     (error "hello"))

(or (error "hello")
     #t)

(or #t
    (error "hello"))