(define (split-at lst n) 
    ;takes a list st and non negative number n as
    ;input returns a pair new such that car new is 
    ;first n elements of lst
    (define (helper lst n help)
        (cond  ((null? lst) help)
        ((= n 0) help)
        (else (cons (car lst) (cons help)))
        (else (helper (cdr lst) (- n 1) (cons help (car lst)) ))
    ))
(helper (cdr lst) n (car lst))
)

(define (compose-all funcs) 
(cond
    ((null? funcs) (lambda (x) x))
    (else (lambda (x) ((compose-all (cdr funcs)) ((car funcs) x))))
  )
)
