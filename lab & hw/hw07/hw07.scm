(define (filter-lst fn lst) 
  (cond ((null? lst) lst)
    ((fn (car lst)) (cons (car lst) (filter-lst fn (cdr lst))))
    (else (filter-lst fn (cdr lst)))
  )
)

; ;; Tests
(define (even? x) (= (modulo x 2) 0))

(filter-lst even? '(0 1 1 2 3 5 8))
;(filter-lst (lambda(x) (= x 5)) (list 5 4 5 4 2 2))

; expect (0 2 8)
(define (interleave first second) 
  (cond ((null? first) second)
    ((null? second) first)
    (else (cons (car first) (cons (car second) (interleave (cdr first) (cdr second)))))
))

(interleave (list 1 5 3) (list 2 4 6))

; expect (1 2 5 4 3 6)
(interleave (list 1 3 5) nil)

; expect (1 3 5)
(interleave (list 1 3 5) (list 2 4))

; expect (1 2 3 4 5)
(define (accumulate combiner start n term)
  (cond ((= n 0) start)
    (else (combiner (term n)(accumulate combiner start (- n 1) term)))
  )
)

(define (without-duplicates lst) 
; takes in a list and returns list without duplicates
; filter out with a condition
    (if (null? lst) lst
        (cons (car lst) (without-duplicates (filter-lst (lambda(x) (not(= x (car lst)))) lst)))
    )
)
