(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
    (define (help zip pair1 pair2 pairs)
        (if (null? pairs) (list pair1 pair2)
            (help zip (append pair1 (list (caar pairs))) (append pair2 (list (car (cdar pairs)))) (cdr pairs) 
            )
        )
    )
(help zip nil nil pairs)
)


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (helper s count)
    (if (null? s) nil
      (cons (list count (car s)) (helper (cdr s) (+ count 1)))
    )
  )
  (helper s 0)
  )
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (define (helper comp list1 list2 merged)
    (cond 
      ((null? list1) (append merged list2))
      ((null? list2) (append merged list1))
      (else
        (helper comp (cdr list1) (cdr list2)
          (if (comp (car list1) (car list2)) (append merged (list (car list1) (car list2)))
        (append merged (list (car list2) (car list1))))
        )
    )
    )
  )
  (helper comp list1 list2 nil)
)
  ; END PROBLEM 16


;; Problem 17

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 17
         ;should just return the atom
         expr
         ; END PROBLEM 17
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 17
            expr
         ; END PROBLEM 17
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           ;form has no let statements
           ;params is the variables or arguments that lamda defines (no let to lambda)
           ;a body can have arbitrarily many statements, a built in begin statement? 
           ;use let to lambda recursively on the body and use map to do replace-this-line
           ;recontruct lamba form and return
           (append (list form params) (map let-to-lambda body))
           ; END PROBLEM 17
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           ;zip first/convert form
           ;appending the form and params but in let values
           (append (list (list 'lambda (car (zip values)) (car (map let-to-lambda body)))) 
            (map let-to-lambda (cadr (zip values)))
            ))
    
           ; END PROBLEM 17
           )
        (else
         ; BEGIN PROBLEM 17
         (map let-to-lambda expr)
         ; END PROBLEM 17
         )))

