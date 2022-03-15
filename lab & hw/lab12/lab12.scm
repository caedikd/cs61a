(define (tail-replicate x n) 
  (define (tail-replicated x n lst)
    (if (= n 0)
      lst
      (tail-replicated x (- n 1) (cons x lst)))
  )
  (tail-replicated x n nil)
)

(define-macro (def func args body)
  `(define ,func (lambda ,args ,body))
)

;simulates a python def statement allowing you to write
;like (def f(x y) (+ x y))
;create a function with parameters x and y, 
;and body (+ x y), then bind it to the name f in the current frame.

(define (repeatedly-cube n x)
  (if (zero? n)
      x
      (let ((y (repeatedly-cube (- n 1) x)))
        (* y y y))))

;cubes the given value x some number n times, based on the given skeleton.