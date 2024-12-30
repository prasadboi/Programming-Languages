#lang racket
(let ([x (+ 1 2)] [y (+ 1 3)] [z (+ 1 4)]) (+ x (+ y z)))
((lambda (x) (+  2 x)) 2)

; so what a lambda does is
; ((lambda ------ define the an instance of the expected input -------- perform an operation with this expected input) ----- pass argument of type expected input)

; there fore a let expression can be written as a lambda expression as follows:
; e1 e2 e3 should be evaluated in parallel
(define e1 (+ 3 2))
(define e2 (+ 3 3))
(define e3 (+ 3 4))

((((lambda (x)
    (lambda (y)
      (lambda (z)
        (+ x y z)))) e1) e3) e2)



; therefore a let* expression can be written as a nested lambda expression as follows:
; e2 should be in scope of lambda x
; e3 should be in scope of lambda y

; ((lambda (x) ((lambda (y) ((lambda (z) e) e3)) e2)) e1)
((lambda (x)  
    ((lambda (y)  
        ((lambda (z) (+ x (* y z)))  
            (+ x 2)))  
        (+ x 3)))  
    e1)


#|(lambda 3
  ((: 0 2) (lambda 2 ((: 1 2) (: 0 0))) (: 0 1)))
|#

#|(lambda (a1)
  (lambda (b1)
    a1
  )
)
|#