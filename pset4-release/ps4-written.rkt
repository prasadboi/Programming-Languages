#lang racket

(define vec (vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(define val (box 0))

(define (msg-self o m . a)
  (apply (o m) o a))

(define-syntax (msg/self stx)
  (syntax-case stx ()
    [(msg/self o m a ...)
     #' ((o m) o a ...)]))

(define obj-1
  (lambda (m)
    (case m
      [(up) (lambda (self) (set-box! val (+ (unbox val) 1)))]
      [(down) (lambda (self) (set-box! val (- (unbox val) 1)))]
      [(up1) (lambda (self)
                    (begin
                       (msg-self self 'up)
                       (msg-self self 'up)
                       (msg-self self 'down)
                       (vector-ref vec (unbox val))))])))

(set-box! val 0)
(displayln "using msg-self (which uses apply)")
(displayln (msg-self (begin (set-box! val (+ (unbox val) 4)) (displayln "val set to 4") obj-1) 'up1))
(display "val: ")
(displayln (unbox val))
(displayln "----------------------------------")

(define obj-2
  (lambda (m)
    (case m
      [(up) (lambda (self) (set-box! val (+ (unbox val) 1)))]
      [(down) (lambda (self) (set-box! val (- (unbox val) 1)))]
      [(up1) (lambda (self)
                    (begin
                       (msg/self self 'up)
                       (msg/self self 'up)
                       (msg/self self 'down)
                       (vector-ref vec (unbox val))))]))
  )

(set-box! val 0)
(displayln "using msg/self (which uses the macro)")
(displayln (msg/self (begin (set-box! val (+ (unbox val) 4)) (displayln "val set to 4") obj-2) 'up1))
(display "val: ")
(displayln (unbox val))