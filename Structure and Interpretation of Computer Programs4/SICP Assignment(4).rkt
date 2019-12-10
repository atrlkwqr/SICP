#lang racket

(define (add-1 x) (+ x 1))

(define (advise func a b) (lambda(n) (a) (b) (func n)))

(define advised-add-1
  (advise add-1
          (lambda () (displayln "calling add-1"))
          (lambda () (displayln "add-1 done"))))
