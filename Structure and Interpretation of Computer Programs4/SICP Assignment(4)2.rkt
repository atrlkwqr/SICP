#lang racket


(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))


(define callnum 0)


(define (make-monitored fib)
  (lambda (n)
  (cond [(equal? n 'how-many-calls?) callnum]
        [(equal? n 'reset-call-count) (set! callnum 0)]
        [else
         (set! callnum (+ callnum 1))
         (if (< n 2)
             n
             (+ ((make-monitored fib) (- n 1)) ((make-monitored fib) (- n 2))))])))


(fib 8)
(set! fib (make-monitored fib))
(fib 8)
(fib 'how-many-calls?)
(fib 8)
(fib 'how-many-calls?)
(fib 'reset-call-count)
(fib 'how-many-calls?)