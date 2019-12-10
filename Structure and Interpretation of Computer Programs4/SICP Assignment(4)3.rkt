#lang racket

;; By default, Racket doesn't have set-car! and set-cdr! functions.  The
;; following line allows us to use those:
(require r5rs)
(require rackunit)
;; Unfortunately, it does this by making cons return a "mutable pair"
;; instead of a "pair", which means that many built-ins may fail
;; mysteriously because they expect a pair, but get a mutable pair.
;; Re-define a few common functions in terms of car and friends, which
;; the above line make work with mutable pairs.
(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
;; We also tell DrRacket to print mutable pairs using the compact syntax
;; for ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)


(define table-tag 'table)

(define (make-table) (cons table-tag null))

(define table (make-table))

(define (add-assoc key val alist)
  (cons (list key val) alist))

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


(define (make-num-calls-table fib n)
  ((make-monitored fib) n)
  (set-cdr! table (add-assoc n callnum (rest table)))
  (set! callnum 0)
  (if (< n 2)
      table
      (make-num-calls-table fib (- n 1))))




;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))