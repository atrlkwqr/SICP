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

(define (find-assoc key table)
  (cond
    ((null? table) 'ERROR)
    ((equal? key (caar table)) (cadar table))
    (else (find-assoc key (cdr table)))))

(define (table-get tbl key)
  (if (table? tbl)
      (find-assoc key (cdr tbl))
      #f))

(define (table? table1)
  (if (equal? (car table1) 'table)
      #t
      #f))

(define (table-has-key? tbl key)
  (if (table? tbl)
      (fortf-find-assoc key (cdr tbl))
      #f))

(define (fortf-find-assoc key table)
      #f(cond
          [(null? table) #f]
          [(eq? key (caar table)) #t]
          [else (fortf-find-assoc key (cdr table))]))

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
  (set-cdr! table (add-assoc n callnum (cdr table)))
  (set! callnum 0)
  (if (< n 2)
      table
      (make-num-calls-table fib (- n 1))))
 

(define room-num 0)

(define (search-and-add n table)
  (if (eq? (table-has-key? table n) #f)
      (set-cdr! table (add-assoc n (fib n) (cdr table)))
      (table-get table n)))
  

(define (memoize fib)
  (set! room-num (+ room-num 1))
  (lambda (n)
    (if (null? (cdr table))
        (set-cdr! table (add-assoc n (fib n) (cdr table)))
        (search-and-add n table))
    (if (< n 2)
        (table-get table room-num)
    ((memoize fib) (- n 1)))))


(define (advise1 func fibnum a b c d) (a) (b) (c) (d))


(define (make-monitored-with-advice fib)
  (lambda (n)
    (set! callnum 0)
    (advise1 make-monitored
            ((make-monitored fib) n)
            (lambda () (display "Num calls: "))
            (lambda () (display callnum))
            (lambda () (display "\n"))
            (lambda () (display ((make-monitored fib) n)))
            )))


 (set! fib (make-monitored-with-advice fib))


;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))