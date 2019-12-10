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


(define (make-point x y) (cons x y))


(define (hash-a-point point N)
   (modulo (+ (car point) (cdr point))
           N))


(define (find-assoc key table)
  (cond
    ((null? table) 'ERROR)
    ((equal? key (caar table)) (cadar table))
    (else (find-assoc key (rest table)))))


(define (add-assoc key val alist)
  (cons (list key val) alist))


(define table-tag 'hash-table)
(define (make-table size hashfunc)
   (let ((buckets (make-vector size null)))
     (list table-tag size hashfunc buckets)))

(define (size-of tbl) (cadr tbl))
(define (hashfunc-of tbl) (caddr tbl))
(define (buckets-of tbl) (cadddr tbl))


(define (make-buckets N v) (make-vector N v))
(define bucket-ref vector-ref)
(define bucket-set! vector-set!)

(define (table-get tbl key)
  (let ((index
         ((hashfunc-of tbl) key (size-of tbl))))
    (find-assoc key
                (bucket-ref (buckets-of tbl) index))))

(define (table-put! tbl key val)
  (let ((index
         ((hashfunc-of tbl) key (size-of tbl)))
        (buckets (buckets-of tbl)))
    (bucket-set! buckets index
                 (add-assoc key val
                            (bucket-ref buckets index)))))

;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))