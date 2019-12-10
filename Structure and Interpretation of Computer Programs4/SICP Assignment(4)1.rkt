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


(define (find-assoc key table)
  (cond
    ((null? table) 'ERROR)
    ((equal? key (caar table)) (cadar table))
    (else (find-assoc key (rest table)))))


(define (add-assoc key val alist)
  (cons (list key val) alist))

(define table-tag 'table)

(define (make-table) (cons table-tag null))

(define (table? table1)
  (if (equal? (first table1) 'table)
      #t
      #f))

(define (table-get tbl key)
  (if (table? tbl)
      (find-assoc key (rest tbl))
      #f))

(define (table-put! tbl key val)
  (if (table? tbl)
      (set-cdr! tbl (add-assoc key val (rest tbl)))
      #f))

(define (table-has-key? tbl key)
  (if (table? tbl)
      (fortf-find-assoc key (rest tbl))
      #f))

(define (fortf-find-assoc key table)
      #f(cond
          [(null? table) #f]
          [(eq? key (caar table)) #t]
          [else (fortf-find-assoc key (rest table))]))



;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))