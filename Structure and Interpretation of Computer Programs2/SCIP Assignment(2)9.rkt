#lang racket

(define (derivative wrt expr)
  (cond ((number? expr) 0)
        
        ((symbol? expr)
         (if (same-variable? wrt expr) 1 0))
        
        ((sum? expr)
         (make-sum (derivative wrt (cadr expr))
                   (derivative wrt (caddr expr))))

        ((product? expr)
         (make-sum
           (make-product (cadr expr)
                         (derivative wrt (caddr expr)))
           (make-product (derivative wrt (cadr expr))
                         (caddr expr))))
        
         (else (error "Don't know how to differentiate" expr))))

(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))
 
(define (make-sum a1 a2) (list '+ a1 a2))
 
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))


(define (make-product b1 b2) (list '* b1 b2))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; 9번은 추가적인 test case를 적는 문제입니다.


;; ex) (derivative 'x '(* x 6)) = '(+ (* x 0) (* 1 6))
;; ex) (derivative 'x '(+ (* x 2) 2)) = '(+ (+ (* x 0) (* 1 2)) 0)
;; ex) (derivative 'x '(+ (+ (* x 6) 7) 5)) = '(+ (+ (+ (* x 0) (* 1 6)) 0) 0)
;; ex) (derivative 'x '(+ (* (* x 6) 2) 1)) = '(+ (+ (* (* x 6) 0) (* (+ (* x 0) (* 1 6)) 2)) 0)
;; ex) (derivative 'x '(*(* (* x 6) 7) 5)) = '(+ (* (* (* x 6) 7) 0) (* (+ (* (* x 6) 0) (* (+ (* x 0) (* 1 6)) 7)) 5))
;; ex) (derivative 'x '(+(*(* (* x 6) 7) 5) 1)) = '(+ (+ (* (* (* x 6) 7) 0) (* (+ (* (* x 6) 0) (* (+ (* x 0) (* 1 6)) 7)) 5)) 0)
;; ex) (derivative 'x '(- (* x 6) 4)) = Don't know how to differentiate (- (* x 6) 4)
;; ex) (derivative 'x '(+ (expt x 2) 4)) = Don't know how to differentiate (expt x 2)
;; ex) (derivative 'x '(/ (* x 3) 4)) = Don't know how to differentiate (/ (* x 3) 4)