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

        ((substract? expr)
         (make-substract (derivative wrt (cadr expr))
                   (derivative wrt (caddr expr))))
        
         (else (error "Don't know how to differentiate" expr))))


(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))
 
(define (make-sum a1 a2) (list '+ a1 a2))
 
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))


(define (make-product b1 b2) (list '* b1 b2))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (make-substract a1 a2) (list '- a1 a2))

(define (substract? x)
  (and (pair? x) (eq? (car x) '-)))


;;optional problem
;;빼기 옵션 추가했습니다.


;; ex) (derivative 'x '(- (* x 6) 4)) =  '(- (+ (* x 0) (* 1 6)) 0)
;; ex) (derivative 'x '(- (- (* x 6) 4) 2)) = '(- (- (+ (* x 0) (* 1 6)) 0) 0)
;; ex) (derivative 'y '(- (* y 3) 7)) =  '(- (+ (* y 0) (* 1 3)) 0)
;; ex) (derivative 'y '(- (- (* y 2) 1) 4)) = '(- (- (+ (* y 0) (* 1 2)) 0) 0)
;; ex) (derivative 'z '(- (* z 1) 1)) = '(- (+ (* z 0) (* 1 1)) 0)
;; ex) (derivative 'z '(- (* (* z 8) z) 6)) = '(- (+ (* (* z 8) 1) (* (+ (* z 0) (* 1 8)) z)) 0)