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


;; 곱하기 옵션을 추가시킵니다.
;; expr가 pair인지 첫 번째 인자가 '*' 기호인지 확인하고 맞다면 (* b1 b2)로 만듭니다.
;; expr의 caddr을 미분시켜서 m1식을 만들고 expr의 cadr을 미분시켜서 m2식을 만듭니다. 그리고 m1과 m2를 더해주는 식을 만듭니다.


;; ex) (derivative 'x '(* x 3)) = '(+ (* x 0) (* 1 3))
;; ex) (derivative 'x '(+ (* x 5) 1)) = '(+ (+ (* x 0) (* 1 5)) 0)