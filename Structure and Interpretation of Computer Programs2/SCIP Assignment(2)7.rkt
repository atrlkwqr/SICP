#lang racket


(define (derivative wrt expr)
  (cond ((number? expr) 0)
        
        ((symbol? expr)
         (if (same-variable? wrt expr) 1 0))
        
        ((sum? expr)
         (make-sum (derivative wrt (cadr expr))
                   (derivative wrt (caddr expr))))
        
        (else
         (error "Don't know how to differentiate" expr))))
 
(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))
 
(define (make-sum a1 a2) (list '+ a1 a2))
 
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; 첫 번째 parameter(wrt)가 두 번째 parameter(expr)과 같다면 1 아니라면 0을 출력합니다.
;; expr가 pair인지 첫 번째 인자가 '+' 기호인지 확인하고 맞다면 (+ a1 a2)로 만듭니다.
;; a1은 첫 번째 parameter(wrt)의 cadr [cdr -> car]을 a2는 caddr [cdr -> cdr -> car]입니다.
;; a1과 a2는 재귀를 합니다.


;; ex) (derivative 'x '(+ x 2)) = '(+ 1 0)
;; ex) (derivative 'x '(+ x 5)) = '(+ 1 0)