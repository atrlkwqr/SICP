#lang racket

(define (derivative wrt expr) (cond ((number? expr) 0)
                                    
                                    ((symbol? expr)
                                     (if (and (symbol? wrt) (eq? wrt expr))
                                              1
                                              0))
                                    
                                     (else (error "Don't know how to differentiate" expr))))

;; 두 번째 parameter(expr)를 첫 번째 parameter(wrt)로 미분하는 프로그램을 작성합니다.
;; cond 명령어로 여러가지 조건을 세웁니다.
;; 두 번째 받은 parameter(expr)가 숫자이면 0을 출력합니다.
;; 두 번째 받은 parameter(expr)가 심볼이라면 첫 번째 parameter(wrt)가 심볼이고 wrt와 expr가 포인터가 같다면 1, 아니라면 0을 출력합니다.
;; 위의 조건들 중 하나도 충족하지 못한다면 ["Don't know how to differentiate" expr] 메세지를 띄웁니다.


;; ex) (derivative 'x 3) = 0
;; ex) (derivative 'x 'x) = 1
;; ex) (derivative 'x '(+ x 3)) = Don't know how to differentiate (+ x 3)