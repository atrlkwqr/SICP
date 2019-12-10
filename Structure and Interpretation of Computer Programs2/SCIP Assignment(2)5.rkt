#lang racket

(define (deriv-variable wrt var) (if (and (symbol? wrt) (symbol? var) (eq? wrt var))
                                     1
                                     0))

;; wrt와 var이라는 변수에 심볼이 입력되있는지 확인합니다. 둘 다 심볼로 입력되있다면 같은지 확인하고 같다면 1 아니라면 0을 출력합니다.


;; ex) (deriv-variable 'x 'x) = 1
;; ex) (deriv-variable 'x 'y) = 0