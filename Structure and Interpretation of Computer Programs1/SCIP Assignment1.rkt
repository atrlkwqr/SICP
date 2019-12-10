#lang racket

(define (bitfunc x) (- (+ (expt x 4) 4) (* 5 (expt x 2))))  ; 식 [x^4 - 5x^2 + 4] 의 값을 정의하는 구문입니다.


;; ex) (bitfunc 2) = 0
;; ex) (bitfunc 3) = 40