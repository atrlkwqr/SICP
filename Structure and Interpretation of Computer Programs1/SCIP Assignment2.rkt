#lang racket

(define (bitfunc-rect x1 x2) (* (- (+ (expt x1 4) 4) (* 5 (expt x1 2))) (- x2 x1)))  ; x1의 함숫값을 계산 후 x2와 x1의 차를 곱해 사각형의 넓이를 구합니다.


;; ex) (bitfunc-rect 2 3) = 0
;; ex) (bitfunc-rect 3 4) = 40