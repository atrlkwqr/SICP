#lang racket


(define (rectangle func x1 x2) (func x1))  ; func를 받아 사각형을 구하는 정의입니다.

(define (trapezoid func x1 x2) ( / (+ (func x1) (func (+ x1 1))) 2))  ; func를 받아 사다리꼴을 구하는 정의입니다.

(define (integral-with piece func num-steps x1 x2) (if (= num-steps 0)
                                                       0
                                                       (cond ((eq? piece rectangle)
                                                              (+ (* (/ (- x2 x1) num-steps) (func x1)) (integral-with rectangle func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2)))
                                                             ((eq? piece trapezoid)
                                                              (+ (+ (func x1) (func (+ x1 (/ (- x2 x1) num-steps)))) (integral-with rectangle func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))))))

;; piece에 rectangle 또는 trapezoid를 넣어 사각형 또는 사다리꼴을 이용해 사용자가 원하는 방식으로 값을 도출할 수 있도록 합니다.
;; 재귀적으로 도형의 넓이를 구해 더해줍니다.


;; ex) (integral-with rectangle (lambda (x) (+ (expt x 2) 3)) 1 5 7) = (2 * ( 5^2 + 3)) = 56
;; ex) (integral-with trapezoid (lambda (x) (+ (expt x 2) 3)) 1 5 7) = ((func(5) + func(7))) * 2 / 2 = (28 + 52) = 80