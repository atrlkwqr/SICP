#lang racket

(define (integral func num-steps x1 x2) (if (= num-steps 0)
                                    0
                                    (+ (* (/ (- x2 x1) num-steps) (func x1)) (integral func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))))

;; func를 받아 x에서 y까지 num-steps만큼 나누고 적분합니다.
;; 사각형의 넓이를 구할 때마다 num-steps의 값을 1씩 감소시키며 x의 값을 y와 x의 차를 num-steps로 나눈 만큼 더해주고 이를 재귀시킵니다.
;; num-steps의 값은 계속 줄어들지만 그에 따라 x의 값도 계속 늘어나기 때문에 적분 넓이를 구할 수 있습니다.

;; ex) (integral (lambda (x) (+ (expt x 2) 3)) 1 5 7) = (5^2 + 3) * 2 = 56
;; ex) (integral (lambda (x) (+ (expt x 2) 3)) 2 5 7) = (5^2 + 3) + (6^2 + 3) = 67

 