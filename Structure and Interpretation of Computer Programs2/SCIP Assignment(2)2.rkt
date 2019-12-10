#lang racket


(define (approx-pi num-steps)
            
                    (do ((a (-  num-steps 1) (- a 1))
                         
                    (total 0))
                      
                    ((< a 0) total)
                      
                    (set! total (+ total (* 4 (* (sqrt (- 1 (expt (/ a  num-steps) 2))) (/ 1  num-steps)))))))

;; do 반복문으로 반복문을 만듭니다.
;; do 반복문 안에서는 (num-steps - 1)을 a라 하고 a를 1씩 감소시킵니다. total을 0으로 초기화한 후 a가 음수가 될 때 까지 넓이를 total에 더해주는 것을 반복합니다.
;; 반복적으로 더해줄 넓이는 주어진 식을 num-steps만큼 나눠서 적분해서 더할 사각형들입니다.
;; 식을 통해 구해준 넓이는 1사분면의 넓이이기 때문에 4를 곱함으로써 π(파이)에 가까운 값을 도출합니다.
;; num-steps의 값이 커질 수록 낭비되는 넓이가 작아지고 π(파이)에 수렴합니다.


;; ex) (approx-pi 10) = 3.3045183262483184
;; ex) (approx-pi 1000) = 3.143555466911027