#lang racket

(define (better-pi num-steps)
            
                    (do ((a (-  num-steps 1) (- a 1))
                         
                    (total 0))
                      
                    ((< a 0) total)
                      
                    (set! total (+ total (* 4  (/ (* (+ (sqrt (- 1 (expt (/ a num-steps) 2))) (sqrt (- 1 (expt (+ (/ 1 num-steps) (/ a num-steps)) 2)))) (/ 1 num-steps)) 2))))))


;; 사각형을 이용하는 것보다 사다리꼴을 이용하는 것이 더 approximation하기 때문에 사다리꼴을 이용해 과제 2번에서 만들었던 프로그램보다 더 나은 프로그램을 만듭니다.
;; 재귀를 사용하지 않고 do반복문을 사용합니다. num-steps만큼 나눠지는 사다리꼴을 total에 다 더해줍니다.


;; ex) (better-pi 10) = 3.1045183262483187
;; ex) (better-pi 100) = 3.140417031779046
;; ex) (better-pi 1000) = 3.1415554669110293