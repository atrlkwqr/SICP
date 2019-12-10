#lang racket

(define (bitfunc x) (- (+ (expt x 4) 4) (* 5 (expt x 2))))  ; 식 [x^4 - 5x^2 + 4] 의 값을 정의하는 구문입니다.


(define (bitfunc-integral-recur num-steps x1 x2) (if (= num-steps 0) 
                               0
                               (+ (* (bitfunc x1) (/ (- x2 x1) num-steps)) (bitfunc-integral-recur (- num-steps 1) (+ x1 1) x2))))


;; 재귀적으로 돌아가는 프로그램을 만듭니다.
;; x1에서 x1+1 까지의 사각형의 넓이를 구하고 num-steps 만큼 재귀합니다.
;; x1에는 x1+1 이 들어가기 때문에 다음 재귀에서는 x1+1에서 x1+2의 사격형의 넓이를 구하게 됩니다.


(define (bitfunc-integral-iter num-steps x1 x2)
                               (do ((a x1 (+ a 1))
                                    (k num-steps (- k 1))(total 0))
                                 ((< k 1) total)
                                 (set! total (+ total (* (bitfunc a) (/ (- x2 x1) num-steps))))))

;; 반복적으로 돌어가는 프로그램을 만듭니다.
;; x1에서 시작해 num-steps 만큼 반복합니다.
;; total에 사각형의 넓이의 값을 계속 더해줍니다.


;; ex) (bitfunc-integral-recur 3 1 4) = 40
;; ex) (bitfunc-integral-iter 3 1 4) = 40
;; ex) (bitfunc-integral-recur 4 1 5) = 220
;; ex) (bitfunc-integral-iter 4 1 5) = 220