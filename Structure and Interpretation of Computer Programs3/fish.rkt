#lang racket

(require racket/gui/base)
(require racket/draw)


;vector
(define (make-vect x y) (cons x y))
(define (xcor vect) (car vect))
(define (ycor vect) (cdr vect))


;curves
(define (make-curve p1 p2 p3 p4) (list p1 p2 p3 p4))
(define (start-curve cur) (car cur))
(define (control-curve cur) (cadr cur))
(define (2nd-control-curve cur) (caddr cur))
(define (end-curve cur) (cadddr cur))


(define p1 (make-vect 0.116 0.702))
(define p2 (make-vect 0.260 0.295))
(define p3 (make-vect 0.330 0.258))
(define p4 (make-vect 0.815 0.078))
(define p5 (make-vect 0.564 0.032))
(define p6 (make-vect 0.730 0.056))
(define p7 (make-vect 0.834 0.042))
(define p8 (make-vect 1.000 0.000))
(define p9 (make-vect 0.250 0.250))
(define p10 (make-vect 0.372 0.194))
(define p11 (make-vect 0.452 0.132))
(define p12 (make-vect 0.564 0.032))
(define p13 (make-vect 0.000 0.000))
(define p14 (make-vect 0.110 0.110))
(define p15 (make-vect 0.175 0.175))
(define p16 (make-vect 0.250 0.250))
(define p17 (make-vect -0.250 0.250))
(define p18 (make-vect -0.150 0.150))
(define p19 (make-vect -0.090 0.090))
(define p20 (make-vect 0.000 0.000))
(define p21 (make-vect -0.250 0.250))
(define p22 (make-vect -0.194 0.372))
(define p23 (make-vect -0.132 0.452))
(define p24 (make-vect -0.032 0.564))
(define p25 (make-vect -0.032 0.564))
(define p26 (make-vect 0.055 0.355))
(define p27 (make-vect 0.080 0.330))
(define p28 (make-vect 0.250 0.250))
(define p29 (make-vect -0.032 0.564))
(define p30 (make-vect -0.056 0.730))
(define p31 (make-vect -0.042 0.834))
(define p32 (make-vect 0.000 1.000))
(define p33 (make-vect 0.000 1.000))
(define p34 (make-vect 0.104 0.938))
(define p35 (make-vect 0.163 0.893))
(define p36 (make-vect 0.234 0.798))
(define p37 (make-vect 0.234 0.798))
(define p38 (make-vect 0.368 0.650))
(define p39 (make-vect 0.232 0.540))
(define p40 (make-vect 0.377 0.377))
(define p41 (make-vect 0.377 0.377))
(define p42 (make-vect 0.400 0.350))
(define p43 (make-vect 0.450 0.300))
(define p44 (make-vect 0.500 0.250))
(define p45 (make-vect 0.500 0.250))
(define p46 (make-vect 0.589 0.217))
(define p47 (make-vect 0.660 0.208))
(define p48 (make-vect 0.766 0.202))
(define p49 (make-vect 0.766 0.202))
(define p50 (make-vect 0.837 0.107))
(define p51 (make-vect 0.896 0.062))
(define p52 (make-vect 1.000 0.000))
(define p53 (make-vect 0.234 0.798))
(define p54 (make-vect 0.340 0.792))
(define p55 (make-vect 0.411 0.783))
(define p56 (make-vect 0.500 0.750))
(define p57 (make-vect 0.500 0.750))
(define p58 (make-vect 0.500 0.625))
(define p59 (make-vect 0.500 0.575))
(define p60 (make-vect 0.500 0.500))
(define p61 (make-vect 0.500 0.500))
(define p62 (make-vect 0.460 0.460))
(define p63 (make-vect 0.410 0.410))
(define p64 (make-vect 0.377 0.377))
(define p65 (make-vect 0.315 0.710))
(define p66 (make-vect 0.378 0.732))
(define p67 (make-vect 0.426 0.726))
(define p68 (make-vect 0.487 0.692))
(define p69 (make-vect 0.340 0.605))
(define p70 (make-vect 0.400 0.642))
(define p71 (make-vect 0.435 0.647))
(define p72 (make-vect 0.489 0.626))
(define p73 (make-vect 0.348 0.502))
(define p74 (make-vect 0.400 0.564))
(define p75 (make-vect 0.422 0.568))
(define p76 (make-vect 0.489 0.563))
(define p77 (make-vect 0.451 0.418))
(define p78 (make-vect 0.465 0.400))
(define p79 (make-vect 0.480 0.385))
(define p80 (make-vect 0.490 0.381))
(define p81 (make-vect 0.421 0.388))
(define p82 (make-vect 0.440 0.350))
(define p83 (make-vect 0.455 0.335))
(define p84 (make-vect 0.492 0.325))
(define p85 (make-vect -0.170 0.237))
(define p86 (make-vect -0.125 0.355))
(define p87 (make-vect -0.065 0.405))
(define p88 (make-vect 0.002 0.436))
(define p89 (make-vect -0.121 0.188))
(define p90 (make-vect -0.060 0.300))
(define p91 (make-vect -0.030 0.330))
(define p92 (make-vect 0.040 0.375))
(define p93 (make-vect -0.058 0.125))
(define p94 (make-vect -0.010 0.240))
(define p95 (make-vect 0.030 0.280))
(define p96 (make-vect 0.100 0.321))
(define p97 (make-vect -0.022 0.063))
(define p98 (make-vect 0.060 0.200))
(define p99 (make-vect 0.100 0.240))
(define p100 (make-vect 0.160 0.282))
(define p101 (make-vect 0.053 0.658))
(define p102 (make-vect 0.075 0.677))
(define p103 (make-vect 0.085 0.687))
(define p104 (make-vect 0.098 0.700))
(define p105 (make-vect 0.053 0.658))
(define p106 (make-vect 0.042 0.710))
(define p107 (make-vect 0.042 0.760))
(define p108 (make-vect 0.053 0.819))
(define p109 (make-vect 0.053 0.819))
(define p110 (make-vect 0.085 0.812))
(define p111 (make-vect 0.092 0.752))
(define p112 (make-vect 0.098 0.700))
(define p113 (make-vect 0.130 0.718))
(define p114 (make-vect 0.150 0.730))
(define p115 (make-vect 0.175 0.745))
(define p116 (make-vect 0.187 0.752))
(define p117 (make-vect 0.130 0.718))
(define p118 (make-vect 0.110 0.795))
(define p119 (make-vect 0.110 0.810))
(define p120 (make-vect 0.112 0.845))
(define p121 (make-vect 0.112 0.845))
(define p122 (make-vect 0.150 0.805))
(define p123 (make-vect 0.172 0.780))
(define p124 (make-vect 0.187 0.752))


(define fish-curves
   (list
      (make-curve p1 p2 p3 p4)
      (make-curve p5 p6 p7 p8)
      (make-curve p9 p10 p11 p12)
      (make-curve p13 p14 p15 p16)
      (make-curve p17 p18 p19 p20)
      (make-curve p21 p22 p23 p24)
      (make-curve p25 p26 p27 p28)
      (make-curve p29 p30 p31 p32)
      (make-curve p33 p34 p35 p36)
      (make-curve p37 p38 p39 p40)
      (make-curve p41 p42 p43 p44)
      (make-curve p45 p46 p47 p48)
      (make-curve p49 p50 p51 p52)
      (make-curve p53 p54 p55 p56)
      (make-curve p57 p58 p59 p60)
      (make-curve p61 p62 p63 p64)
      (make-curve p65 p66 p67 p68)
      (make-curve p69 p70 p71 p72)
      (make-curve p73 p74 p75 p76)
      (make-curve p77 p78 p79 p80)
      (make-curve p81 p82 p83 p84)
      (make-curve p85 p86 p87 p88)
      (make-curve p89 p90 p91 p92)
      (make-curve p93 p94 p95 p96)
      (make-curve p97 p98 p99 p100)
      (make-curve p101 p102 p103 p104)
      (make-curve p105 p106 p107 p108)
      (make-curve p109 p110 p111 p112)
      (make-curve p113 p114 p115 p116)
      (make-curve p117 p118 p119 p120)
      (make-curve p121 p122 p123 p124)
))


(define (+vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2))
                      (+ (ycor v1) (ycor v2))))


(define (scale-vect vect factor)
  (make-vect (* factor (xcor vect))
                        (* factor (ycor vect))))


(define (-vect v1 v2)
  (+vect v1 (scale-vect v2 -1)))


(define make-rectangle list)
(define origin1 first)
(define x-axis1 second)
(define y-axis1 third)


(define (coord-map rect)
  (lambda (p)
    (+vect (origin1 rect)
           (+vect (scale-vect (x-axis1 rect) (xcor p))
                  (scale-vect (y-axis1 rect) (ycor p))))))


(define (make-picture-from-curve curvelist)
  (lambda (rect)
     (for-each
        (lambda (cur)
              (let* ((b (start-curve cur))
                     (c (control-curve cur))
                     (d (2nd-control-curve cur))
                     (e (end-curve cur))
                     (m (coord-map rect))
                     (b2 (m b))
                     (c2 (m c))
                     (d2 (m d))
                     (e2 (m e))
                     (zee (new dc-path%)))
                (send zee move-to (xcor e2) (ycor e2))
                (send zee curve-to (xcor d2) (ycor d2)
                      (xcor c2) (ycor c2)
                      (xcor b2) (ycor b2))
                (send dc draw-path zee)))
        curvelist)))


(define (together4 pict1 pict2 pict3 pict4)
  (lambda (rect)
    (pict1 rect) (pict2 rect) (pict3 rect) (pict4 rect)))


(define (together pict1 pict2)
  (lambda (rect)
    (pict1 rect) (pict2 rect)))


(define origin (make-vect 0 0))
(define x-axis (make-vect 730 0))
(define y-axis (make-vect 0 730))
(define frame1 
   (make-rectangle origin 
                   x-axis
                   y-axis))


(define (rotate pict angle)
  (lambda (rect)
    (pict (make-rectangle
           (cond
             [(and (> (sin angle) 0) (>= (cos angle) 0)) (+vect (origin1 rect) (scale-vect (y-axis1 rect) (abs (* (/ angle pi) 2))))]
             [(and (>= (sin angle) 0) (< (cos angle) 0)) (+vect (+vect (origin1 rect) (y-axis1 rect))(scale-vect (x-axis1 rect)(- 1 (sin angle))))] 
             [(and (< (sin angle) 0) (<= (cos angle) 0)) (+vect (+vect (origin1 rect) (scale-vect (y-axis1 rect) (abs (- 1 (* (/ (- angle pi) pi) 2))))) (x-axis1 rect))]
             [(and (<= (sin angle) 0) (> (cos angle) 0)) (+vect (origin1 rect) (scale-vect (x-axis1 rect)(abs (sin angle))))])
           (-vect (scale-vect (x-axis1 rect) (cos angle)) (scale-vect (y-axis1 rect) (sin angle)))
           (+vect (scale-vect (x-axis1 rect) (sin angle)) (scale-vect (y-axis1 rect) (cos angle)))))))


(define (rotate90 pict)
   (rotate pict (* 0.5 pi)))


(define (flip pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin1 rect) (x-axis1 rect))
           (scale-vect (x-axis1 rect) -1)
           (y-axis1 rect)))))


(define (above-rotate45 pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin1 rect)(+vect (scale-vect (x-axis1 rect) (/ 1 2)) (scale-vect (y-axis1 rect) (/ 1 2))))
           (scale-vect (-vect (scale-vect (x-axis1 rect) (cos (* 0.25 pi))) (scale-vect (y-axis1 rect) (sin (* 0.25 pi)))) (/ 1 (sqrt 2)))
           (scale-vect (+vect (scale-vect (x-axis1 rect) (sin (* 0.25 pi))) (scale-vect (y-axis1 rect) (cos (* 0.25 pi)))) (/ 1 (sqrt 2)))))))


(define fish (make-picture-from-curve fish-curves))
(define fish2 (flip (above-rotate45 fish)))
(define fish3 (rotate fish2 (* 0.5 pi)))
(define fish4 (rotate fish2 (* 1.0 pi)))
(define fish5 (rotate fish2 (* 1.5 pi)))
(define fish-tile (together4 fish2 fish3 fish4 fish5))


(define (beside pict1 pict2 a)
  (lambda (rect)
    (pict1
     (make-rectangle
      (origin1 rect)
      (scale-vect (x-axis1 rect) a)
      (y-axis1 rect)))
    (pict2
     (make-rectangle
      (+vect
       (origin1 rect)
       (scale-vect (x-axis1 rect) a))
      (scale-vect (x-axis1 rect) (- 1 a))
      (y-axis1 rect)))))


(define (repeated f n)
  (if (= n 1)
      f
      (compose
       f(repeated f (- n 1)))))


(define (above pict1 pict2 a)
  (lambda (rect)
    (pict1
     (make-rectangle
      (origin1 rect)
      (x-axis1 rect)
      (scale-vect (y-axis1 rect) a)))
    (pict2
     (make-rectangle
      (+vect (origin1 rect) (scale-vect (y-axis1 rect) a))
      (x-axis1 rect)
      (scale-vect (y-axis1 rect) (- 1 a))))))


(define (quardtet p q r s)
  (above (beside p q 0.5) (beside r s 0.5) 0.5))


(define (nonet p q r s t u v w x)
  (above (beside (beside p q (/ 1 2)) r (/ 2 3))
         (above (beside (beside s t (/ 1 2)) u (/ 2 3))
                (beside (beside v w (/ 1 2)) x (/ 2 3)) (/ 1 2)) (/ 1 3)))


(define empty-picture (make-picture-from-curve null))


(define (side-push pict n)
  (if (<= n 0)
      empty-picture
      (quardtet (side-push pict (- n 1)) (side-push pict (- n 1)) (rotate90 pict) pict)))


(define (corner-push pict n)
  (if (<= n 0)
      empty-picture
      (quardtet (side-push pict (- n 1)) (corner-push pict (- n 1)) pict (rotate (side-push pict (- n 1)) (* 1.5 pi)))))


(define (square-limit pict n)
  (if (<= n 0)
      empty-picture
      (nonet (rotate90 (corner-push pict n))
             (side-push pict n)
             (corner-push pict n)
             (rotate90 (side-push pict n))
             pict
             (rotate (side-push pict n) (* 1.5 pi))
             (rotate (corner-push pict n) pi)
             (rotate (side-push pict n) pi)
             (rotate (corner-push pict n) (* 1.5 pi)))))


(define fish-square-limit (square-limit fish-tile 2))

;========================= Picture ==============================
; MAKING FRAME WINDOW, CANVAS, and DC
(define frame (new frame% [label "Paint_Fish"]
                   [width 747]
                   [height 769]))
(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (lambda(canvas dc)
                       (send dc set-pen red-pen)
                       (send dc set-brush no-brush)
                       (on-paint))]))
(define red-pen (make-object pen% "RED" 2 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define dc (send canvas get-dc))

;DEFINE CALLBACK PAINT PROCEDURE

;(define (on-paint) (fish frame1)) ; 2.1 Problem 1: make-picture-from-curve
;(define (on-paint) ((rotate fish (* 0.5 pi)) frame1)) ; 2.2 Problem 2: rotate
;(define (on-paint) ((rotate90 fish) frame1)) ; 2.2 Problem 2: rotate90
;(define (on-paint) ((above-rotate45 fish) frame1)) ; 2.2 Problem 3: above-rotate45
;(define (on-paint) (fish-tile frame1)) ; 2.3 Problem 3: Fish tile
;(define (on-paint) ((quardtet fish fish fish fish) frame1)) ;2.4 Problem 4: quardtet
;(define (on-paint) ((nonet fish fish fish fish fish fish fish fish fish) frame1)) ;2.4 Problem 4: nonet
;(define (on-paint) ((side-push fish-tile 2) frame1)) ;2.5 Problem 5: side-push
;(define (on-paint) ((corner-push fish-tile 2) frame1)) ;2.5 Problem 5: corner-push
(define (on-paint) (fish-square-limit frame1)) ;2.6 Problem 6: square-limit

;MAKING THE FRAME VISIBLE
(send frame show #t)