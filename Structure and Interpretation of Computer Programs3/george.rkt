#lang racket
(require racket/gui/base)


;vector
(define (make-vect x y) (cons x y))
(define (xcor vect) (car vect))
(define (ycor vect) (cdr vect))


;segment
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))


;george-vects
(define p1 (make-vect .25 0))
(define p2 (make-vect .35 .5))
(define p3 (make-vect .3 .6))
(define p4 (make-vect .15 .4))
(define p5 (make-vect 0 .65))
(define p6 (make-vect .4 0))
(define p7 (make-vect .5 .3))
(define p8 (make-vect .6 0))
(define p9 (make-vect .75 0))
(define p10 (make-vect .6 .45))
(define p11 (make-vect 1 .15))
(define p12 (make-vect 1 .35))
(define p13 (make-vect .75 .65))
(define p14 (make-vect .6 .65))
(define p15 (make-vect .65 .85))
(define p16 (make-vect .6 1))
(define p17 (make-vect .4 1))
(define p18 (make-vect .35 .85))
(define p19 (make-vect .4 .65))
(define p20 (make-vect .3 .65))
(define p21 (make-vect .15 .6))
(define p22 (make-vect 0 .85))


;george-lines
(define george-lines
  (list (make-segment p1 p2)
        (make-segment p2 p3)
        (make-segment p3 p4)
        (make-segment p4 p5)
        (make-segment p6 p7)
        (make-segment p7 p8)
        (make-segment p9 p10)
        (make-segment p10 p11)
        (make-segment p12 p13)
        (make-segment p13 p14)
        (make-segment p14 p15)
        (make-segment p15 p16)
        (make-segment p17 p18)
        (make-segment p18 p19)
        (make-segment p19 p20)
        (make-segment p20 p21)
        (make-segment p21 p22)
        ))


(define (+vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2))
                      (+ (ycor v1) (ycor v2))))


(define (scale-vect vect factor)
    (make-vect (* factor (xcor vect))
                        (* factor (ycor vect))))


(define make-rectangle list)
(define origin1 first)
(define x-axis1 second)
(define y-axis1 third)


(define (coord-map rect)
  (lambda (p)
    (+vect (origin1 rect)
           (+vect (scale-vect (x-axis1 rect) (xcor p))
                  (scale-vect (y-axis1 rect) (ycor p))))))


(define (make-picture seglist)
  (lambda (rect)
     (for-each
        (lambda (seg)
              (let* ((s (start-segment seg))
                     (e (end-segment seg))
                     (m (coord-map rect))
                     (b2 (m s))
                     (e2 (m e)))
                (send dc draw-line (xcor b2) (ycor b2)
                      (xcor e2) (ycor e2))))
        seglist)))


(define george (make-picture george-lines))
(define origin (make-vect 0 0))
(define x-axis (make-vect 730 0))
(define y-axis (make-vect 0 730))
(define frame1
  (make-rectangle origin
                  x-axis
                  y-axis))


(define (rotate90 pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin1 rect) (y-axis1 rect))
           (scale-vect (y-axis1 rect) -1)
           (x-axis1 rect)))))


(define (repeated f n)
  (if (= n 1)
      f
      (compose
       f(repeated f (- n 1)))))


(define rotate180 (repeated rotate90 2))


(define (flip pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin1 rect) (x-axis1 rect))
           (scale-vect (x-axis1 rect) -1)
           (y-axis1 rect)))))


(define (screen-transform pict)
  (lambda (rect)
    (pict (make-rectangle
           (+vect (origin1 rect) (y-axis1 rect))
           (x-axis1 rect)
           (scale-vect (y-axis1 rect) -1)))))


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


(define (above pict1 pict2 a)
  ((repeated rotate90 3)
   (beside (rotate90 pict1)
           (rotate90 pict2)
           a)))


(define (4pict p1 r1 p2 r2 p3 r3 p4 r4)
  (beside
   (above
    ((repeated rotate90 r1) p1)
    ((repeated rotate90 r2) p2)
    .5)
   (above
    ((repeated rotate90 r3) p3)
    ((repeated rotate90 r4) p4)
    .5)
   .5))


(define (up-push pict n)
  (if(= n 1)
     pict
     (above (up-push pict (- n 1))
            pict
            .25)))


(define (right-push pict n)
  (if (= n 1)
      pict
      (beside pict
              (right-push pict (- n 1))
              .75)))


(define (corner-push pict n)
  (if (= n 1)
      pict
      (above
       (beside
        (up-push pict n)
        (corner-push pict (- n 1))
        .75)
       (beside
        pict
        (right-push pict (- n 1))
        .75)
       .25)))


(define (4same p r1 r2 r3 r4)
  (4pict p r1 p r2 p r3 p r4))


(define (square-limit pict n)
  (4same (corner-push pict n)
         1 2 4 3))


(define acrobats;
  (beside (screen-transform george) (flip (rotate180 (screen-transform george))) .5))


(define 4bats
  (above acrobats
         (flip acrobats)
         .5))


(define george-squarelimit
         (square-limit 4bats 2))

;========================= Picture ==============================
; MAKING FRAME WINDOW, CANVAS, and DC
(define frame (new frame% [label "Paint_george"]
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
;(define (on-paint) (george frame1)) ;; 1.1 Problem 1: make-picture 구현
;(define (on-paint) ((screen-transform george) frame1)) ;; 1.2 Problem 2: screen-transform 구현
(define (on-paint) (george-squarelimit frame1)) ;; 1.3 Problem 3: square-limit 구현

;MAKING THE FRAME VISIBLE
(send frame show #t)