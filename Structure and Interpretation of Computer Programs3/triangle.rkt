#lang racket

(require racket/gui/base)
(require racket/draw)


;vector
(define (make-vect x y) (cons x y))
(define (xcor vect) (car vect))
(define (ycor vect) (cdr vect))

;segment
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))


(define p1 (make-vect 0 0))
(define p2 (make-vect 1 0))
(define p3 (make-vect 0 1))


(define triangle-lines
   (list
      (make-segment p1 p2)
      (make-segment p1 p3)
      (make-segment p2 p3)))


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


(define (make-picture-from-triangle seglist)
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


(define (repeatminus pict angle)
  (rotate pict (- angle (* 2.0 pi))))


(define (rotate pict angle)
  (lambda (rect)
    (pict (make-rectangle
           (cond
             [(and (> (sin angle) 0) (>= (cos angle) 0)) (+vect (origin1 rect) (scale-vect (y-axis1 rect) (abs (* (/ angle pi) 2))))] ;완료
             [(and (>= (sin angle) 0) (< (cos angle) 0)) (+vect (+vect (origin1 rect) (y-axis1 rect))(scale-vect (x-axis1 rect)(- 1 (sin angle))))] ;완료
             [(and (< (sin angle) 0) (<= (cos angle) 0)) (+vect (+vect (origin1 rect) (scale-vect (y-axis1 rect) (abs (- 1 (* (/ (- angle pi) pi) 2))))) (x-axis1 rect))];완료
             [(and (<= (sin angle) 0) (> (cos angle) 0)) (+vect (origin1 rect) (scale-vect (x-axis1 rect)(abs (sin angle))))]);완료
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


(define triangle (make-picture-from-triangle triangle-lines))
(define triangle2 (flip (above-rotate45 triangle)))
(define triangle3 (rotate triangle2 (* 0.5 pi)))
(define triangle4 (rotate triangle2 (* 1.0 pi)))
(define triangle5 (rotate triangle2 (* 1.5 pi)))
(define triangle-tile (together4 triangle2 triangle3 triangle4 triangle5))


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


(define empty-picture (make-picture-from-triangle null))


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
      pict
      (nonet (rotate90 (corner-push pict n))
             (side-push pict n)
             (corner-push pict n)
             (rotate90 (side-push pict n))
             pict
             (rotate (side-push pict n) (* 1.5 pi))
             (rotate (corner-push pict n) pi)
             (rotate (side-push pict n) pi)
             (rotate (corner-push pict n) (* 1.5 pi)))))


(define triangle-square-limit (square-limit triangle-tile 2))

;========================= Picture ==============================
; MAKING FRAME WINDOW, CANVAS, and DC
(define frame (new frame% [label "Paint_Triangle"]
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

(define (on-paint) (triangle-square-limit frame1))

;MAKING THE FRAME VISIBLE
(send frame show #t)