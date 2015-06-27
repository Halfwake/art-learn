#lang racket

(require racket/list)

(struct posn (x y) #:transparent)
(struct color (r g b a) #:transparent)
(struct triangle (posn-a posn-b posn-c color) #:transparent)

(define (score original attempt)
  0)

(define (tweak-color-1 a-color)
  (match-define (color r g b a) a-color)
  (match (first (shuffle '(r g b a)))
    ['r (color (random 256) g b a)]
    ['g (color r (random 256) b a)]
    ['b (color r g (random 256) a)]
    ['a (color r g b (random 256))]))

(define (bounded min n max)
  (cond [(< n min) min]
        [(> n max) max]
        [else n]))

(define min-x 0)
(define max-x 100)
(define min-y 0)
(define max-y 100)

(define (tweak-posn-1 a-posn)
  (match-define (posn x y) a-posn)
  (match (first (shuffle '(x y)))
    ['x (posn (bounded min-x (random 10) max-x) y)]
    ['y (posn x (bounded min-y (random 10) max-y))]))

(define (tweak-triangle-1 a-triangle)
  (match-define (triangle posn-a posn-b posn-c color) a-triangle)
  (match (first (shuffle '(posn-a posn-b posn-c color)))
    ['posn-a (triangle (tweak-posn-1 posn-a)
                       posn-b
                       posn-c
                       color)]
    ['posn-b (triangle posn-a
                       (tweak-posn-1 posn-b)
                       posn-c
                       color)]
    ['posn-c (triangle posn-a
                       posn-b
                       (tweak-posn-1 posn-c)
                       color)]
    ['color (triangle posn-a
                      posn-b
                      posn-c
                      (tweak-color-1 color))]))

(define (tweak-feature-1 triangles)
  (match-define (list* x xs) (shuffle triangles))
  (cons (tweak-triangle-1 x 0 400 0 400) xs))

(define (random-range min max)
  (+ (random (- max min)) min))

(define (random-posn)
  (posn (random-range min-x max-x)
        (random-range min-y max-y)))



