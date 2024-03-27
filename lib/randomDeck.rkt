#lang racket
(require math)

(define deck '(A A A A 
              K K K K 
              Q Q Q Q 
              J J J J
              2 2 2 2
              3 3 3 3
              4 4 4 4
              5 5 5 5
              6 6 6 6
              7 7 7 7
              8 8 8 8
              9 9 9 9
              10 10 10 10))


(define (randomList)
  (shuffle deck))


(provide randomList)
