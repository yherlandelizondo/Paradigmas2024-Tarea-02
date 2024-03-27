#lang racket
(require math)

(define deck '(A A A A 
              K K K K 
              Q Q Q Q 
              J J J J
              (2 03) (2 04) (2 05) (2 06)
              (3 03) (3 04) (3 05) (3 06)
              (4 03) (4 04) (4 05) (4 06)
              (5 03) (5 04) (5 05) (5 06)
              (6 03) (6 04) (6 05) (6 06)
              (7 03) (7 04) (7 05) (7 06)
              (8 03) (8 04) (8 05) (8 06)
              (9 03) (9 04) (9 05) (9 06)
              (10 03) (10 04) (10 05) (10 06)))


(define (randomList)
  (shuffle deck))


(provide randomList)
