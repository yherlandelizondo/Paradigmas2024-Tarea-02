#lang racket

(define (drawCards canvas)
  (define dc (send canvas get-dc))
  (send dc set-text-foreground "black")
  (send dc set-font (make-object font% 14 'default 'normal 'normal))

  
  (send dc draw-rectangle 15 40 96 112) ; dibujando una carta
  (send dc draw-rectangle 126 40 96 112) ; dibujando una carta
  (send dc draw-rectangle 237 40 96 112) ; dibujando una carta

  (send dc draw-rectangle 15 184 96 112) ; dibujando una carta
  (send dc draw-rectangle 126 184 96 112) ; dibujando una carta
  (send dc draw-rectangle 237 184 96 112) ; dibujando una carta
  ;(send dc draw-text "♥" 60 60) ; texto en la carta
  )