#lang racket
(require racket/gui/base)

;(require "interfaz.rkt")

(define players 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Selec Window;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define selecWindow (new frame% [label "BlaCEJack"]
                        [width 550]
                        [height 300]))


(define selecPanel (new horizontal-panel% [parent selecWindow]))
(define leftPanel (new vertical-panel% [parent selecPanel]
                       [min-width 150]
                       [min-height 300]
                       ))


(define space1 (new message% [parent leftPanel]
                           [label " "]))

(define space2 (new message% [parent leftPanel]
                           [label " "]))

(define space3 (new message% [parent leftPanel]
                           [label " "]))
(define welcomeMessage (new message% [parent leftPanel]
                           [label "Bienvenido a BlaCEJack"]))

(define space4 (new message% [parent leftPanel]
                           [label " "]))

(define bestPathTitle (new message% [parent leftPanel]
                           [label "Por favor seleccione la cantidad de jugadores presentes"]))

(define space5 (new message% [parent leftPanel]
                           [label " "]))
(define space6 (new message% [parent leftPanel]
                           [label " "]))

(define onePButton (new button% [parent leftPanel]
                           [label "   1   "]
                           [callback (lambda (button event)
                                       (onePButtonCallback event))]))

(define twoPButton (new button% [parent leftPanel]
                         [label "   2   "]
                         [callback (lambda (button event)
                                     (twoPButtonCallback event))]))

(define threePButton (new button% [parent leftPanel]
                         [label "   3   "]
                         [callback (lambda (button event)
                                     (threePButtonCallback event))]))




(define (onePButtonCallback event)
  (set! players 1)
  (send mainWindow show #t)
  (send selecWindow show #f))

(define (twoPButtonCallback event)
  (set! players 2)
  (send mainWindow show #t)
  (send selecWindow show #f))

(define (threePButtonCallback event)
  (set! players 3)
  (send mainWindow show #t)
  (send selecWindow show #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;show the selec frame;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send selecWindow show #t)
