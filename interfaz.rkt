;Tarea 2
;Yherland Elizondo Cordero
;Gabriela Quesada Sancho
;Jafet Dixon Solano

#lang racket
(require racket/gui/base)

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

;*******************************************************************************************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;structure: Pantalla principal;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mainWindow (new frame% [label "BlaCEJack"]
                        [width 900]
                        [height 700]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;structure: Disposicion de la pantalla;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mainPanel (new vertical-panel% [parent mainWindow])) ;panel principal

(define upperPanel (new horizontal-panel% [parent mainPanel] ;panel superior 
                       [min-width 900] 
                       [min-height 280]                     
                       ))

(define lowerPanel (new horizontal-panel% [parent mainPanel] ;panel inferior
                        [min-width 900]
                        [min-height 420]
                        ))

(define player1Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 1
                        [min-width 300]
                        [min-height 420]
                        ))

(define player2Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 2
                        [min-width 300]
                        [min-height 420]
                        ))

(define player3Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 3
                        [min-width 300]
                        [min-height 420]
                        ))                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: Lineas divisoras;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-lines canvas)
  (define dc (send canvas get-dc))
  (send dc set-pen "black" 2 'solid)
  (send dc draw-line 0 280 900 280) ; División horizontal
  
  ; Dibujar líneas verticales en la sección de abajo
  (send dc draw-line 300 280 300 700)  ; División vertical 1
  (send dc draw-line 600 280 600 700) ; División vertical 2
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: canvas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;functions to refresh each canvas

(define (paint-callback0 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
 )

(define (paint-callback1 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
 )

(define (paint-callback2 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
 )

(define (paint-callback3 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
 )

;dealer canvas definition
(define dealerCanvas (new canvas% [parent upperPanel]
                    [style (list 'border)]
                    [paint-callback paint-callback0]
                    ))

;player 1 canvas definition
(define player1Canvas (new canvas% [parent player1Panel]
                    [style (list 'border)]
                    [paint-callback paint-callback1]
                    ))

;player 2 canvas definition
(define player2Canvas (new canvas% [parent player2Panel]
                    [style (list 'border)]
                    [paint-callback paint-callback2]
                    ))

;player 3 canvas definition
(define player3Canvas (new canvas% [parent player3Panel]
                    [style (list 'border)]
                    [paint-callback paint-callback3]
                    ))                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;show the frame;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send mainWindow show #f); esto lo puse en false ******************************************************************************************************