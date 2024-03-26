;Tarea 2
;Yherland Elizondo Cordero
;Gabriela Quesada Sancho
;Jafet Dixon Solano

#lang racket
(require racket/gui/base)

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
;function to refresh the canvas
(define (paint-callback canvas dc)
  (send dc set-background "green")
  (send dc clear)
  (draw-lines canvas)
 )

;canvas definition
(define canvas (new canvas% [parent player2Panel]
                    [style (list 'border)]
                    [paint-callback paint-callback]
                    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;show the frame;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send mainWindow show #t)