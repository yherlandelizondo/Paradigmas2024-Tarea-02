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
(send mainWindow show #t)