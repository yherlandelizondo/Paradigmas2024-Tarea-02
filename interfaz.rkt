;Tarea 2
;Yherland Elizondo Cordero
;Gabriela Quesada Sancho
;Jafet Dixon Solano

#lang racket
(require racket/gui/base)
(require math)
(require "lib/randomDeck.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Variables y constantes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define players 0)
(define randomDeck (randomList))
(define dealerDeck '())
(define player1Deck '())
(define player2Deck '())
(define player3Deck '())
(define cont 1)
(define dealerScore 0)
(define player1Score 0)
(define player2Score 0)
(define player3Score 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;funciones especifica;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (print list) ;funcion para imprimir una lista
  (for-each display list)
  (newline))

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
  (initializeDecks)
  (send mainWindow show #t)
  (send selecWindow show #f))

(define (twoPButtonCallback event)
  (set! players 2)
  (initializeDecks)
  (send mainWindow show #t)
  (send selecWindow show #f))

(define (threePButtonCallback event)
  (set! players 3)
  (initializeDecks)
  (send mainWindow show #t)
  (send selecWindow show #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;creacion de las 4 decks iniciales;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initializeDecks)
  (cond ((< cont 3) (deckAuxiliar (cons (car randomDeck) dealerDeck)))
        ((< cont 5) (deckAuxiliar (cons (car randomDeck) player1Deck)))
        ((< cont 7) (deckAuxiliar (cons (car randomDeck) player2Deck)))
        (else (deckAuxiliar (cons (car randomDeck) player3Deck)))
          )
)

;printprintprintprintprintprintprintprintprintprintprintprintprintprintprintprintprintprintprint
(define (printAux)
  (print dealerDeck)
  (print player1Deck)
  (print player2Deck)
  (print player3Deck))

(define (deckAuxiliar newDeck)
  (cond ((< cont 3) (begin 
                          (set! dealerDeck newDeck)
                          ;(set! dealerScore (+ (car randomDeck) ))
  ))
        ((< cont 5) (set! player1Deck newDeck))
        ((< cont 7) (set! player2Deck newDeck))
        (else (set! player3Deck newDeck)))
  (set! cont (+ cont 1))
  (set! randomDeck (cdr randomDeck))
  (cond ((< cont 9) (initializeDecks))
        (else (printAux)))
)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;show the selec frame;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send selecWindow show #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;structure: Pantalla principal;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mainWindow (new frame% [label "BlaCEJack"]
                        [width 900]
                        [height 760]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;structure: Disposicion de la pantalla;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mainPanel (new vertical-panel% [parent mainWindow])) ;panel principal

(define upperPanel (new horizontal-panel% [parent mainPanel] ;panel superior 
                       [min-width 900] 
                       [min-height 280]                     
                       ))

(define lowerPanel (new horizontal-panel% [parent mainPanel] ;panel inferior
                        [min-width 900]
                        [min-height 480]
                        ))

(define player1Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 1
                        [min-width 300]
                        [min-height 480]
                        ))

(define player2Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 2
                        [min-width 300]
                        [min-height 480]
                        ))

(define player3Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 3
                        [min-width 300]
                        [min-height 480]
                        ))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: cartas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-cards canvas)
  (define dc (send canvas get-dc))
  (send dc set-text-foreground "black")
  (send dc set-font (make-object font% 14 'default 'normal 'normal))
  (send dc draw-rectangle 50 50 100 150) ; ejemplo de carta
  (send dc draw-text "A" 60 60) ; texto de ejemplo en la carta
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
  (draw-cards canvas)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Buttons for decisions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hit1 (new button% [parent player1Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                     (paint-callback1)
                                     (twoPButtonCallback event))]))

(define stand1 (new button% [parent player1Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (threePButtonCallback event))]))

(define hit2 (new button% [parent player2Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                     (twoPButtonCallback event))]))

(define stand2 (new button% [parent player2Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (threePButtonCallback event))]))

(define hit3 (new button% [parent player3Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                     (twoPButtonCallback event))]))

(define stand3 (new button% [parent player3Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (threePButtonCallback event))]))  

                                                                        