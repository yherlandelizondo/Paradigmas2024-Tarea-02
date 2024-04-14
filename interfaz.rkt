;Tarea 2
;Yherland Elizondo Cordero
;Gabriela Quesada Sancho
;Jafet Dixon Solano

#lang racket
(require racket/gui/base)
(require math)
(require "lib/randomDeck.rkt")
(require "data/cons.rkt")

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
(define Anumd 0)
(define Anum1 0)
(define Anum2 0)
(define Anum3 0)
(define tempA 0)
(define valA 0)
(define tempId 0)

;;;;Cordenadas para las cartas de los jugadores / dealer;;;;;;;
(define dealerCoords (getCoords 0))
(define playerCoords (getCoords 1))

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
  (newline)
  (print dealerDeck)
  (print player1Deck)
  (print player2Deck)
  (print player3Deck))
  (newline)

;***********************************************************Sumar ya todos lo A**********************************************************************
(define (sumA id)
  (cond((equal? tempA 0)(send aWindow show #f))
       ((equal? id 0)(begin (set! tempA (- tempA 1))(send aWindow show #t)))
       ((equal? id 1)(begin (set! tempA (- tempA 1))(send aWindow show #t)))
       ((equal? id 2)(begin (set! tempA (- tempA 1))(send aWindow show #t)))
       (else(begin (set! tempA (- tempA 1))(send aWindow show #t)))))

(define (totalA id)
  (cond((equal? id 0)(set! dealerScore (+ dealerScore valA)))
       ((equal? id 1)(set! player1Score (+ player1Score valA)))
       ((equal? id 2)(set! player2Score (+ player2Score valA)))
       (else(set! player3Score (+ player3Score valA)))))




(set! player1Score (+ player1Score valA))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ventana para dar valores a A;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aWindow (new frame% [label "Valor de A"]
                        [width 550]
                        [height 300]))

(define aPanel (new horizontal-panel% [parent aWindow]))
(define leftaPanel (new vertical-panel% [parent aPanel]
                       [min-width 150]
                       [min-height 300]
                       ))

(define spac1 (new message% [parent leftaPanel]
                           [label " "]))

(define spac2 (new message% [parent leftaPanel]
                           [label " "]))

(define spac3 (new message% [parent leftaPanel]
                           [label " "]))
(define selecMessage (new message% [parent leftaPanel]
                           [label "Que valor desea darle a su carta A"]))

(define spac4 (new message% [parent leftaPanel]
                           [label " "]))

(define spac5 (new message% [parent leftaPanel]
                           [label " "]))
(define spac6 (new message% [parent leftaPanel]
                           [label " "]))

(define oneAButton (new button% [parent leftaPanel]
                           [label "   1   "]
                           [callback (lambda (button event)
                                       (oneAButtonCallback event))]))

(define elevenAButton (new button% [parent leftaPanel]
                         [label "   11   "]
                         [callback (lambda (button event)
                                     (elevenAButtonCallback event))]))

(define (oneAButtonCallback event)
  (set! valA (+ valA 1))
  (send aWindow show #f)
  (sumA tempId))

(define (elevenAButtonCallback event)
  (set! valA (+ valA 11))
  (send aWindow show #f)
  (sumA tempId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Verificación del puntaje total;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (veriChara character id)
  (cond ((equal? 'J character)10)
        ((equal? 'Q character)10)
        ((equal? 'K character)10)
        ((equal? 'A character)(begin
                                    (incA id) 0))
        (else character)))

(define (incA id)
  (cond((equal? id 0)(set! Anumd (+ Anumd 1)))
       ((equal? id 1)(set! Anum1 (+ Anum1 1)))
       ((equal? id 2)(set! Anum2 (+ Anum2 1)))
       (else(set! Anum3 (+ Anum3 1)))))

(define (deckAuxiliar newDeck)
  (cond ((< cont 3) (begin 
                      (set! dealerDeck newDeck)
                      (set! dealerScore (+ dealerScore (veriChara (caar randomDeck) 0)))))
        ((< cont 5) (begin
                      (set! player1Deck newDeck)
                      (set! player1Score (+ player1Score(veriChara (caar randomDeck) 1)))))
        ((< cont 7) (begin
                      (set! player2Deck newDeck)
                      (set! player2Score (+ player2Score (veriChara (caar randomDeck) 2)))))
        
        (else (begin
                (set! player3Deck newDeck)
                (set! player3Score (+ player3Score(veriChara (caar randomDeck) 3))))))
  
  (set! cont (+ cont 1)) ; aumentando el contador auxiliar para asignar cartas
  (set! randomDeck (cdr randomDeck)) ;quitando del deck principal la carta ya asignada
  (cond ((< cont 9) (initializeDecks))
        (else (printAux)))
)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;show the selec frame;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send selecWindow show #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;structure: Pantalla principal;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mainWindow (new frame% [label "BlaCEJack"]
                        [width 1050]
                        [height 670]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;structure: Disposicion de la pantalla;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mainPanel (new vertical-panel% [parent mainWindow])) ;panel principal

(define upperPanel (new horizontal-panel% [parent mainPanel] ;panel superior 
                       [min-width 1050] 
                       [min-height 280]                     
                       ))

(define lowerPanel (new horizontal-panel% [parent mainPanel] ;panel inferior
                        [min-width 1050]
                        [min-height 390]
                        ))

(define player1Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 1
                        [min-width 350]
                        [min-height 390]
                        ))

(define player2Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 2
                        [min-width 350]
                        [min-height 390]
                        ))

(define player3Panel (new vertical-panel% [parent lowerPanel] ; panel para jugador 3
                        [min-width 350]
                        [min-height 390]
                        ))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: cartas de jugadores;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;formato para dibujar cartas: (send dc draw-rectangle xCoord yCoord width height)

(define (drawCards canvas cardList coordList) 
  
  (define dc (send canvas get-dc))
  (send dc set-text-foreground "black")
  (send dc set-font (make-object font% 14 'default 'normal))

  ; Recorrer la lista de cartas y las coordenadas asociadas
  (for ([card cardList] ;card: carta actual
        [coord coordList] ;coord: lista de coordenadas de la carta actual
        [i (in-naturals)]) ;indice para el bucle
    
    (define coords (car coord)) ;obteniendo las coordenadas por separado
    (define dimentions (cadr coord)) ;obteniendo las dimensiones por separado

    (define xCoord (car coords)) ;extrayendo el valor de la coord x de la carta
    (define yCoord (cadr coords)) ;extrayendo el valor de la coord y de la carta

    (define width (car dimentions)) ;extrayendo la anchura de la carta
    (define height (cadr dimentions)) ;extrayendo la altura de la carta

    (define (valueType) ;funcion utilizada por la variabilidad de los valores de cartas, ej: A, 6, K.
      (cond ;extrayendo el valor de la carta y pasandolo a string
        ((number? (car card))
          (number->string (car card))) ;si es numero lo pasa a string

        (else 
          (symbol->string (car card))) ;si es un simbolo lo pasa a string
      )
    )
    (valueType)
    
     
    (define suit (symbol->string (cadr card))) ;extrayendo el simbolo de la carta y pasandolo a string

    (send dc draw-rectangle xCoord yCoord width height) ;dibujando la carta con dimensiones y coords extraidas

    (define suitX (+ xCoord 42)) ;definiendo una posición para colocar el simb de la carta en x
    (define suitY (+ yCoord 42)) ;definiendo una posición para colocar el simb de la carta en y

    (define textX (+ xCoord 5)) ;definiendo una posición para colocar el text de la carta en x
    (define textY (+ yCoord 5)) ;definiendo una posición para colocar el text de la carta en y

    (send dc draw-text suit suitX suitY) ;dibujando el simbolo de la carta
    (send dc draw-text (valueType) textX textY) ;dibujando el valor de la carta
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: canvas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;functions to refresh each canvas

(define (paint-callback0 canvas dc)
  (send dc set-background (make-object color% 58 170 63)) ;color verde
  (send dc clear)
  (drawCards canvas dealerDeck dealerCoords)
 )

(define (paint-callback1 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  (drawCards canvas player1Deck playerCoords)
  
 )

(define (paint-callback2 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  (drawCards canvas player2Deck playerCoords)
 )

(define (paint-callback3 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  (drawCards canvas player3Deck playerCoords)
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
                                     (hit 1))]))

(define stand1 (new button% [parent player1Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (stand1ButtonCallback event))]))

(define hit2 (new button% [parent player2Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                     (hit 2))]))

(define stand2 (new button% [parent player2Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (stand2ButtonCallback event))]))

(define hit3 (new button% [parent player3Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                     (hit 3))]))

(define stand3 (new button% [parent player3Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (stand3ButtonCallback event))]))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;Logic: logica para los botones de plantarse;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stand1ButtonCallback event)
  (set! tempA Anum1)
  (set! Anum1 0)
  (set! tempId 1)
  (sumA 1))
(define (stand2ButtonCallback event)
  (set! tempA Anum2)
  (set! Anum2 0)
  (set! tempId 2)
  (sumA 2))
(define (stand3ButtonCallback event)
  (set! tempA Anum3)
  (set! Anum3 0)
  (set! tempId 3)
  (sumA 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;Logic: logica para los botones de pedir;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hit playerId) ;funcion para anadir una carta nueva a la baraja de cada jugador
  (cond 
    ((equal? playerId 0) (begin ;si se trata del jugador 1, se anade la carta y su valor en puntaje 
        (set! dealerDeck (cons (car randomDeck) dealerDeck))
        (set! dealerScore (+ player1Score (veriChara (caar randomDeck) 1)))
        (set! randomDeck (cdr randomDeck)) ;quitando del deck principal la carta ya asignada
        (send upperPanel refresh) ; Refrescar el canvas

    ))
    ((equal? playerId 1) (begin ;si se trata del jugador 1, se anade la carta y su valor en puntaje 
        (set! player1Deck (cons (car randomDeck) player1Deck))
        (set! player1Score (+ player1Score (veriChara (caar randomDeck) 1)))
        (set! randomDeck (cdr randomDeck)) ;quitando del deck principal la carta ya asignada
        (send player1Panel refresh) ; Refrescar el canvas

    ))
    ((equal? playerId 2) (begin ;si se trata del jugador 2, se anade la carta y su valor en puntaje 
        (set! player2Deck (cons (car randomDeck) player2Deck))
        (set! player2Score (+ player2Score (veriChara (caar randomDeck) 2)))
        (set! randomDeck (cdr randomDeck)) ;quitando del deck principal la carta ya asignada
        (send player2Panel refresh) ; Refrescar el canvas
    ))
    (else(begin ;si se trata del jugador 3, se anade la carta y su valor en puntaje 
        (set! player3Deck (cons (car randomDeck) player3Deck))
        (set! player3Score (+ player3Score (veriChara (caar randomDeck) 3))) 
        (set! randomDeck (cdr randomDeck)) ;quitando del deck principal la carta ya asignada
        (send player3Panel refresh) ; Refrescar el canvas
    ))
  )
  
  (printAux)
)

(define (test event)
  (newline)
  (display tempA)
  (newline))