;Tarea 2
;Yherland Elizondo Cordero
;Gabriela Quesada Sancho
;Jafet Dixon Solano

#lang racket
(require racket/gui/base)
(require math)
(require "lib/randomDeck.rkt")
(require "data/cons.rkt")
(require "./logica.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Variables y constantes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;informacion de los jugadores/dealer;;;;;;;
(define players 0)
(define hiddenCard 0)
(define randomDeck (randomList))
(define dealerDeck '())
(define player1Deck '())
(define player2Deck '())
(define player3Deck '())
(define dealerScore 0)
(define player1Score 0)
(define player2Score 0)
(define player3Score 0)
(define msg1 0)
(define msg2 0)
(define winner1 0)


;;;;;;;variable booleana para controlar el flujo;;;;;;;
(define dealerPlaying 0) ;sin esta variable, si el juego planta a alguien se encicla el programa

;;;;;;;contador provicional para asignar barajas/puntajes iniciales;;;;;;;
(define cont 1) 

;;;;;;;variables para realizar la suma de los aces en el puntaje;;;;;;;
(define Anumd 0) ;cantidad de A's del dealer
(define Anum1 0) ;cantidad de A's del jugador 1
(define Anum2 0) ;cantidad de A's del jugador 2 
(define Anum3 0) ;cantidad de A's del jugador 3
(define tempA 0) ;variable temporal de cantidad de A's del jugador que se esta analizando en ese momento 
(define valA 0) ;valor que va sumando de los A's
(define tempId 0) ;

;;;;;;;variables booleanas para manejar el "plantarse. 1=plantado 0=jugando;;;;;;;
(define dealerStand 0)
(define player1Stand 0)
(define player2Stand 0)
(define player3Stand 0)

;;;;;;;Cordenadas para las cartas de los jugadores / dealer;;;;;;;
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

(define (onePButtonCallback event) ;si se pulsa un jugador
  (set! players 1) ;modificando la cantidad de jugadores
  (initializeDecks) ;se inicializan las bajaras
  (send mainWindow show #t) ;se muestra la ventana de juego
  (send selecWindow show #f)) ;se desactiva la ventana de seleccion de jugador

(define (twoPButtonCallback event) ;si se pulsa dos jugadores
  (set! players 2) ;modificando la cantidad de jugadores
  (initializeDecks) ;se inicializan las bajaras
  (send mainWindow show #t) ;se muestra la ventana de juego
  (send selecWindow show #f)) ;se desactiva la ventana de seleccion de jugador

(define (threePButtonCallback event) ;si se pulsa tres jugadores
  (set! players 3) ;modificando la cantidad de jugadores
  (initializeDecks) ;se inicializan las bajaras
  (send mainWindow show #t) ;se muestra la ventana de juego
  (send selecWindow show #f)) ;se desactiva la ventana de seleccion de jugador



;***********************************************************Verificar si es mayor a 21*************************************************************************
(define(rmvIllegal idWinner1); La función se utiliza para verificar si algún jugador posee un puntaje mayor a 21 ya que perdería el juego
  ;Se identifica el id de los jugadores que perdieron por tener un puntaje mayor a 21 y se colocan sus valores en 0, además se identifica
  ;y se excluye el ganador ya que este se presenta al verificar el JackBlack
    (cond ((or (> dealerScore 21) (equal? 0 idWinner1))(begin (set! dealerScore 0))))
    (cond ((or (> player1Score 21) (equal? 1 idWinner1))(begin (set! player1Score 0))))
    (cond ((or (> player2Score 21) (equal? 2 idWinner1))(begin (set! player2Score 0))))
    (cond ((or (> player3Score 21) (equal? 3 idWinner1))(begin (set! player3Score 0))))
    (winner dealerScore player1Score player2Score player3Score));Se envían todos los puntaje a la función winner la cuál calcula el podio

;****************************************************************Gana el 21 con menos cartas*******************************************************************
;Para poder verificar si un jugador tiene el BlackJack se debe verificar todos los masos
(define(smallDeck ptd pt1 pt2 pt3 ld l1 l2 l3)
  (cond((not (= 21 ptd))(smallDeck1 ptd pt1 pt2 pt3 7 l1 l2 l3))
       (else(smallDeck1 ptd pt1 pt2 pt3 ld l1 l2 l3))));si el puntaje no es 21 se descarta ya que no es posible que sea el BlackJack
(define(smallDeck1 ptd pt1 pt2 pt3 ld l1 l2 l3);Se verifican los 4 puntajes en forma de cascada 
  (cond((not (= 21 pt1))(smallDeck2 ptd pt1 pt2 pt3 ld 7 l2 l3))
       (else (smallDeck2 ptd pt1 pt2 pt3 ld l1 l2 l3))))
(define(smallDeck2 ptd pt1 pt2 pt3 ld l1 l2 l3)
  (cond((not (= 21 pt2))(smallDeck3 ptd pt1 pt2 pt3 ld l1 7 l3))
       (else (smallDeck3 ptd pt1 pt2 pt3 ld l1 l2 l3))))
(define(smallDeck3 ptd pt1 pt2 pt3 ld l1 l2 l3)
  (cond((not (= 21 pt3))(smallWinner ptd pt1 pt2 pt3 ld l1 l2 7))
  (else (smallWinner ptd pt1 pt2 pt3 ld l1 l2 l3))))

;Se tomaron las barajas de todos los jugadores que tengan 21 y el que posea el BlackJack o el menor número de cartas será el ganador 
(define (smallWinner ptd pt1 pt2 pt3 ld l1 l2 l3)
  ;Para que cumpla los requisitos tiene que tener la baraja con menor logitud y no haberse descartado en smallDeck
  (cond((and (equal? (min ld l1 l2 l3) ld) (not(equal? ld 7)))(begin (set! winner1 0) (set! dealerScore 0)(send winner1Message set-label (format "¡¡El ganador es el dealer!!"))))
       ((and (equal? (min ld l1 l2 l3) l1) (not(equal? l1 7)))(begin (set! winner1 1) (set! player1Score 0)(send winner1Message set-label (format "¡¡El ganador es el jugador número ~a!!"1))))
       ((and (equal? (min ld l1 l2 l3) l2) (not(equal? l2 7)))(begin (set! winner1 2) (set! player2Score 0)(send winner1Message set-label (format "¡¡El ganador es el jugador número ~a!!"2))))
       ((and (equal? (min ld l1 l2 l3) l3) (not(equal? l3 7)))(begin (set! winner1 3) (set! player3Score 0)(send winner1Message set-label (format "¡¡El ganador es el jugador número ~a!!"3))))))

;***********************************************************Función pa ver quien ganó**************************************************************************
;Esta función obtiene solamente los puntajes que cumplen con las condiciones para ganar 
(define (winner scoreD score1 score2 score3)
  ;Para cada jugador se verifica si tiene el puntaje más alto, además se cambia el valor del puntaje de ese jugador para
  ;que no interfiera con las demás posiciones, se porporciona el id del jugador ganador
  (cond((equal? (max scoreD score1 score2 score3) scoreD)(begin (set! dealerScore 0) (send winner2Message set-label (format "¡¡El segundo lugar es el dealer!!"))))
       ((equal? (max scoreD score1 score2 score3) score1)(begin (set! player1Score 0) (send winner2Message set-label (format "¡¡El segundo lugar es el jugador número ~a!!" 1))))
       ((equal? (max scoreD score1 score2 score3) score2)(begin (set! player2Score 0) (send winner2Message set-label (format "¡¡El segundo lugar es el jugador número ~a!!" 2))))
       (else(begin (set! player3Score 0) (send winner2Message set-label (format "¡¡El segundo lugar es el jugador número ~a!!" 3))))))

;***********************************************************Funcion para determinar los puestos del podio***********************************************
(define (gameOver dScore p1Score p2Score p3Score lond lon1 lon2 lon3)
  (smallDeck dScore p1Score p2Score p3Score lond lon1 lon2 lon3)
  (rmvIllegal winner1)
  (send winnerWindow show #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Winner Window;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define winnerWindow (new frame% [label "¡¡Ganador!!"]
                        [width 550]
                        [height 300]))

(define winnerPanel (new horizontal-panel% [parent winnerWindow]))
(define leftwinnerPanel (new vertical-panel% [parent winnerPanel]
                       [min-width 150]
                       [min-height 300]
                       ))

(define fin1 (new message% [parent leftwinnerPanel]
                           [label " "]))

(define fin2 (new message% [parent leftwinnerPanel]
                           [label " "]))

(define fin3 (new message% [parent leftwinnerPanel]
                           [label " "]))
(define winner1Message (new message% [parent leftwinnerPanel]
;Para el primer lugar se muestra el ganador utilizando la verificación del JackBlack 
                           [label (format "¡¡El ganador es el jugador número ~a!!" msg1)]))
(define winner2Message (new message% [parent leftwinnerPanel]
;Para el segundo y tercer lugar se verifica de forma normal
                            [label (format "¡¡En segundo lugar está el jugador número ~a!!" msg2)]))
(define fin4 (new message% [parent leftwinnerPanel]
                           [label " "]))

(define fin5 (new message% [parent leftwinnerPanel]
                           [label " "]))
(define fin6 (new message% [parent leftwinnerPanel]
                           [label " "]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;logic: creacion de las 4 decks iniciales;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initializeDecks); uso de el contador de control para asignar 2 cartas a cada jugador, se llama recursivamente con apoyo de deckAuxiliar
  (cond ((< cont 3) (deckAuxiliar (cons (car randomDeck) dealerDeck)))
        ((< cont 5) (deckAuxiliar (cons (car randomDeck) player1Deck)))
        ((< cont 7) (deckAuxiliar (cons (car randomDeck) player2Deck)))
        (else (deckAuxiliar (cons (car randomDeck) player3Deck)))
  )
)

(define (deckAuxiliar newDeck) ;funcion usada para ir anadiendo cada carta a la baraja de los jugadores, ademas se va anadiendo el respectivo puntaje
  (cond ((< cont 3) (begin 
                      (set! dealerDeck newDeck)))
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

  (cond 
    ((< cont 9) (initializeDecks));entra aqui si no ha terminado de agregar las 8 cartas (llamada recursiva)
    (else 
      (begin
        (set! dealerScore (startDealerScore dealerDeck 0)) ;obteniendo el puntaje del dealer desde la logica principal
        (printAux)
      )
    )
  )

  (cond
    ((= players 1)(begin ;si solo hay un jugador los otros dos jugadores se colocan con 0 pts y sin baraja
      (set! player2Score 0)
      (set! player2Deck '())
      (set! player3Score 0)
      (set! player3Deck '())
    ))

    ((= players 2)(begin ;si hay dos jugadores el jugador 3 se pone con puntuacion 0 y sin baraja
      (set! player3Score 0)
      (set! player3Deck '())
    ))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;logic: funcion para realizar las jugadas del dealer;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dealerTurn)
  (cond 
    ((= dealerStand 1) (gameOver dealerScore player1Score player2Score player3Score (longitud dealerDeck) (longitud player1Deck) (longitud player2Deck) (longitud player3Deck))) ;si el dealer se planta significa que la partida termino
    (else ;primero se debe añadir la carta al deck (realizar la jugada como tal), luego se debe llamar recursivamente a dealerTurn para ver si se juega de nuevo
      (begin
       (set! dealerStand (dealerMove dealerScore))
       (cond 
        ((= dealerStand 0)(dealerUpdate)) ;condicion utilizada para que el dealer solo juegue cuando no esta plantado
       )
       (dealerTurn);llamada recursiva para realizar todas las jugadas
      )
    )
  )
  
  (send upperPanel refresh) ;refrescando el canvas para reflejar los cambios
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;logic: logica para jugada del dealer despues de la primera vez;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dealerUpdate) ;funcion para ejecutar la logica del dealer
  (set! dealerDeck (cons (car randomDeck) dealerDeck)) ;modificando la baraja del dealer
  (set! dealerScore (+ dealerScore (scoreSelector (car randomDeck) dealerScore))) ;modificando el puntaje del dealer, utilizando la funcion que elige los puntajes correspondientes en la logica
  (set! randomDeck (cdr randomDeck)) ;quitando del deck principal la carta ya asignada\
  (set! dealerStand (dealerMove dealerScore))
  (send upperPanel refresh) ; Refrescar el canvas
  (printAux)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Debugging: funcion para mostrar valores durante la ejecucion;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (printAux)
  (newline)
  (print dealerDeck)
  (print player1Deck)
  (print player2Deck)
  (print player3Deck)
  (newline)
  (displayln dealerScore)
  (displayln player1Score)
  (displayln player2Score)
  (displayln player3Score)
  )
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; logic: tratamiento de As ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sumA id)
  ;Se verifica si el jugador que se plantó posee algún A 
  ;Si no posee ningún As entonces se suma el puntaje actual de los As a el puntaje del id correspondiente
  (cond((equal? tempA 0)(begin (totalA tempId)(checkStand)))
       ;Para cada iteración se decrementa la cantidad de As que tiene el jugador y se muestra la ventana
       ;al jugador para que seleccione el valor que le desea dar a su carta Ai
       ((equal? id 0)(begin (set! tempA (- tempA 1))(send aWindow show #t)))
       ((equal? id 1)(begin (set! tempA (- tempA 1))(send aWindow show #t)))
       ((equal? id 2)(begin (set! tempA (- tempA 1))(send aWindow show #t)))
       (else(begin (set! tempA (- tempA 1))(send aWindow show #t)))))
;Una vez que el usuario seleccionó los valores de sus cartas As 
(define (totalA id)
  (cond((equal? id 0)(begin (set! dealerScore (+ dealerScore valA)) (set! valA 0)))
       ((equal? id 1)(begin (set! player1Score (+ player1Score valA)) (set! valA 0)))
       ((equal? id 2)(begin (set! player2Score (+ player2Score valA)) (set! valA 0)))
       (else(begin (set! player3Score (+ player3Score valA)) (set! valA 0)))
  )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: ventana valores de A ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;logic: Verificación del puntaje total;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (veriChara character id) ;verifica el valor de un caracter o numero
  (cond ((equal? 'J character)10) ;J asigna 10 de puntaje
        ((equal? 'Q character)10) ;Q asigna 10 de puntaje
        ((equal? 'K character)10) ;K asigna 10 de puntaje
        ((equal? 'A character)(begin ;A asigna 1 o 11 de puntaje, dependiendo del jugador
                                    (incA id) 0))
        (else character))) ;cualquier numero asigna su mismo valor

(define (incA id)
  (cond((equal? id 0)(set! Anumd (+ Anumd 1)))
       ((equal? id 1)(set! Anum1 (+ Anum1 1)))
       ((equal? id 2)(set! Anum2 (+ Anum2 1)))
       (else(set! Anum3 (+ Anum3 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;show the selec frame;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send selecWindow show #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: Pantalla principal;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mainWindow (new frame% [label "BlaCEJack"]
                        [width 1050]
                        [height 670]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: Disposicion de la pantalla;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  
  (define dc (send canvas get-dc)) ;obteniendo el entorno
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

(define (paint-callback0 canvas dc) ;canvas de dealer
  (send dc set-background (make-object color% 58 170 63)) ;color verde
  (send dc clear)
  (drawCards canvas dealerDeck dealerCoords) ;llamando a la funcion para imprimir las cartas del dealer

  (cond
    ((zero? hiddenCard) ;si la varialbe hiddenCard es 0, se muestra un rectangulo que oculta la primera carta (solo sucede en la primera jugada del dealer)
      (begin 
        (send dc draw-rectangle 190 16.66 96 112)
        (send dc draw-text (symbol->string '?) 195 21.66)
        (send dc draw-text (symbol->string '?) 232 58.66)
      )
    )
  )
  (set! hiddenCard 1) ;cambiando el valor de hiddenCard para que no se vuelva a mostrar el rectangulo para ocultar
 )

(define (paint-callback1 canvas dc) ;canvas de jugador 1
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  (drawCards canvas player1Deck playerCoords) ;imprimiendo las cartas del jugador 1
 )

(define (paint-callback2 canvas dc) ;canvas de jugador 2
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  (cond
    ((>= players 2)(drawCards canvas player2Deck playerCoords)) ;imprimiendo las cartas del jugador 2, si y solo si la cantidad de jugadores es mayor o igual a 2
  )
 )

(define (paint-callback3 canvas dc) ;canvas de jugador 3
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  (cond
    ((>= players 3)(drawCards canvas player3Deck playerCoords)) ;imprimiendo las cartas del jugador 3, si y solo si la cantidad de jugadores es mayor o igual a 3
  )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;logic: Buttons for decisions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hit1 (new button% [parent player1Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                    (cond
                                      ((zero? player1Stand)(hit 1));si el jugador no esta plantado, pide carta
                                    ))]))

(define stand1 (new button% [parent player1Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                    (cond
                                      ((zero? player1Stand) ;verifica si el jugador esta plantado
                                        (begin
                                          (stand1ButtonCallback); si el jugador no esta plantado cambia la var de control para plantarse
                                        ))
                                      (else (send stand1 set-label "Plantado")) ;si la computadora planto al jugador se actualiza el label
                                    )
                                    )]))

(define hit2 (new button% [parent player2Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                    (cond
                                      ((>= players 2);checando que el jugador 2 este jugando
                                        (cond
                                          ((zero? player2Stand)(hit 2));si el jugador no esta plantado, pide carta
                                        ))
                                    ))]))

(define stand2 (new button% [parent player2Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                    (cond
                                      ((>= players 2);checando que el jugador 2 este jugando
                                        (cond
                                          ((zero? player2Stand) ;verifica si el jugador esta plantado
                                            (begin
                                              (stand2ButtonCallback); si el jugador no esta plantado cambia la var de control para plantarse
                                            ))
                                          (else (send stand2 set-label "Plantado")) ;si la computadora planto al jugador se actualiza el label
                                        ))
                                    )
                                    )]))

(define hit3 (new button% [parent player3Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                    (cond
                                      ((>= players 3) ;checando que el jugador 3 este jugando
                                        (cond
                                          ((zero? player3Stand)(hit 3)) ;si el jugador no esta plantado, pide carta
                                        ))
                                    ))]))

(define stand3 (new button% [parent player3Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                    (cond
                                      ((>= players 3);checando que el jugador 3 este jugando
                                        (cond
                                          ((zero? player3Stand) ;verifica si el jugador esta plantado
                                            (begin
                                              (stand3ButtonCallback); si el jugador no esta plantado cambia la var de control para plantarse
                                            ))
                                          (else (send stand3 set-label "Plantado"))
                                        ))
                                    )
                                    )]))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;logic: checa que todos esten plantados;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (checkStand)
  (cond
    ((= players 3) ; Caso para 3 jugadores
     (cond
        ((= player1Stand 1)
          (cond
            ((= player2Stand 1)
              (cond
              ((= player3Stand 1) 
                (cond 
                  ((zero? dealerPlaying)
                    (begin
                      (set! dealerPlaying 1)
                      (dealerTurn) ; si todos estan plantados juega el dealer
                    )
                  )
                )
              ) 
              )
            )
          )
        )
      )
    )

    ((= players 2) ; Caso para 2 jugadores
      (cond
        ((= player1Stand 1)
          (cond
            ((= player2Stand 1) 
              (cond 
                ((zero? dealerPlaying)
                  (begin
                    (set! dealerPlaying 1)
                    (dealerTurn) ; si los 2 jugadores estan plantados juega el dealer
                  )
                )  
              )
            )
          )
        )
      )
    )
    
    ((= players 1) ; Caso para 1 jugador
      (cond
        ((= player1Stand 1) 
          (cond 
            ((zero? dealerPlaying)
            (begin
              (set! dealerPlaying 1)
              (dealerTurn) ; si el jugador 1 esta plantado juega el dealer
            )
            )
          )
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;Logic: logica para los botones de plantarse;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stand1ButtonCallback)
  (send stand1 set-label "Plantado") ;si se planta de manera voluntaria se cambia label
  (set! tempA Anum1)
  (set! Anum1 0)
  (set! tempId 1)
  (set! player1Stand 1) ;se coloca el jugador 1 como plantado
  (sumA 1)
)
(define (stand2ButtonCallback)
  (send stand2 set-label "Plantado") ;si se planta de manera voluntaria se cambia label
  (set! tempA Anum2)
  (set! Anum2 0)
  (set! tempId 2)
  (set! player2Stand 1) ;se coloca el jugador 2 como plantado
  (sumA 2)
)
(define (stand3ButtonCallback)
  (send stand3 set-label "Plantado") ;si se planta de manera voluntaria se cambia label
  (set! tempA Anum3)
  (set! Anum3 0)
  (set! tempId 3)
  (set! player3Stand 1) ;se coloca el jugador 3 como plantado
  (sumA 3)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;Logic: logica para los botones de pedir;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hit playerId) ;funcion para anadir una carta nueva a la baraja de cada jugador
  (cond 
    ((equal? playerId 0) (begin ;si se trata del jugador 1, se anade la carta y su valor en puntaje 
        (set! dealerDeck (cons (car randomDeck) dealerDeck))
        (set! dealerScore (+ dealerScore (scoreSelector (car randomDeck) dealerScore)))
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

  (cond
    ((> player1Score 21) 
      (begin
        (stand1ButtonCallback); ;si el jugador una vez jugo se paso de 21, se planta automaticamente
      )
    )
  )
  (cond
    ((> player2Score 21) 
      (begin
        (stand2ButtonCallback); ;si el jugador una vez jugo se paso de 21, se planta automaticamente
      )
    ) 
  )
  (cond
    ((> player3Score 21) 
      (begin
        (stand3ButtonCallback); ;si el jugador una vez jugo se paso de 21, se planta automaticamente
      )
    )   
  )
  (printAux)
)

