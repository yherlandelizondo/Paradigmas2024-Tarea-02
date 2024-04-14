;Tarea 2
;Yherland Elizondo Cordero
;Gabriela Quesada Sancho
;Jafet Dixon Solano

#lang racket
(require racket/gui/base)
(require math)
(require "lib/randomDeck.rkt")
(require "lib/readWrite.rkt")
(require "data/cons.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Variables y constantes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define players 0)
(define randomDeck (randomList))
(define dealerDeck '())
(define player1Deck '())
(define player2Deck '())
(define player3Deck '())
(define cont 1)
(define dealerScore 5)
(define player1Score 18)
(define player2Score 21)
(define player3Score 5)
(define test3 2)
(define Anumd 0)
(define Anum1 0)
(define Anum2 0)
(define Anum3 0)
(define tempA 0)
(define valA 0)
(define tempId 0)
(define test1 78)
(define test2 47)

;;;;Cordenadas para las cartas de los jugadores / dealer;;;;;;;
(define dealerCoords (getCoords 0))
(define playerCoords (getCoords 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;funciones especifica;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print list) ;funcion para imprimir una lista
  (for-each display list)
  (newline))

;*************************************************************************************longitud*******************************************************
(define (longitud list);La función longitud se encarga de  devolver la longitud de una lista
  (cond ((null? list)0)
  (else (+ 1 (longitud (cdr list))))))
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

;no estoy segura de lo de la variable******************************************************************************************************************************
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
  
  (set! cont (+ cont 1))
  (set! randomDeck (cdr randomDeck))
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

    (define xCoord (caar coordList)) ;extrayendo el valor de la coord x de la carta
    (define yCoord (cadar coordList)) ;extrayendo el valor de la coord y de la carta
    (define width (caadr coordList)) ;extrayendo la anchura de la carta
    (define height (cadadr coordList)) ;extrayendo la altura de la carta

    (define suit (symbol->string (cadr card))) ;extrayendo el simbolo de la carta
    (define value (car card)) ;extrayendo el valor de la carta
  

    (send dc draw-rectangle xCoord yCoord width height) ;dibujando la carta con dimensiones y coords extraidas

    (define textX (+ xCoord 40)) ;definiendo una posición para colocar el texto de la carta en x
    (define textY (+ yCoord 50)) ;definiendo una posición para colocar el texto de la carta en y

    (send dc draw-text suit textX textY) ;dibujando el simbolo de la carta
    (send dc draw-text value (+ xCoord 5) (+ yCoord 20)) ;dibujando el valor de la carta
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics: canvas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;functions to refresh each canvas

(define (paint-callback0 canvas dc)
  (send dc set-background (make-object color% 58 170 63)) ;color verde
  (send dc clear)
 )

(define (paint-callback1 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  ;(drawCards canvas player1Deck playerCoords)
  
 )

(define (paint-callback2 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  
 )

(define (paint-callback3 canvas dc)
  (send dc set-background (make-object color% 58 170 63))
  (send dc clear)
  ;(drawCards canvas)
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
                                     (test event))]))
(define(test event)
  (displayln test1)
  (smallDeck dealerScore player1Score player2Score player3Score 2 3 4 5)
  (rmvIllegal test3)
  (send winnerWindow show #t))
(define stand1 (new button% [parent player1Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (stand1ButtonCallback event))]))

(define hit2 (new button% [parent player2Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                     (twoPButtonCallback event))]))

(define stand2 (new button% [parent player2Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (stand2ButtonCallback event))]))

(define hit3 (new button% [parent player3Panel]
                         [label "Pedir"]
                         [callback (lambda (button event)
                                     (twoPButtonCallback event))]))

(define stand3 (new button% [parent player3Panel]
                         [label "Plantarse"]
                         [callback (lambda (button event)
                                     (stand3ButtonCallback event))]))  

;****************************************************Llamadas de plantarse**************************************************************************************

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

;***********************************************************Verificar si es mayor a 21*************************************************************************

(define(rmvIllegal idWinner1); La función se utiliza para verificar si algún jugador posee un puntaje mayor a 21 ya que perdería el juego
  ;Se identifica el id de los jugadores que perdieron por tener un puntaje mayor a 21 y se colocan sus valores en 0, además se identifica
  ;y se excluye el ganador ya que este se presenta al verificar el JackBlack
    (cond ((or (> dealerScore 21) (equal? 0 idWinner1))(begin(set! dealerScore 0)(display "holi"))))
    (cond ((or (> player1Score 21) (equal? 1 idWinner1))(begin (display player1Score) (set! player1Score 0))))
    (cond ((or (> player2Score 21) (equal? 2 idWinner1))(begin (set! player2Score 0)(display "hola"))))
    (cond ((or (> player3Score 21) (equal? 3 idWinner1))(begin (set! player3Score 0)(display "holo"))))
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
  (cond((and (equal? (min ld l1 l2 l3) ld) (not(= ld 7)))(begin (set! dealerScore 0)(send winner1Message set-label (format "¡¡El ganador es el dealer!!"))));Para que cumpla los requisitos tiene que tener la baraja con menor logitud y no haberse descartado en smallDeck
       ((and (equal? (min ld l1 l2 l3) l1) (not(= l1 7)))(begin (set! player1Score 0)(send winner1Message set-label (format "¡¡El ganador es el jugador número ~a!!"1))))
       ((and (equal? (min ld l1 l2 l3) l2) (not(= l2 7)))(begin (set! player2Score 0)(send winner1Message set-label (format "¡¡El ganador es el jugador número ~a!!"2))))
       ((and (equal? (min ld l1 l2 l3) l3) (not(= l3 7)))(begin (set! player3Score 0)(send winner1Message set-label (format "¡¡El ganador es el jugador número ~a!!"3))))))
       ;(else(rmvIllegal 4))))
;La función devuelve el id del jugador ganador, de no existir ninguno con 21 puntos retorna un 4 que indica que no hay un ganador aún

;***********************************************************Función pa ver quien ganó**************************************************************************
;Esta función obtiene solamente los puntajes que cumplen con las condiciones para ganar 
(define (winner scoreD score1 score2 score3)
  ;Para cada jugador se verifica si tiene el puntaje más alto, además se cambia el valor del puntaje de ese jugador para
  ;que no interfiera con las demás posiciones, se porporciona el id del jugador ganador
  (cond((equal? (max scoreD score1 score2 score3) scoreD)(begin (set! dealerScore 0) (send winner2Message set-label (format "¡¡El segundo lugar es el dealer!!"))))
       ((equal? (max scoreD score1 score2 score3) score1)(begin (set! player1Score 0) (send winner2Message set-label (format "¡¡El segundo lugar es el jugador número ~a!!" 1))))
       ((equal? (max scoreD score1 score2 score3) score2)(begin (set! player2Score 0) (send winner2Message set-label (format "¡¡El segundo lugar es el jugador número ~a!!" 2))))
       (else(begin (set! player3Score 0) (send winner2Message set-label (format "¡¡El segundo lugar es el jugador número ~a!!" 3))))))
;**********************************************************Ventana del ganador*********************************************************************************
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
                           [label (format "¡¡El ganador es el jugador número ~a!!" test1)]));(smallDeck dealerScore player1Score player2Score player3Score (longitud dealerDeck) (longitud player1Deck) (longitud player2Deck) (longitud player3Deck)))]))
(define winner2Message (new message% [parent leftwinnerPanel]
;Para el segundo y tercer lugar se verifica de forma normal
                            [label (format "¡¡En segundo lugar está el jugador número ~a!!" test2)]));(rmvIllegal (smallDeck dealerScore player1Score player2Score player3Score (longitud dealerDeck) (longitud player1Deck) (longitud player2Deck) (longitud player3Deck))))]))
(define fin4 (new message% [parent leftwinnerPanel]
                           [label " "]))

(define fin5 (new message% [parent leftwinnerPanel]
                           [label " "]))
(define fin6 (new message% [parent leftwinnerPanel]
                           [label " "]))
