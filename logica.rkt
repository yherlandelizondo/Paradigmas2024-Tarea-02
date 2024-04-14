#lang racket

(define (dealerMove score) ;funcion que determina si el dealer se planta o no basado en el score del mismo, devuelve booleanos dependiendo de la situacion
    (cond 
        ((>= score 17) 1) ;si el puntaje es mayor a 17 devuelve 1 (se planta)
        (else 0)) ;si el puntaje es menor a 17 se devuelve 0 (juega)
)

(define (startDealerScore deck score) ;funcion para asignar el puntaje inicial del dealer
    (cond
        ((null? deck) score) ;caso base, ya se recorrio la baraja
        (else (startDealerScore (cdr deck) (+ score (scoreSelector (car deck) score)))) ;llamada recursiva, se desarma la baraja de manera que se va analizando carta por carta
    )
)

(define (scoreSelector card score) ;analizando la carta y asignado su valor al puntaje, usada para determinar el valor de una nueva carta
    (cond ((equal? 'J (car card)) 10)
        ((equal? 'Q (car card)) 10)
        ((equal? 'K (car card)) 10)
        ((equal? 'A (car card))(verifyScore score)) ;si se trata de un As se llama a la funcion auxiliar
        (else (car card)))
)

(define (verifyScore score) ;funcion que analiza el puntaje actual del jugador para asignar un valor al As
    (cond 
        ((<= (+ score 11) 21) 11) ;si la suma es menor a 21, el As vale 11
        (else 1) ;si la suma excede los 21, el As vale 1
    )
)

(provide startDealerScore scoreSelector dealerMove) ;haciendo provide de manera que se pueda acceder a estas funciones desde la interfaz