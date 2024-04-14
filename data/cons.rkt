#lang racket

(define playerCoords'(((15 40) (96 112))
                       ((126 40) (96 112))
                       ((237 40) (96 112))
                       ((15 184) (96 112))
                       ((126 184) (96 112))
                       ((237 184) (96 112))))

(define dealerCoords'(((190 16.66) (96 112))
                       ((476 16.66) (96 112))
                       ((762 16.66) (96 112))
                       ((190 145.32) (96 112))
                       ((476 145.32) (96 112))
                       ((762 145.32) (96 112))))
                       
(define (getCoords id) ;funcion que devuelve las coordenadas dependiendo del ID
    (cond
        ((= id 0) dealerCoords)
        ((= id 1) playerCoords)
    )
)

(provide getCoords)
