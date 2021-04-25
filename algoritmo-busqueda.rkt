#lang racket

; Se realizan los imports necesarios
(require rackunit
         math/matrix
         racket/list)

(define-signature grafo^
  (nodo? vecino? nodo-vecinos vecino-inicial vecino-costo vecino-destino))

; Función para crear la matriz con obstáculos
(define (crear-matriz n)
  (build-matrix n n (λ (x y) (random 2))))

; Se definen los structs a utilizar
(struct map-nodo (matriz x y) #:transparent)
(struct map-vecino (ini dx dy dest))

(define ((nodo-manhattan-distance x-dest y-dest) n)
  (match-define (map-nodo matriz x y) n)

  (+ (abs (- x x-dest))
     (abs (- y y-dest)))
    )

(define N 10)

(define tablero (crear-matriz N))


