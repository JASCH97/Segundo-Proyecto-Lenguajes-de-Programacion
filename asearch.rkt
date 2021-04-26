#lang racket

; Se realizan los imports necesarios
(require rackunit
         math/matrix
         racket/unit
         racket/match
         racket/list
         data/heap
         2htdp/image
         racket/runtime-path
         racket/format)

(define-signature grafo^
  (nodo? vecino? nodo-vecinos vecino-inicial vecino-costo vecino-destino))

; Función para crear la matriz con obstáculos
(define (crear-matriz n)
  (build-matrix n n (λ (x y) (random 3))))

; Se definen los structs a utilizar para el grafo
(struct map-nodo (matriz x y) #:transparent)
(struct map-vecino (ini dx dy dest))

(define-unit map@
  (import) (export grafo^)
 
  (define nodo? map-nodo?)
  (define vecino? map-vecino?)
  (define vecino-inicial map-vecino-ini)
  (define vecino-destino map-vecino-dest)
 
  (define (vecino-costo costo)
    (match-define (map-vecino _ _ _ (map-nodo matriz x y)) costo)
    (match (matrix-ref matriz x y)
        [0  1]
        [1  1]
        [2  50]))

  (define (nodo-vecinos n)
    (match-define (map-nodo matriz x y) n)
    (append*
        (for*/list ([dx (in-list '(1 0 -1))]
               [dy (in-list '(1 0 -1))]
               #:when (and (not (and (zero? dx) (zero? dy))) 
                      (or (zero? dx) (zero? dy))))
                      
        (cond
            [(and (<= 0 (+ dx x) (sub1 (matrix-num-cols matriz)))
             (<= 0 (+ dy y) (sub1 (matrix-num-rows matriz))))
             (define dest (map-nodo matriz (+ dx x) (+ dy y)))
             (list (map-vecino n dx dy dest))]
            [else empty])))))

(define (a-search graph@ inicio costo)

    (define-values/invoke-unit graph@ (import) (export grafo^))
  
    (define contador 0)
  
    (define nodo-camino (make-hash))
  
    (define nodo-costo-camino (make-hash))
 
    (hash-set! nodo-camino      inicio empty)
  
    (hash-set! nodo-costo-camino inicio 0)

    (define (nodo-costo-camino-aprox n)
        (+ (costo n) (hash-ref nodo-costo-camino n)))
  
    (define (node-cmp x y)
        (<= (nodo-costo-camino-aprox x) (nodo-costo-camino-aprox y)))
  
    (define open-list (make-heap node-cmp))
  
    (heap-add! open-list inicio)
 
    (begin0
        (let/ec esc
            (for ([x (in-heap/consume! open-list)])

            (set! contador (add1 contador))
        
            (define h-x (costo x))
        
            (define recorrido-x (hash-ref nodo-camino x))
 
            (when (zero? h-x)
                (esc (reverse recorrido-x)))
 
            (define g-x (hash-ref nodo-costo-camino x))
        
            (for ([x->y (in-list (nodo-vecinos x))])

                (define y (vecino-destino x->y))

                (define nuevo-recorrido (+ g-x (vecino-costo x->y)))

                (define recorrido-actual
                    (hash-ref nodo-costo-camino y +inf.0))

                (when (and (< nuevo-recorrido recorrido-actual) (< nuevo-recorrido 30))

                    (hash-set! nodo-costo-camino y nuevo-recorrido)

                    (hash-set! nodo-camino y (cons x->y recorrido-x))

                    (heap-add! open-list y))
            )
        )
      '()
          )))

(define ((nodo-manhattan-distance x-dest y-dest) n)
  (match-define (map-nodo matriz x y) n)

  (+ (abs (- x x-dest))
     (abs (- y y-dest)))
    )

(define N 13)

; Convierte los datos del struct en una lista para generar el camino en la GUI
(define (obtener-camino camino)
  (if (null? camino)
      (display "El camino está vacío")
      (obtener-camino-aux camino '()))
  )

(define (obtener-camino-aux camino lista)
  
  (cond
    [(null? camino) lista]
    [else (obtener-camino-aux (cdr camino) (append lista (list (convertirFilasColumnas (string->number (string (string-ref (~a (map-vecino-dest (list-ref camino 0))) (- (string-length (~a(map-vecino-dest (list-ref camino 0)))) 4)))) (string->number (string (string-ref (~a (map-vecino-dest (list-ref camino 0))) (- (string-length (~a(map-vecino-dest (list-ref camino 0)))) 2))))))))]
    )
  )

;Funcion que convierte la posicion i j de una matriz en la misma posicion pero con un numero entero. Ej: Matriz(1,0) de una matriz 13x13 devolveria 14
(define (convertirFilasColumnas i j)
  (cond
    [(> i 12) (display "Rango de filas invalido")]
    [(> j 12) (display "Rango de columnas invalido")]
    [(equal? i 0) j]
    [(equal? i 1) (+ j 13)]
    [(equal? i 2) (+ j 26)]
    [(equal? i 3) (+ j 39)]
    [(equal? i 4) (+ j 52)]
    [(equal? i 5) (+ j 65)]
    [(equal? i 6) (+ j 78)]
    [(equal? i 7) (+ j 91)]
    [(equal? i 8) (+ j 104)]
    [(equal? i 9) (+ j 117)]
    [(equal? i 10) (+ j 130)]
    [(equal? i 11) (+ j 143)]
    [else (+ j 156)]))


(define map-scale 15)

(define (type-color ty)
  (match ty
    [0 "yellow"]
    [1 "yellow"]
    [2 "red"]))

(define (cell-square ty)
  (square map-scale "solid" (type-color ty)))

(define (row-image matriz row)
  (apply beside
         (for/list ([col (in-range (matrix-num-cols matriz))])
           (cell-square (matrix-ref matriz row col)))))

(define (map-image matriz)
  (apply above
         (for/list ([row (in-range (matrix-num-rows matriz))])
           (row-image matriz row))))

(define (edge-image-on e i)
  (match-define (map-vecino (map-nodo _ sx sy) _ _ (map-nodo _ dx dy)) e)
  (add-line i
            (* (+ sy 0.5) map-scale) (* (+ sx 0.5) map-scale)
            (* (+ dy 0.5) map-scale) (* (+ dx 0.5) map-scale)
            "black"))

(define (path-image matriz path)
  (foldr edge-image-on (map-image matriz) path))

(define-runtime-path map-image.png "pictures/astar.png")
 
(define-runtime-path path-image.png "pictures/capture.png")


(define (main filaIni colIni)
  
  (define tablero (crear-matriz N))
  
  (define recorrido (a-search map@ (map-nodo tablero 0 0) (nodo-manhattan-distance 0 5)))
  
  (if (null? recorrido)
      (display "No existe un camino desde ese punto hasta la meta, por favor vuelva a ejecutar el programa con un punto diferente\n")
      (display (obtener-camino recorrido))
      )
        (save-image (map-image tablero) map-image.png)
        (save-image (path-image tablero recorrido) path-image.png)
  )


