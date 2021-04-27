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
  (build-matrix n n (λ (x y) (random 4))))

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
        [2  1]
        [3  50]))

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
    [else (obtener-camino-aux (cdr camino) (append lista (list (convertirFilasColumnas (string->number (list-ref (string-split (string-trim (substring (~a (map-vecino-dest (list-ref camino 0))) 376 (- (string-length (~a (map-vecino-dest (list-ref camino 0)))) 1)))) 0)) (string->number (list-ref (string-split (string-trim (substring (~a (map-vecino-dest (list-ref camino 0))) 376 (- (string-length (~a (map-vecino-dest (list-ref camino 0)))) 1)))) 1))))))]
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

(define (obtenerIdePos pos)
  (cond
    [(< pos 13) 0]
    [(< pos 26) 1]
    [(< pos 39) 2]
    [(< pos 52) 3]
    [(< pos 65) 4]
    [(< pos 78) 5]
    [(< pos 91) 6]
    [(< pos 104) 7]
    [(< pos 117) 8]
    [(< pos 130) 9]
    [(< pos 143) 10]
    [(< pos 156) 11]
    [else 12]))

;Prueba de la funcion obtenerIdePos
;(obtenerIdePos 165)

(define (obtenerJdePos pos)
  (cond
    [(< pos 13) pos]
    [(< pos 26) (- pos 13)]
    [(< pos 39) (- pos 26)]
    [(< pos 52) (- pos 39)]
    [(< pos 65) (- pos 52)]
    [(< pos 78) (- pos 65)]
    [(< pos 91) (- pos 78)]
    [(< pos 104) (- pos 91)]
    [(< pos 117) (- pos 104)]
    [(< pos 130) (- pos 117)]
    [(< pos 143) (- pos 130)]
    [(< pos 156) (- pos 143)]
    [else (- pos 156)]))

;Prueba de la funcion obtenerJdePos
;(obtenerJdePos 13)


(define tablero (crear-matriz N))
  




;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------;

(require picturing-programs)

;(define obstacle1 )

;(define obstacle2 )

;(define meta )

;(define imgFrente )

;(define imgEspalda )

;(define imgIzq )

;(define imgDer )

;(define imgBackground )

(define ordenMostrarPj (list imgFrente))    ;lista que va a contener el orden en el que se debe mostrar el perfil del personaje. De primero siempre debe estar la imagen frontal!!


(define obstaculosPos (list )) ;lista que contiene las posiciones de los obstaculos

(define solucion (list )) ;lista que contiene la implementacion de la funcion mostrarSolucion. Aqui se utiliza la ruta mas corta y la lista con los movimientos que va a hacer el personaje



;Funcion que toma la lista con el camino mas corto y guarda el orden en el que se debe mover el personaje en la lista ordenMostrarPj
(define (movimientoPersonaje path)
  (if (equal? (length path) 1)
      (display "orden de movimientos completado \n") ;Cuando tenga lista la funcion mostrar solucion se prodria llamar desde aqui en lugar de mostrar el msj!!
      (movimientoPersonajeAux (car path) (car(cdr path)) path)))

(define (movimientoPersonajeAux actual sig path)
  (cond
    [(equal? (sub1 actual) sig) (set! ordenMostrarPj (append ordenMostrarPj (list imgIzq))) (movimientoPersonaje(cdr path))]
    [(equal? (add1 actual) sig) (set! ordenMostrarPj (append ordenMostrarPj (list imgDer))) (movimientoPersonaje(cdr path))]
    [(equal? (- actual 5) sig) (set! ordenMostrarPj (append ordenMostrarPj (list imgEspalda))) (movimientoPersonaje(cdr path))] ;El 5 es por el tamanio de la matriz. Cambiar cuando se sepa el fijo.
    [else
     (set! ordenMostrarPj (append ordenMostrarPj (list imgFrente))) (movimientoPersonaje(cdr path))]))

;Prueba movimientoPersonaje:
;(movimientoPersonaje bestPath)
;ordenMostrarPj



;Notas y pruebas
;en underlay/xy -> la x indica cuantos espacios a la derecha, la y cuantos hacia abajo
;(define h (list imgBackground (underlay/xy imgBackground 20 20 imgFrente) imgBackground (underlay/xy imgBackground 71 20 imgDer) imgBackground (underlay/xy imgBackground 71 71 imgFrente)))
;(run-movie 1 h) ;x derecha y abajo

;(underlay/xy imgBackground 7 17 imgFrente)
#|
(set! imgBackground (underlay/xy imgBackground 20 20 obstacle1)) ;con set! se puede ir cambiando el fondo para ponerle los obstaculos
(set! imgBackground (underlay/xy imgBackground 71 20 imgDer)) ;espacio entre obstaculos exacto 50. Para ver mejor visualmente si estan juntos dejar 51!!!!!!
(set! imgBackground(underlay/xy imgBackground 122 20 obstacle1))
(set! imgBackground(underlay/xy imgBackground 173 20 obstacle2))
(set! imgBackground(underlay/xy imgBackground 224 20 obstacle1))
(set! imgBackground(underlay/xy imgBackground 275 20 obstacle2))
(set! imgBackground(underlay/xy imgBackground 326 20 obstacle1))
(set! imgBackground(underlay/xy imgBackground 377 20 obstacle2))
(set! imgBackground(underlay/xy imgBackground 428 20 obstacle1))
(set! imgBackground(underlay/xy imgBackground 479 20 obstacle2))
(set! imgBackground(underlay/xy imgBackground 530 20 obstacle1))
(set! imgBackground(underlay/xy imgBackground 581 20 obstacle2))
(set! imgBackground(underlay/xy imgBackground 632 20 obstacle1))

(set! imgBackground(underlay/xy imgBackground 632 71 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 122 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 173 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 224 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 275 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 326 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 377 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 428 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 479 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 530 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 581 obstacle1))
(set! imgBackground(underlay/xy imgBackground 632 632 obstacle1))
(display imgBackground)
|#


;Funcion que devuelve una posicion random del laberinto donde no haya obstaculos
(define (colocarMeta listaObstaculos i nRandom)
  (cond
    [(equal? i (length listaObstaculos)) nRandom]
    [(equal? (list-ref listaObstaculos i) nRandom) (colocarMeta listaObstaculos 0 (random 169))]
    [else (colocarMeta listaObstaculos (+ i 1) nRandom)]))

(define posMeta (colocarMeta obstaculosPos 0 (random 169)))
;posMeta

;;;;;;;;;;;;;;;;;;
(define (ponerMetaVisual pos)
  (cond
    [(< pos 13) (set! imgBackground (underlay/xy imgBackground (+ (* pos 51) 20) 20 meta)) (display imgBackground)]
    [(< pos 26) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 13) 51) 20) 71 meta)) (display imgBackground)]
    [(< pos 39) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 26) 51) 20) 122 meta)) (display imgBackground)]
    [(< pos 52) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 39) 51) 20) 173 meta)) (display imgBackground)]
    [(< pos 65) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 52) 51) 20) 224 meta)) (display imgBackground)]
    [(< pos 78) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 65) 51) 20) 275 meta)) (display imgBackground)]
    [(< pos 91) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 78) 51) 20) 326 meta)) (display imgBackground)]
    [(< pos 104) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 91) 51) 20) 377 meta)) (display imgBackground)]
    [(< pos 117) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 104) 51) 20) 428 meta)) (display imgBackground)]
    [(< pos 130) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 117) 51) 20) 479 meta)) (display imgBackground)]
    [(< pos 143) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 130) 51) 20) 530 meta)) (display imgBackground)]
    [(< pos 156) (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 143) 51) 20) 581 meta)) (display imgBackground)]
    [else (set! imgBackground (underlay/xy imgBackground (+ (* (- pos 156) 51) 20) 632 meta)) (display imgBackground)]))


;Funcion que coloca los obstaculos y la meta en el laberinto dada una lista que contenga las posiciones de dichos elementos.El ultimo elemento se interpreta como la meta.
(define (colocarElementosLab lista) 
  (cond
    [(empty? lista) (display "\nSe ha cargado el laberinto exitosamente:\n") (display imgBackground)]
    [(= (length lista) 1) (ponerMetaVisual posMeta)] ;(colocarElementosLabAux lista (random 0 2) 1)] ;el ultimo elemento de la lista se va a tomar como meta!!
    [else
     (colocarElementosLabAux lista (random 0 2) 0)])) ;(random 0 2) devuelve un #random entre 1 y 2

(define (colocarElementosLabAux lista nRandom bandera)
  ;la bandera se pone en 1 cuando hay que ingresar la meta
  ;x indica los espacios a la derecha, y los espacios hacia abajo
  ;la posicion 0 se valida especificamente porque es un caso especial que no aplica a la formula creada. 
  ;la formula creada se basa en las posiciones del 0 al 12. Se multiplica la posicion de la matriz por 51 y se le suma 20. Esto nos da los espacios exactos a la derecha que hay que moverse
  ;para el resto de lineas 2-13, se aplica la misma logica pero restandole a la posicion de la matriz el valor necesario para convertir dicha posicion como un valor de la primera fila 0-12
  ;finalmente, dependiendo de la linea donde se este, el valor de y va aumentando en 51
  ;51 es el numero clave para moverse a la derecha o hacia abajo, ya que ese es el espacio  que hay que dejar entre obstaculos, meta y jugador.
  (cond
    ;AL PARECER 0 NO ES UN CASO ESPECIAL!1 YA QUE (< (car lista) 13) APLICA TAMBIEN PARA LA POSICION 0
    ;[(equal? (car lista) 0) (cond
                              ;[(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground 20 20 obstacle1)) (set! imgBackground (underlay/xy imgBackground 20 20 obstacle2))) (colocarElementosLab (cdr lista))]
                              ;[else (set! imgBackground (underlay/xy imgBackground 20 20 meta)) (colocarElementosLab (cdr lista))])]
    [(< (car lista) 13) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (car lista) 51) 20) 20 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (car lista) 51) 20) 20 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (car lista) 51) 20) 20 meta)) (colocarElementosLab (cdr lista))])]


    [(< (car lista) 26) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 13) 51) 20) 71 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 13) 51) 20) 71 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 13) 51) 20) 71 meta)) (colocarElementosLab (cdr lista))])]                    
                                                  

    [(< (car lista) 39) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 26) 51) 20) 122 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 26) 51) 20) 122 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 26) 51) 20) 122 meta)) (colocarElementosLab (cdr lista))])]


    [(< (car lista) 52) (cond
                           [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 39) 51) 20) 173 obstacle1))
                                                   (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 39) 51) 20) 173 obstacle2)))
                                               (colocarElementosLab (cdr lista))]
                           [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 39) 51) 20) 173 meta)) (colocarElementosLab (cdr lista))])]


    
    [(< (car lista) 65) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 52) 51) 20) 224 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 52) 51) 20) 224 obstacle2)))
                                                  (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 52) 51) 20) 224 meta)) (colocarElementosLab (cdr lista))])]


    [(< (car lista) 78) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 65) 51) 20) 275 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 65) 51) 20) 275 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 65) 51) 20) 275 meta)) (colocarElementosLab (cdr lista))])]
                 

    [(< (car lista) 91) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 78) 51) 20) 326 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 78) 51) 20) 326 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 78) 51) 20) 326 meta)) (colocarElementosLab (cdr lista))])]


    [(< (car lista) 104) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 91) 51) 20) 377 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 91) 51) 20) 377 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 91) 51) 20) 377 meta)) (colocarElementosLab (cdr lista))])]

                
    [(< (car lista) 117) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 104) 51) 20) 428 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 104) 51) 20) 428 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 104) 51) 20) 428 meta)) (colocarElementosLab (cdr lista))])]
          

    [(< (car lista) 130) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 117) 51) 20) 479 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 117) 51) 20) 479 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 117) 51) 20) 479 meta)) (colocarElementosLab (cdr lista))])]


    [(< (car lista) 143) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 130) 51) 20) 530 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 130) 51) 20) 530 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 130) 51) 20) 530 meta)) (colocarElementosLab (cdr lista))])]

    
    [(< (car lista) 156) (cond  
            [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 143) 51) 20) 581 obstacle1))
                                    (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 143) 51) 20) 581 obstacle2)))
                                (colocarElementosLab (cdr lista))]
            [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 143) 51) 20) 581 meta)) (colocarElementosLab (cdr lista))])]

    [(< (car lista) 169) (cond  
            [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 156) 51) 20) 632 obstacle1))
                                    (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 156) 51) 20) 632 obstacle2)))
                                (colocarElementosLab (cdr lista))]
            [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 156) 51) 20) 632 meta)) (colocarElementosLab (cdr lista))])]))


;Prueba colocarElementosLab:
;(colocarElementosLab '(1 2 3 4 5 6 7 8 9 10 155 154 166))





;De la siguiente manera se crea una matriz 
;(define (make-map N)
 ; (build-matrix N N (λ (x y) (random 2)))) 

;(define x (make-map 13)) ;x contiene una matriz de 13x13
;x

;Funcion que guarda en la lista obstaculosPos las posiciones donde se encuentran los obstaculos de la matriz creada (laberinto). Utiliza como ayuda convertirFIlasColumnas
(define (posicionesObstaculos matriz i j)
  (cond
    [(equal? i 13) (set! obstaculosPos (shuffle obstaculosPos))(colocarElementosLab obstaculosPos)] ;Una vez terminada la recursividad, se colocan los elementos en la parte visual con la lista desordenada
    [(equal? j 13) (set! i (+ i 1)) (set! j 0) (posicionesObstaculos matriz i j)]
    [else (cond
                ;[(equal? (matrix-ref matriz i j) 1) (set! obstaculosPos (append obstaculosPos (list (convertirFilasColumnas i j)))) (posicionesObstaculos matriz i (+ j 1))]
                [(equal? (matrix-ref matriz i j) 3) (set! obstaculosPos (append obstaculosPos (list (convertirFilasColumnas i j)))) (posicionesObstaculos matriz i (+ j 1))]
                [else (posicionesObstaculos matriz i (+ j 1))])]))


;Funcion que desordena una lista utilizando shuffle!! Creo que esta funcion es inecesaria
(define (desordenarLista lista)
  (set! obstaculosPos (shuffle lista)))

;De la siguiente manera se toman losobstaculos de la matriz x , se desordena la lista con obstaculos y se agregan al laberinto visualmente
;(set! imgBackground (underlay/xy imgBackground 20 20 imgDer))
;(posicionesObstaculos random-M 0 0)
;obstaculosPos
;imgBackground
;(desordenarLista obstaculosPos)
;(colocarElementosLab (shuffle obstaculosPos))

;Funcion que devuelve la posicion exacta de la meta luego de haber rellenado el laberinto con los obstaculos
;(define (posMeta)
 ; (list-ref obstaculosPos (- (length obstaculosPos) 1)))  ;NO SE USA!!!!!

;Funcion que muestra al usuario el camino mas corto a la meta mediante la interfaz usando run-movie
(define (mostrarSolucion bestPath ordenMovimientos) 
  (cond
    [(empty? bestPath) (run-movie 1 solucion)]
    [else (set! solucion (append solucion (list imgBackground))) (mostrarSolucionAux bestPath ordenMovimientos)]))

(define (mostrarSolucionAux bestPath ordenMovimientos)
   (cond
    [(= (car bestPath) 0) (set! solucion (append solucion (list (underlay/xy imgBackground 20 20 (car ordenMovimientos)))))
                          (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 13) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (car bestPath) 51) 20) 20 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 26) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 13) 51) 20) 71 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 39) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 26) 51) 20) 122 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 52) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 39) 51) 20) 173 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 65) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 52) 51) 20) 224 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 78) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 65) 51) 20) 275 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 91) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 78) 51) 20) 326 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 104) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 91) 51) 20) 377 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 117) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 104) 51) 20) 428 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 130) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 117) 51) 20) 479 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 143) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 130) 51) 20) 581 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [(< (car bestPath) 156) (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 143) 51) 20) 530 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]

    [else (set! solucion (append solucion (list (underlay/xy imgBackground (+ (* (- (car bestPath) 156) 51) 20) 632 (car ordenMovimientos)))))
                           (mostrarSolucion (cdr bestPath) (cdr ordenMovimientos))]))


;Primero cargar el tablero
(posicionesObstaculos tablero 0 0)

;(define bestPath (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13))

;(movimientoPersonaje bestPath)
;ordenMostrarPj
;(mostrarSolucion bestPath ordenMostrarPj)

(define (obtenerBestPath filaIni colIni tablero)
  
  (define recorrido (a-search map@ (map-nodo tablero filaIni colIni) (nodo-manhattan-distance (obtenerIdePos posMeta) (obtenerJdePos posMeta))))
  
  (if (null? recorrido)
      (display "No existe un camino desde ese punto hasta la meta, por favor vuelva a ejecutar el programa con un punto diferente\n")
      (obtener-camino recorrido)
      )
        ;(save-image (map-image tablero) map-image.png)
       ; (save-image (path-image tablero recorrido) path-image.png)
  )

(display "\nSeleccione las coordenadas donde quiere colocar el personaje")
(display "\nIngrese la posicion i: ")
(define i (read))
(display "\nIngrese la posicion j: ")
(define j (read))

(define bestPath (obtenerBestPath i j tablero)) ;lista con el camino mas corto

;obtenerBestPath
(define (inicio)
  (cond
    [(void? (movimientoPersonaje bestPath) (display "No existe un camino desde ese punto hasta la meta, por favor vuelva a ejecutar el programa con un punto diferente\n"))]
    [else (movimientoPersonaje bestPath) (mostrarSolucion bestPath ordenMostrarPj)]))
      
;(movimientoPersonaje bestPath)
;ordenMostrarPj
;(mostrarSolucion bestPath ordenMostrarPj)
(inicio)


;bestPath