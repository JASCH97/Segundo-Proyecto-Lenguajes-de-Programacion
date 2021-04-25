#lang racket

(require picturing-programs)
(require math/matrix)

;(define obstacle1 -arania- )

;(define obstacle2 -arbusto-)

;(define meta -antorcha-)

;(define imgFrente -link de Frente-)

;(define imgEspalda -link de espaldas-)

;(define imgIzq -link de perfil izquierdo-)

;(define imgDer -link de perfil derecho-)

;(define imgBackground -fondo del laberindo-)

(define ordenMostrarPj (list imgFrente))    ;lista que va a contener el orden en el que se debe mostrar el perfil del personaje. De primero siempre debe estar la imagen frontal!!

(define bestPath (list 17 12 13 18)) ;lista con el camino mas corto

(define obstaculosPos (list )) ;lista que contiene las posiciones de los obstaculos


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
;(run-movie 0.5 h) ;x derecha y abajo

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


;Funcion que coloca los obstaculos y la meta en el laberinto dada una lista que contenga las posiciones de dichos elementos.El ultimo elemento se interpreta como la meta.
(define (colocarElementosLab lista) 
  (cond
    [(empty? lista) (display "\nSe ha cargado el laberinto exitosamente:\n") (display imgBackground)]
    [(= (length lista) 1) (colocarElementosLabAux lista (random 0 2) 1)] ;el ultimo elemento de la lista se va a tomar como meta!!
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
    [(equal? (car lista) 0) (cond
                              [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground 20 20 obstacle1)) (set! imgBackground (underlay/xy imgBackground 20 20 obstacle2))) (colocarElementosLab (cdr lista))]
                              [else (set! imgBackground (underlay/xy imgBackground 20 20 meta)) (colocarElementosLab (cdr lista))])]
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
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 65) 51) 20) 326 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 65) 51) 20) 326 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 65) 51) 20) 326 meta)) (colocarElementosLab (cdr lista))])]
                 

    [(< (car lista) 91) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 78) 51) 20) 377 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 78) 51) 20) 377 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 78) 51) 20) 377 meta)) (colocarElementosLab (cdr lista))])]


    [(< (car lista) 104) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 91) 51) 20) 428 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 91) 51) 20) 428 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 91) 51) 20) 428 meta)) (colocarElementosLab (cdr lista))])]

                
    [(< (car lista) 117) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 104) 51) 20) 479 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 104) 51) 20) 479 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 104) 51) 20) 479 meta)) (colocarElementosLab (cdr lista))])]
          

    [(< (car lista) 130) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 117) 51) 20) 530 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 117) 51) 20) 530 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 117) 51) 20) 530 meta)) (colocarElementosLab (cdr lista))])]


    [(< (car lista) 143) (cond
                          [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 130) 51) 20) 581 obstacle1))
                                                  (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 130) 51) 20) 581 obstacle2)))
                                              (colocarElementosLab (cdr lista))]
                          [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 130) 51) 20) 581 meta)) (colocarElementosLab (cdr lista))])]

    
    [(< (car lista) 156) (cond  
            [(equal? bandera 0) (if (equal? nRandom 1) (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 143) 51) 20) 632 obstacle1))
                                    (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 143) 51) 20) 632 obstacle2)))
                                (colocarElementosLab (cdr lista))]
            [else (set! imgBackground (underlay/xy imgBackground (+ (* (- (car lista) 143) 51) 20) 632 meta)) (colocarElementosLab (cdr lista))])]

    [else
     (cond
     [(equal? bandera 1) (colocarElementosLab '(- (car lista) 20))] ;Si cae aqui y es la antorcha, se modifica su valor para que no se ignore. Restarle 20 es buena opcion.
     [else
      (colocarElementosLab (cdr lista))])])) ;Si cae aqui es porque se genera un obstaculo fuera de los parametros del laberinto, entonces lo ignora.



;Prueba colocarElementosLab:
;(colocarElementosLab '(1 2 3 4 5 6 7 8 9 10 155 154 166))



;De la siguiente manera se crea una matriz 
(define (make-map N)
  (build-matrix N N (λ (x y) (random 2)))) 

(define x (make-map 13)) ;x contiene una matriz de 13x13

;x

;Funcion que guarda en la lista obstaculosPos las posiciones donde se encuentran los obstaculos de la matriz creada (laberinto). Utiliza como ayuda convertirFIlasColumnas
(define (posicionesObstaculos matriz i j)
  (cond
    [(equal? i 13) (set! obstaculosPos (shuffle obstaculosPos))(colocarElementosLab obstaculosPos)] ;Una vez terminada la recursividad, se colocan los elementos en la parte visual con la lista desordenada
    [(equal? j 13) (set! i (+ i 1)) (set! j 0) (posicionesObstaculos matriz i j)]
    [else (cond
                [(equal? (matrix-ref matriz i j) 1) (set! obstaculosPos (append obstaculosPos (list (convertirFilasColumnas i j)))) (posicionesObstaculos matriz i (+ j 1))]
                [else (posicionesObstaculos matriz i (+ j 1))])]))

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


;De la siguiente manera se toman losobstaculos de la matriz x , se desordena la lista con obstaculos y se agregan al laberinto visualmente
;(set! imgBackground (underlay/xy imgBackground 20 20 imgDer))
(posicionesObstaculos x 0 0)

;(colocarElementosLab (shuffle obstaculosPos))

;Funcion que devuelve la posicion exacta de la meta luego de haber rellenado el laberinto con los obstaculos
(define (posMeta)
  (list-ref obstaculosPos (- (length obstaculosPos) 1)))
