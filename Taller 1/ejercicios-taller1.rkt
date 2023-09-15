#lang eopl

;; ==========================================================================
;;|                                                                          |
;;|FUNDAMENTOS DE INTERPRETACIÓN Y COMPILACIÓN DE LENGUAJES DE PROGRAMACIÓN  |
;;|TALLER 1: Definición recursiva de programas e inducción                   |
;;|                                                                          |
;;|INTREGANTES:                                                              |
;;|SANTIAGO DUQUE CHACÓN - 2180099                                           |    
;;|DEIBY ALEXANDER RODRIGUEZ RODALLEGA - 1842917                              |
;;|VALENTINA SALAMANCA RODRIGUEZ - 1842427                                   |
;;|                                                                          |
;; =========================================================================

;; ==============================================================================
;;|                                                                              |
;;|                           FUNCIONES AUXILIARES                               |
;;| append:                                                                      |
;;| Proposito:                                                                   |
;;| L * L -> L' : Procedimiento que une dos listas, de tal forma que la segunda  |
;;| se coloca al final de la primera.                                            |
;;| <lista> := ()                                                                |
;;|         := (<valor-de-scheme> <lista>)                                       |
;;|                                                                              |
(define append
  (lambda (list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2)))))
;;| Pruebas                                                                     |
(append '() '())
(append '() '(1 a 3 5 (1 (2) a)))
(append '(1 a 3 5 (1 (2) a)) '())
(append '(1 a 3 5 (1 (2) a)) '(1 a 3 5 (1 (2) a)))
;;|                                                                             |
;;|                                                                             |
;; ============================================================================

;; PUNTO 1
;; invert :
;; Proposito:
;; L -> L' : Procedimiento que invierte los elementos de una lista
;; de duplas.
;;
;;<dupla> := (<valor-de-scheme> <valor-de-scheme>)
;;<lista> := ()
;;        := (<dupla> <lista>)

(define invert
(lambda (L)
  (if (null? L)
      '()
      (cons (list (cadr (car L)) (car (car L)))
            (invert (cdr L))))))

;; Pruebas
(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))


;; PUNTO 2
;; down :
;; Proposito:
;; L -> L' : Funcíon que retorna una lista con cada
;; elemento de L asociado a un nivel ḿas de paŕentesis
;; comparado con su estado original en L.
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

(define down
  (lambda (L)
    (if (null? L)
        L
        (cons (cons (car L) '()) (down (cdr L))))))

;; Pruebas
(down '(1 2 3))
(down '((una) (buena) (idea)))
(down '(un (objeto (mas)) complicado))


;; 3. list-set :
;; Proposito:
;; L * n * x -> L’ : Procedimiento que cambia el elemento
;; de la posicion n, de la lista L, por el elemento x
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)
;;
;; <elemento> := <scheme-value>

;; > (list-set ’(a b c d) 2 ’(1 2))
;; (a b (1 2) d)
;; > (list-set ’(a b c d) 3 ’(1 5 10))
;; (a b c (1 5 10))

(define list-set
  (lambda (L n x)
    (if (null? L)
        '()
        (if (eqv? n 0)
            (cons x (cdr L))
            (cons (car L) (list-set (cdr L) (- n 1) x))))))

;; Pruebas
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))
(list-set '(f i c l p) 0 'fundamentos)
(list-set '(esta es una lista) 3 '(lista '(con listas)))


;; 4. filter-in :
;; P * L -> L' : Procedimiento que toma un predicado y
;; una lista. Retorna otra lista donde sus elementos son
;; aquellos presentes en la lista L que recibe, pero que
;; cumplen con el predicado P.
;;
;; <pred> := <identificador>
;;        := (<identificador> <lista-args>)
;; <lista-args> := empty
;;              := <scheme-value> <lista-args>

(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))

;; Pruebas
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
(filter-in symbol? '((palabra) 3 (4 5) #t '(una oracion)))
(filter-in boolean? '(0 1 verdadero #f true?))


;; PUNTO 5
;; list-index :
;; Proposito:
;; P * L -> N : funcíon retorna (desde una posicíon inicial 0)
;; el primer elemento de la lista que satisface el predicado P.
;; Retornar #f en caso que no se satisfaga el predicado.
;;
;; <predicado-exp>  := (<identificador>)
;;                  := (<identificador> <predicado-exp>)
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define list-index
  (lambda (P L)
    (letrec ((pos 0)
             (index
              (lambda (P L pos)
                (cond
                  [(null? L) #f]
                  [(P (car L)) pos]
                  [else (index P (cdr L) (+ pos 1))]))))
      (index P L pos))))

;; Pruebas
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))

;; PUNTO 6
;; swapper :
;; Proposito:
;; E1 * E2 * L -> L' : Procedimiento que toma un elemento E1 y lo intercambia
;; por E2 y viceversa en una lista.
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

(define swapper
  (lambda (E1 E2 L)
  (if (null? L)
      '()
      (if (eqv? E1 (car L))
          (cons E2 (swapper E1 E2 (cdr L)))
          (if (eqv? E2 (car L))
              (cons E1 (swapper E1 E2 (cdr L)))
              (cons (car L) (swapper E1 E2 (cdr L))))))))

;; Prueba
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))

;; 7. cartesian-product
;; Proposito:
;; L1 * L2 -> L' : Procedimiento que toma 2 listas de
;; simbolos y devuelve una lista de tuplas que
;; representa el producto cartesiano entre ellas
;;
;; <lista-simbolos> ::= ()
;;              ::= (<scheme-value> <lista-simbolos>)

(define cartesian-product
  (lambda (L1 L2)
    (cond
      ((null? L1) '())
      ((null? L2) '())
      (else
       (cons (cons (car L1) (list (car L2)))
             (append (cartesian-product (list (car L1)) (cdr L2))
                     (cartesian-product (cdr L1) L2)))))))

;; Pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))
(cartesian-product '(racket java python) '(FPFC FPOO FPI))
(cartesian-product '(sistemas ciencias) '(3743 E20))


;;PUNTO 8
;; mapping : 
;; Proposito:
;; F * L1 * L2 -> L': Procedimiento que al aplicar la función
;; unaria F con el argumento a, debe arrojar el número b.
;;
;; <lista> := () 
;;         := (<valor-de-scheme> <lista>)

(define mapping
  (lambda (F L1 L2)
    (cond
      ((null? L1) '())
      ((null? L2) '())
      (else
       (if (eqv? (F (car L1)) (car L2))
           (cons (cons (car L1) (list(car L2)))
                   (mapping F (cdr L1) (cdr L2)))
           (mapping F (cdr L1) (cdr L2)))))))

;; Pruebas
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

;; PUNTO 9
;; inversions :
;; Proposito:
;; L -> N : Funcíon que recibe como entrada una lista L,
;; y determina el ńumero de inversiones de la lista L.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define inversions
  (lambda (L)
    (letrec ((count-inversions
              (lambda (list)
                (cond
                  [(null? list) 0]
                  [else
                   (+ (count-inversions (cdr list))
                      (count-greater (car list) (cdr list)))])))
             (count-greater
              (lambda (n list)
                (cond
                  [(null? list) 0]
                  [(> n (car list)) (+ 1 (count-greater n (cdr list)))]
                  [else (count-greater n (cdr list))]))))
      (count-inversions L))))

;; Pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))

;; PUNTO 10
;; up :
;; Proposito:
;; L -> L' : Funcíon que recibe como entrada una lista L,
;; y remueve un par de paŕentesis a cada elemento del nivel ḿas alto de la lista
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define up
  (lambda (L)
    (cond
      [(null? L) '()]
      [(list? (car L)) (append (car L) (up (cdr L)))]
      [else (cons (car L) (up (cdr L)))])))

;; Pruebas
(up '((1 2) (3 4)))
(up '((x (y)) z))
(up '((1 2) (x (y)) (((z))) 3 4))

;; PUNTO 11
;; zip :
;; Proposito:
;; F * L1 * L2 -> L' : Procedimiento que aplica la función F sobre
;;los elementos en la posición n-ésima en L1 y L2.

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (cons (F (car L1) (car L2))(zip F (cdr L1) (cdr L2))))))


;; 12. filter-acum
;; Proposito:
;; a * b * F * acum * filter -> n : Procedimiento que aplica
;; la funcion binaria F a los numeros en [a,b] y que cumplan
;; el predicado de la funcion filter; el resultado se va
;; guardando en acum y se retorna este valor final
;;
;; <func-bin> := <identificador>
;;            := (<identificador> <numero> <numero>)
;;
;; <pred> := <identificador>
;;        := (<identificador> <lista-args>)
;; <lista-args> := empty
;;              := <scheme-value> <lista-args>

(define filter-acum
  (lambda (a b F acum filter)
    (cond
      ((eqv? a b)
       (if (filter a)
           (F a acum)
           acum))
      ((not (filter a))
       (filter-acum (+ a 1) b F acum filter))
      (else
       (filter-acum (+ a 1) b F (F a acum) filter)))))

;; Pruebas
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)
(filter-acum 1 10 * 1 number?)
(filter-acum 4 20 * 1 even?)
(filter-acum 1 5 expt 1 odd?)


;; PUNTO 13
;; operate :
;; Proposito:
;; L * L -> L' : La funcíon retorna el resultado de aplicar
;; sucesivamente las operaciones en lrators a los valores en lrands.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define operate
  (lambda (lrators lrands)
    (if (null? lrators)
        (car lrands)
        ((car lrators) (car lrands) (operate (cdr lrators) (cdr lrands))))))

;; Pruebas
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))
(operate (list + * * * *) '(1 1 1 1 2 3))


;; 14. path
;; Proposito:
;; n * BST -> L : Funcion que toma un numero n y un
;; arbol binario de busqueda, representado con listas
;; y que contenga n. Retorna una lista L que contiene
;; las cadenas 'right' o 'left', que indican la ruta
;; para llegar al numero n. Si n es la raiz devuelve
;; una lista vacia.
;;
;; <arbol-binario> := (arbol-vacio) empty
;;                  := (nodo) numero <arbol-binario> <arbol-binario>

;; NOTA IMPORTANTE: Se asume que el numero SIEMPRE ESTA en el arbol

(define path
  (lambda (n BST)
    (cond
      ((null? BST) (eopl:error "Arbol vacio"))
;      ((and (null? (cadr BST)) (null? (caddr BST)))
;       '())
      ((eqv? (car BST) n) '()) 
      ((< n (car BST))
       (cons 'left (path n (cadr BST))))
      ((> n (car BST))
       (cons 'right (path n (caddr BST))))
      (else
       (eopl:error "Uso incorrecto de la funcion")))))
                        
    
;; Pruebas
(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))
(path 20 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))
(path 31 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))
(path 13 '(14 (7 (12 () ()) (13 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))
(path 14 '(14 (7 (12 () ()) (13 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))

;; PUNTO 15
;; count-odd-and-even arbol :
;; Proposito: 
;; operacionB -> N | error : Procedimiento que retorna la cantidad de pares e impares
;; de un arbol.
;;
;;<tree> := (tree) empty
;;       := (nodo) número <tree> <tree>
;;<aux-count> ::= (lambda (<tree>) <condicionales>)

(define count-odd-and-even
  (lambda (tree)
    (define aux-count
      (lambda (tree)
        (cond
          ((null? tree) '(0 0)) 
          ((pair? tree)
           (let ((lc (aux-count (car tree)))
                 (rc (aux-count (cdr tree))))
             (list (+ (car lc) (car rc))
                   (+ (cadr lc) (cadr rc)))))
          ((even? tree) '(1 0)) 
          ((odd? tree) '(0 1))
          (else (eopl:error "Invalido")))))
    (aux-count tree)))    
          
; Pruebas
(count-odd-and-even '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))
(count-odd-and-even '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (32 () ()))))

;; PUNTO 16
;; Operar-binarias :
;; Proposito: 
;; operacionB -> N | error : Dada una expresión aritmética representada como una lista anidada, esta función
;; evalúa la expresión y devuelve un número si la expresión es válida o un error si
;; la expresión es inválida.
;;
;; <expr> := <valor-de-scheme>
;;        :=(suma <expr> <expr>)
;;        :=(resta <expr> <expr>)
;;        :=(multiplica <expr> <expr>)

(define Operar-binarias
  (lambda (operacionB)
    (cond
      ((number? operacionB) operacionB)
      ((list? operacionB)
       (cond
         ((eqv? (cadr operacionB) 'suma)
          (+ (Operar-binarias (car operacionB))
             (Operar-binarias (caddr operacionB))))
         ((eqv? (cadr operacionB) 'resta)
          (- (Operar-binarias (car operacionB))
             (Operar-binarias (caddr operacionB))))
         ((eqv? (cadr operacionB) 'multiplica)
          (* (Operar-binarias (car operacionB))
             (Operar-binarias (caddr operacionB))))
         (else (eopl:error "Invalido"))))
      (else (eopl:error "Invalido")))))

;; Pruebas
(Operar-binarias 4)
(Operar-binarias '(2 suma 9))
(Operar-binarias '((2 multiplica 3) suma (5 resta 1)))
(Operar-binarias'((2 multiplica (4 suma 1))
                  multiplica((2 multiplica 4) resta 1 )))


;; 17. prod-scalar-matriz
;; Proposito:
;; mat * vec -> vecr : Procedimiento que toma una matriz mat
;; que se representa como una lista de listas y un vector vec
;; que es una lista de numeros. Retorna la matriz resultante
;; de multiplicar la matriz por el vector (se hace producto
;; de cada elemento en una fila con el elemento de mismo indice
;; en el vector) así: ((a,b) (c,d))*(e,f)=((ae,bf) (ce,df))
;;
;; <matriz> := (matriz-vacia) empty
;;          := (fila) <lista-de-numeros> <matriz>
;;       
;; <lista-de-numeros> := () empty
;;                    := numero <lista-de-numeros>
;;
;; No se incluye la gramatica de un vector, pues este es una
;; lista de numeros.


;; auxFila
;; Proposito:
;; a * b -> c : Procedimiento auxiliar para la el producto
;; de matriz por vector. Toma dos listas a y b, y retorna
;; la lista c que se construye asi:
;;    c = (a1*b1, a2*b2, ... , a_*b_)
;;
;; <lista-de-numeros> := () empty
;;                    := numero <lista-de-numeros>

(define auxFila
  (lambda (a b)
    (if (or (null? a) (null? b))
        '()
        (cons (* (car a) (car b))
              (auxFila (cdr a) (cdr b))))))

;; Pruebas
(auxFila '(1 1) '(2 3))
(auxFila '(2 2) '(2 3))
(auxFila '(1 3 5 7 9) '(2 4 6 8 10))


(define prod-scalar-matriz
  (lambda (mat vec)
    (cond
      ((or (null? mat) (null? vec)) '())
      (else
       (cons (auxFila (car mat) vec)
             (prod-scalar-matriz (cdr mat) vec))))))

;; Pruebas
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
(prod-scalar-matriz '((1 2 3) (2 2 2) (9 8 7)) '(4 2 10))
(prod-scalar-matriz '((1 2 3) (2 2 2) (9 8 7) (15 10 5)) '(4 2 10))


;; PUNTO 18
;; pascal :
;; Proposito:
;; N -> L' : retorna la fila N del triangulo de Pascal.
;;
;; <number> := <valor-de-scheme>

(define pascal
  (lambda (N)
    (letrec ((aux
              (lambda (list N)
                (if (equal? 0 N)
                    list
                    (aux (sum-list (append '(0) list) (append list '(0)) '()) (- N 1)))))
             (sum-list
              (lambda (list1 list2 list3)
                (cond
                  [(null? list1) list3]
                  [else (sum-list (cdr list1) (cdr list2) (append list3 (list (+ (car list1) (car list2)))))]))))
      (aux '(1) (- N 1)))))

;; Pruebas
(pascal 5)
(pascal 1)
(pascal 15)
