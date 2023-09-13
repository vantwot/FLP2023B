#lang eopl

;; Punto 1
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

;; Punto 6
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

;; 3. list-set :
;; Proposito:
;; L * n * x -> L’ : Procedimiento que cambia el elemento
;; de la posicion n, de la lista L, por el elemento x
;;
;; <lista> := ()
;; := (<valor-de-scheme> <lista>)

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
            (cons (car L) (list-set (cdr L) (- n 1) x)
                  )
            )
        )
    )
  )

;;punto 4
(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))
      
