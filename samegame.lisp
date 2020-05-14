;;;; Projeto de Procura e Planeamento 2019/2020
;;;;
;;;; Joao Palet  - 86447
;;;; Simao Nunes - 86512


(in-package :user)


;;; compile and load
(compile-file "procura.lisp")
(load "setup")
(load "procura")


;;;; estrutura que representa uma coordenada do tabuleiro
(defstruct point
  i
  j
)


;;; main function
(defun resolve-same-game (problem strategy)
    (write problem)
    (write strategy)
)


;;; recebe uma jogada e um tabuleiro e devolve o tabuleiro resultante
(defun apply-play (i j board)
    )


;;; recebe uma grupo de pecas e calcula o seu representante
(defun leader (pieces)
    (car (sort pieces 'compare-points)))


;;; compara dois pontos
(defun compare-points(p1 p2)
    (if (<= (point-i p1) (point-i p2))
        (if (<= (point-j p1) (point-j p2))
            T)))


;;; recebe uma posicao e um tabuleiro e devolve as posições adjacentes
;;; que tem a mesma cor
(defun check-adjacents (i j board)
    (let ((block nil))
        ))


;;; recebe uma posicao e um tabuleiro e devolve true se a posicao acima for
;;; da mesma cor
(defun check-up (i j board)
    (if (> i 0)
        (if (eql (which-color i j board) (which-color (- i 1) j board))
            T)))


;;; recebe uma posicao e um tabuleiro e devolve true se a posicao a esquerda
;;; for da mesma cor
(defun check-left (i j board)
    (if (> j 0)
        (if (eql (which-color i j board) (which-color i (- j 1) board))
            T)))


;;; recebe uma posicao e um tabuleiro e devolve true se a posicao abaixo
;;; for da mesma cor
(defun check-down (i j board)
    (if (< i (list-length board))
        (if (eql (which-color i j board) (which-color (+ i 1) j board))
            T)))


;;; recebe uma posicao e um tabuleiro e devolve true se a posicao a direita
;;; for da mesma cor
(defun check-right (i j board)
    (if (< j (list-length (car board)))
        (if (eql (which-color i j board) (which-color i (+ j 1) board))
            T)))


;;; funcao que devolve a cor de uma determinada posicao
(defun which-color (i j board)
    ;; linha correspondente
    (setf row (nth i board))
    ;; cor 
    (nth j row)
)


;; (resolve-same-game problem_1 strategy_1)
;; (write (which-color 3 9 problem_1))
