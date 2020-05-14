;;;; xxxx
(in-package :user)


;;;; compile and load
(compile-file "procura.lisp")
(load "setup")
(load "procura")

;;;; estrutura que representa uma coordenada do tabuleiro
(defstruct point
  i
  j
)

;;;; main function
(defun resolve-same-game (problem strategy)
    (write problem)
    (write strategy)
)

;;; recebe uma posicao e um tabuleiro e devolve true se a posicao acima for
;;; da mesma cor
(defun check-up (i j board)
    (if (/= i 0)
        (if (eql (which-color i j board) (which-color (- i 1) j board))
            T)))


;;; recebe uma posicao e um tabuleiro e devolve true se a posicao a esquerda
;;; for da mesma cor
(defun check-left (i j board)
    (if (/= j 0)
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


;;;; recebe uma posicao e um tabuleiro e devolve a cor correspondente
(defun which-color (i j board)
    ;; linha correspondente
    (setf row (nth i board))
    ;; cor 
    (nth j row)
)

(defun point-in-list (point points)
    (setf flag nil)
    (write "point in list")
    (write (list-length points))
    ;;; se nao for uma lista vazia
    (if (not (equal points (list ()))) (progn 
        (loop for p in points do
            (write p)
            ;;; i equals
            (setf iFlag (equal (point-i p) (point-i point)))
            ;;; j equals
            (setf jFlag (equal (point-j p) (point-j point)))
            (if (and iFlag jFlag) (setf flag T))
        ))
    )
    (write "passei isto")
    (if flag T nil)
)

;; (write (point-in-list (make-point :i 1 :j 1) (list (make-point :i 2 :j 1))))

;;; recebe uma posicao e um tabuleiro e devolve as posições adjacentes
;;; que tem a mesma cor
(defun check-group (i j board)

    ;;;; setup das listas de fila, visitados e grupo
    (setf visited (list ()))
    (setf queue (list (make-point :i i :j j)))
    (setf group (list (make-point :i i :j j)))

    ;;;; enquanto a fila ainda tem elementos
    (loop while (> (list-length queue) 0) do
        (write "entrei no loop")

        ;;;; fazer pop do proximo ponto da fila
        (setf point (pop queue))

        ;;;; se ainda nao foi visitado
        (if (not (point-in-list point visited)) (progn 
            ;;;; adicionar aos visitados
            (push point visited)
            
            ;;;; se a bola de cima pertencer ao grupo
            (if (check-up i j board) (progn 
                ;;;; adicionar a bola ao grupo e a queue
                (push (make-point :i (- i 1) :j j) group)
                (push (make-point :i (- i 1) :j j) queue)
                (write "up")
            ))
            ;;;; se a bola de baixo pertencer ao grupo
            (if (check-down i j board) (progn 
                ;;;; adicionar a bola ao grupo e a queue
                (push (make-point :i (+ i 1) :j j) group)
                (push (make-point :i (+ i 1) :j j) queue)
                (write "down")
            ))
            ;;;; se a bola da esquerda pertencer ao grupo
            (if (check-left i j board) (progn 
                ;;;; adicionar a bola ao grupo e a queue
                (push (make-point :i i :j (- j 1)) group)
                (push (make-point :i i :j (- j 1)) queue)
                (write "left")
            ))
            ;;;; se a bola da direita pertencer ao grupo
            (if (check-right i j board) (progn 
                ;;;; adicionar a bola ao grupo e a queue
                (push (make-point :i i :j (+ j 1)) group)
                (push (make-point :i i :j (+ j 1)) queue)
                (write "right")
            ))
            (write "group->")
            (write group)
            (write "visitados->")
            (write visited)
            (write "queue->")
            (write queue)
        ))
    )
    group
)


(terpri)
(terpri)
;; (resolve-same-game problem_1 strategy_1)
;; (write (which-color 3 9 problem_1))
;; (write (check-group 1 0 problem_1))
