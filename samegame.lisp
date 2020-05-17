;;;; Projeto de Procura e Planeamento 2019/2020
;;;;
;;;; Joao Palet  - 86447
;;;; Simao Nunes - 86512


(in-package :user)


;;; compile and load
(compile-file "procura.lisp")
(load "setup")
(load "procura")


;;; estrutura que representa uma coordenada do tabuleiro
(defstruct point
  i
  j
)


;;; main function
(defun resolve-same-game (problem strategy)
    (write problem)
    (write strategy)
)

;;; recebe uma grupo de pecas e calcula o seu representante
(defun leader (pieces)
    (car (sort pieces #'compare-points)))

;;; compara dois pontos
(defun compare-points(p1 p2)
    (if (< (point-i p1) (point-i p2))
        T
        (if (= (point-i p1) (point-i p2))
            (if (<= (point-j p1) (point-j p2))
                T))))


;;; recebe uma jogada e um tabuleiro e devolve o tabuleiro resultante
(defun apply-play (point board)
    ) ; TODO

(defun generate-successors (board)
    ;;; gerar lideres do tabuleiro
    (setf leaders (filter (all-points 0 0 (list-length board) (list-length (car board))) board))
    ;;; chamar apply com os lideres
    ;;; por agora retorna leaders so para testar
    leaders
)


;;; recebe uma posicao e um tabuleiro e devolve true se a posicao 
;;; acima/a esquerda/abaixo/a direita for da mesma cor
(defun check-up (point board)
    (if (> (point-i point) 0)
        (if (eql (which-color point board) (which-color (make-point :i (- (point-i point) 1) :j (point-j point)) board))
            T)))
(defun check-left (point board)
    (if (> (point-j point) 0)
        (if (eql (which-color point board) (which-color (make-point :i (point-i point) :j (- (point-j point) 1)) board))
            T)))
(defun check-down (point board)
    (if (< (point-i point) (list-length board))
        (if (eql (which-color point board) (which-color (make-point :i (+ (point-i point) 1) :j (point-j point)) board))
            T)))
(defun check-right (point board)
    (if (< (point-j point) (list-length (car board)))
        (if (eql (which-color point board) (which-color (make-point :i (point-i point) :j (+ (point-j point) 1)) board))
            T)))


;;; recebe uma posicao e um tabuleiro e devolve a cor correspondente
(defun which-color (point board)
    (nth (point-j point) (nth (point-i point) board)))


;;; recebe uma lista de pontos, um tabuleiro e uma cor e devolve um tabuleiro 
;;; onde os pontos tem a cor pretendida
(defun change-block (points board color)
    (if (not (null points))
        (change-block (cdr points) (change-color (car points) board color) color))
        board)

(defun change-color (point board color)
    (setf (nth (point-j point) (nth (point-i point) board)) color)
    board)


;;; recebe um ponto e uma lista de pontos e verifica se o ponto esta
;;; contido na lista
(defun point-in-list (point points)
    (if (not (null points))
        (if (equalp point (car points))
            T ; ponto foi encontrado
            (point-in-list point (cdr points)))))


; recebe uma posicao e um tabuleiro e devolve as posições adjacentes
; que tem a mesma cor
(defun check-group (point board)

    ;; setup das listas de fila, visitados e grupo
    (setf visited (list ()))
    (setf queue (list point))
    (setf group (list point))

    ;; enquanto a fila ainda tem elementos
    (loop while (> (list-length queue) 0) do

        ;; fazer pop do proximo ponto da fila
        (setf point (pop queue))
        
        ;; se ainda nao foi visitado
        (if (not (point-in-list point visited)) (progn 
            ;; adicionar aos visitados
            (push point visited)
            
            ;; se a bola de cima for da mesma cor
            (if (check-up point board) (progn 
                (setf pointUp (make-point :i (- (point-i point) 1) :j (point-j point)))
                ;; caso a bola ainda nao esteja no grupo, temos de a adicionar
                (if (not (point-in-list pointUp group)) (push pointUp group))
                ;; adicionar a bola na queue para ser expandida
                (push pointUp queue)
            ))
            ;; se a bola de baixo for da mesma cor
            (if (check-down point board) (progn
                (setf pointDown (make-point :i (+ (point-i point) 1) :j (point-j point)))
                ;; caso a bola ainda nao esteja no grupo, temos de a adicionar
                (if (not (point-in-list pointDown group)) (push pointDown group))
                ;; adicionar a bola na queue para ser expandida
                (push pointDown queue)
            ))
            ;; se a bola da esquerda for da mesma cor
            (if (check-left point board) (progn
                (setf pointLeft (make-point :i (point-i point) :j (- (point-j point) 1)))
                ;; caso a bola ainda nao esteja no grupo, temos de a adicionar
                (if (not (point-in-list pointLeft group)) (push pointLeft group))
                ;; adicionar a bola na queue para ser expandida
                (push pointLeft queue)
            ))
            ;; se a bola da direita for da mesma cor
            (if (check-right point board) (progn
                (setf pointRight (make-point :i (point-i point) :j (+ (point-j point) 1)))
                ;; caso a bola ainda nao esteja no grupo, temos de adicionar
                (if (not (point-in-list pointRight group)) (push pointRight group))
                ;; adicionar a bola na queue para ser expandida
                (push pointRight queue)
            ))
        ))
    )
    group
)

;;; recebe uma lista de pontos e o tabuleiro
;;; devolve apenas os pontos lideres do tabuleiro 
;;; que produzem jogadas diferentes 
(defun filter (points board)
    (if (not (typep points 'cons))
        (if (equalp points (leader (check-group points board)))
            points
            nil
        )
        (if (equalp (car points) (leader (check-group (car points) board)))
            (cons (car points) (filter (cdr points) board))
            (filter (cdr points) board)
        )
    )
)

;;; recebe a coordenadas de controlo, o total de linhas 
;;; e o total de colunas do tabuleiro e retorna todas as 
;;; posicoes possiveis do tabuleiro
(defun all-points (i j totalRows totalColumns)
    (if (equal j (1- totalColumns))
        (if (equal i (1- totalRows))
            (make-point :i i :j j)
            (cons (make-point :i i :j j) (all-points (1+ i) 0 totalRows totalColumns))
        )
        (cons (make-point :i i :j j) (all-points i (1+ j) totalRows totalColumns))
    )
)


(terpri)
(terpri)
;; (resolve-same-game problem_1 strategy_1)
;; (write (which-color 3 9 problem_1))
;; (write (check-group 1 1 problem_1))



;;; ---------------------- para testes ----------------------

; (point-in-list (make-point :i 1 :j 1) (cons (make-point :i 2 :j 3) (cons (make-point :i 3 :j 3) (cons (make-point :i 1 :j 2) nil))))

; (change-block (cons (make-point :i 0 :j 0) (cons (make-point :i 0 :j 1) (cons (make-point :i 0 :j 2) nil))) problem_1 0)

; (check-group (make-point :i 1 :j 3) problem_1)

; (check-group (make-point :i 1 :j 1) problem_1 (list ()) (which-color 1 1 problem_1))

(write (generate-successors problem_1))

; (leader (cons (make-point :i 1 :j 2) (cons (make-point :i 0 :j 3) (cons (make-point :i 1 :j 3) nil))))


