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
    (setf p (cria-problema (list problem nil nil) (list #'get-successors) :objectivo? #'goal :estado= #'same-boards))
    (procura p strategy))

(defun same-boards (board1 board2)
    (equalp (car board1) (car board2)))

(defun goal (board)
    (all-nil (espalme (car board))))


(defun biggest-group-heuristic (state)
    (let ((filtered (filter (all-points 0 0 (list-length (car state)) (list-length (car (car state)))) (car state))) (max-group 0) )
        (loop for point in filtered do
            (let ((group-size (list-length (check-group point (car state)))))
                (if (> group-size max-group)
                (setf max-group group-size)))) 
    (/ 100 max-group)))



(defun espalme (ls)
    (if (null ls)
        nil
        (if (consp (car ls))
            (append (espalme (car ls)) (espalme (cdr ls)))
            (if (null (car ls))
                (espalme (cdr ls))
                (cons (car ls) (espalme (cdr ls)))))))

(defun all-nil (ls)
    (if (cdr ls)
        (if (not (car ls))
            (all-nil (cdr ls))
            nil)
        (if (not (car ls))
            T)))

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

;;; funcao que remove um elemento especifico de uma lista
;;; recebe o index e a lista
(defun remove-nth (n list)
  (loop for i in list
        for idx from 0
        unless (= idx n)
        collect i))

;;; recebe uma jogada e um tabuleiro e devolve o tabuleiro resultante
(defun apply-play (point board)
    (process-columns 0 (let-fall (change-block (check-group point board) (copy-tree board) 0))))

(defun let-fall (board)
    (loop
        (setf done T)
        (loop for i from 0 to (1- (list-length board)) doing
            (loop for j from 0 to (list-length (car board)) doing
                (let ((p1 (make-point :i i :j j))
                    (p2 (make-point :i (1+ i) :j j)))
                    (if (and (not (equal 0 (which-color p1 board))) (equal 0 (which-color p2 board)))
                        (progn
                            (setf board (change-color p1 (change-color p2 board (which-color p1 board)) 0))
                            (setf done nil))))))
        (when done (return board))))


;;; recebe o index da linha inicial e o tabuleiro
;;; devolve o tabuleiro com as linhas a zero removidas
(defun process-lines (line-index board)
    (if (equal line-index  (list-length  board))
        board
        (if (check-line line-index 0 board)
            (process-lines line-index (remove-line line-index board))
            (process-lines (1+ line-index) board))))


;;; recebe um index de controlo (0), a linha que se pretende analisar
;;; e um tabuleiro. Devolve true caso a linha que se forneceu seja 
;;; constituida apenas por zeros
(defun check-line (line index board)
    (if (equal (which-color (make-point :i line :j index) board) 0) 
        (progn
            (if (= index (1- (list-length (car board))))
                T
                (check-line line (1+ index) board)))))

;;; recece o index da linha a apagar e devolve o 
;;; tabuleiro sem a linha especifica
(defun remove-line (line board)
    (remove-nth line board))

;;; recebe o index da coluna inicial e o tabuleiro
;;; devolve o tabuleiro com as colunas a zero removidas
(defun process-columns (column-index board)
    (if (equal column-index  (list-length (car board)))
        board
        (if (check-column 0 column-index board)
            (process-columns column-index (remove-column (list-length board) column-index board))
            (process-columns (1+ column-index) board))))


;;; recebe um index de controlo (0), a coluna que se pretende analisar
;;; e um tabuleiro. Devolve true caso a coluna que se forneceu seja 
;;; constituida apenas por zeros
(defun check-column (index column board)
    (if (equal (which-color (make-point :i index :j column) board) 0) 
        (progn
            (if (= index (1- (list-length board)))
                T
                (check-column (1+ index) column board)))))


;;; recece o numero de linhas restantes a apagar a coluna, a coluna 
;;; especifica e o tabuleiro
(defun remove-column (rows-left column board)
    (if (= rows-left 1)
        (list (nconc (remove-nth column (car board)) (list nil)))
        (cons (nconc (remove-nth column (car board)) (list nil)) (remove-column (1- rows-left) column (cdr board)))))

;;; recebe um tabuleiro e gera uma lista com todos os sucessores possiveis
(defun get-successors (board)
    (generate-successors (filter (all-points 0 0 (list-length (car board)) (list-length (car (car board)))) (car board)) (car board)))

(defun generate-successors (plays board)
    (if (not (null plays))
        (cons (list (apply-play (car plays) board) (car plays) (list-length (check-group (car plays) board))) (generate-successors (cdr plays) board))))


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
                (setf point-up (make-point :i (- (point-i point) 1) :j (point-j point)))
                ;; caso a bola ainda nao esteja no grupo, temos de a adicionar
                (if (not (point-in-list point-up group)) (push point-up group))
                ;; adicionar a bola na queue para ser expandida
                (push point-up queue)))

            ;; se a bola de baixo for da mesma cor
            (if (check-down point board) (progn
                (setf point-down (make-point :i (+ (point-i point) 1) :j (point-j point)))
                ;; caso a bola ainda nao esteja no grupo, temos de a adicionar
                (if (not (point-in-list point-down group)) (push point-down group))
                ;; adicionar a bola na queue para ser expandida
                (push point-down queue)))

            ;; se a bola da esquerda for da mesma cor
            (if (check-left point board) (progn
                (setf point-left (make-point :i (point-i point) :j (- (point-j point) 1)))
                ;; caso a bola ainda nao esteja no grupo, temos de a adicionar
                (if (not (point-in-list point-left group)) (push point-left group))
                ;; adicionar a bola na queue para ser expandida
                (push point-left queue)))

            ;; se a bola da direita for da mesma cor
            (if (check-right point board) (progn
                (setf point-right (make-point :i (point-i point) :j (+ (point-j point) 1)))
                ;; caso a bola ainda nao esteja no grupo, temos de adicionar
                (if (not (point-in-list point-right group)) (push point-right group))
                ;; adicionar a bola na queue para ser expandida
                (push point-right queue))))))
    group)


;;; recebe uma lista de pontos e o tabuleiro
;;; devolve apenas os pontos lideres do tabuleiro 
;;; que produzem jogadas diferentes 
(defun filter (points board)
    (if (not (typep points 'cons))
        (if (and (not (equalp (which-color points board) nil)) (equalp points (leader (check-group points board))) (not (equal 1 (list-length (check-group points board)))))
            points)
        (if (and (not (equalp (which-color (car points) board) nil)) (equalp (car points) (leader (check-group (car points) board))) (not (equal 1 (list-length (check-group (car points) board)))))
            (cons (car points) (filter (cdr points) board))
            (filter (cdr points) board))))


;;; recebe a coordenadas de controlo, o total de linhas 
;;; e o total de colunas do tabuleiro e retorna todas as 
;;; posicoes possiveis do tabuleiro
(defun all-points (i j num-rows num-columns)
    (if (equal j (1- num-columns))
        (if (equal i (1- num-rows))
            (make-point :i i :j j)
            (cons (make-point :i i :j j) (all-points (1+ i) 0 num-rows num-columns)))
        (cons (make-point :i i :j j) (all-points i (1+ j) num-rows num-columns))))



;;; ---------------------- para testes ----------------------

; (filter (all-points 0 0 (list-length problem_1) (list-length (car problem_1))) problem_1)

; (filter (all-points 0 0 4 10) problem_1)

; (point-in-list (make-point :i 1 :j 1) (cons (make-point :i 2 :j 3) (cons (make-point :i 3 :j 3) (cons (make-point :i 1 :j 2) nil))))

; (change-block (cons (make-point :i 0 :j 0) (cons (make-point :i 0 :j 1) (cons (make-point :i 0 :j 2) nil))) problem_1 0)

; (check-group (make-point :i 1 :j 3) problem_1)

; (check-group (make-point :i 1 :j 1) problem_1 (list ()) (which-color 1 1 problem_1))

; (write (generate-successors problem_1))

; (leader (cons (make-point :i 1 :j 2) (cons (make-point :i 0 :j 3) (cons (make-point :i 1 :j 3) nil))))

; (apply-play (make-point :i 1 :j 1) problem_6)

; (goal? (apply-play (make-point :i 1 :j 1) problem_7))


