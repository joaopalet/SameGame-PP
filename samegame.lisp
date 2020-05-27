;;;; Projeto de Procura e Planeamento 2019/2020
;;;;
;;;; Joao Palet  - 86447
;;;; Simao Nunes - 86512

(in-package :user)

;;; compile and load
(compile-file "procura.lisp")
(load "setup")
(load "procura")

(setq start-time nil)
(setq best-state nil)
(setq current-heuristic nil)


;;; ------------------------------
;;; -------- STRUCTURES ----------
;;; ------------------------------

;;; estrutura que representa um estado
(defstruct state
    board           ; the board
    move            ; the last move taken
    move-score      ; number of pieces eliminated on the last move
    total-score     ; the total accumulated score up to the point
    sequence        ; sequence of points
)

;;; estrutura que representa uma coordenada do tabuleiro
(defstruct point
    i               ; row
    j               ; column
)



;;; ------------------------------
;;; ------- MAIN FUNCTION --------
;;; ------------------------------

;;; main function
(defun resolve-same-game (problem strategy)
    ;; initializing global variables
    (setf best-state (create-state nil nil -1 -1 nil))
    (setf start-time (get-internal-run-time))
    (setf current-heuristic #'isolated-heuristic)
    (setf p (cria-problema (create-state problem nil 0 0 nil) (list #'get-successors)
                            :objectivo? #'goal-time
                            :estado= #'same-boards 
                            :custo #'cost-function 
                            :heuristica #'mixed-heuristic))
    (procura p strategy)
    best-state)

(defun resolve-sondagem (problem)
    (values (sondagem-iterativa (create-state problem nil 0 0 nil))))

(defun multiple-sondagem (n problem)
    (let ((counter 0) (maximum nil))
        (loop while (< counter n) do
            (setf result (nth 0 (last (sondagem-iterativa (create-state problem nil 0 0 nil)))))
            (if (or (equal maximum nil) (> (state-total-score result) (state-total-score maximum)))
                (setf maximum result))
            (setf counter (1+ counter)))
    maximum))

;;; creates a state of the problem
(defun create-state (board move move-score total-score sequence)
    (make-state :board          board
		        :move           move
		        :move-score     move-score
		        :total-score    total-score
                :sequence       sequence))



;;; ------------------------------
;;; ------- COST FUNCTION --------
;;; ------------------------------

(defun cost-function (state)
    (if (equal 0 (state-move-score state))
        2
        (/ 1 (state-move-score state))))



;;; ------------------------------
;;; ---- SONDAGEM ITERATIVA ------
;;; ------------------------------

(defun sondagem-aux (state)
    (if (goal state) ; se for estado objetivo
        (list state)
        (let ( (successors (get-successors state)) ) 
            (if (equal (list-length successors) 0) ; se nao tiver filhos
                nil
                (let ((random-successor (nth (random (list-length successors)) successors)))
                    (let ( (solution (sondagem-aux random-successor)) )
                        (if (not (null solution))
                            (cons state solution)
                            nil)))))))

(defun sondagem-iterativa (state)
    (let ( (caminho nil) )
        (loop while (not caminho) do
            (setf caminho (sondagem-aux state)))
    (values caminho)))



;;; ------------------------------
;;; -------- GOAL STATE? ---------
;;; ------------------------------

(defun goal (state)
    (all-nil (espalme (state-board state))))

(defun goal-time (state)
    (let ((diff (- (get-internal-run-time) start-time)))
        (if (>= diff (* 288 internal-time-units-per-second))
            T)))

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



;;; ------------------------------
;;; ------- HEURISTICS -----------
;;; ------------------------------

;;; control heuristic
(defun heuristic-0 (state)
    1)


;;; numero de grupos no tabuleiro
(defun group-number-heuristic (state)
    (/ 1 (list-length (filter (all-points 0 0 (list-length (state-board state)) (list-length (car (state-board state)))) (state-board state)))))


;;; maior grupo no tabuleiro
(defun biggest-group-heuristic (state)
    (let ((filtered (filter (all-points 0 0 (list-length (state-board state)) (list-length (car (state-board state)))) (state-board state))) (max-group 0) )
        (loop for point in filtered do
            (let ((group-size (list-length (check-group point (state-board state)))))
                (if (> group-size max-group)
                (setf max-group group-size))))
    (if (equal max-group 0)
        2
        (/ 1 max-group)
    )))


;;; numero de pecas isoladas no tabuleiro
(defun isolated-heuristic (state)
    (list-length (filter-single-points (all-points 0 0 (list-length (state-board state)) (list-length (car (state-board state)))) (state-board state))))


;;; mix between isolated-heurstic and biggest-group-heuristic
(defun mixed-heuristic (state)
    (+ (isolated-heuristic state) (biggest-group-heuristic state)))



;;; ------------------------------
;;; ------- AUX FUNCTIONS --------
;;; ------------------------------

;;; True is state is better than best-state
(defun is-the-best (state)
    (if (>= (state-total-score state) (state-total-score best-state))
        T))


;;; recebe um tabuleiro e gera uma lista com todos os sucessores possiveis
(defun get-successors (state)
    (let ( (successors (generate-successors (filter (all-points 0 0 (list-length (state-board state)) (list-length (car (state-board state)))) (state-board state)) state)))        
        (if (> (list-length successors) 10)
            (subseq  (sort successors #'compare-successors) 0 10)
            successors
        )))

(defun generate-successors (plays state)
    (if (not (null plays))
        (let ((move-score (get-score (list-length (check-group (car plays) (state-board state))))))
            (let ((new-state (create-state (apply-play (car plays) (state-board state)) (car plays) move-score (+ (state-total-score state) move-score)  (append (state-sequence state) (list (car plays))))))
                (if (is-the-best new-state)
                    (setf best-state new-state))
                (cons new-state (generate-successors (cdr plays) state))))))


;;; compara dois sucessores
(defun compare-successors(s1 s2)
    (if (< (+ (cost-function s1) (funcall current-heuristic s1)) (+ (cost-function s2) (funcall current-heuristic s2)))
        T))


;;; are the board equal?
(defun same-boards (state1 state2)
    (equalp (state-board state1) (state-board state2)))


;;; returns the score given a number os pieces removed
(defun get-score (n-pieces)
    (expt (- n-pieces 2) 2))


;;; recebe uma jogada e um tabuleiro e devolve o tabuleiro resultante
(defun apply-play (point board)
    (process-columns 0 (let-fall (change-block (check-group point board) (copy-tree board) 0))))


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


;;;
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
        (if (and (not (equalp (which-color points board) 0)) (not (equalp (which-color points board) nil)) (equalp points (leader (check-group points board))) (not (equal 1 (list-length (check-group points board)))))
            points)
        (if (and (not (equalp (which-color (car points) board) 0)) (not (equalp (which-color (car points) board) nil)) (equalp (car points) (leader (check-group (car points) board))) (not (equal 1 (list-length (check-group (car points) board)))))
            (cons (car points) (filter (cdr points) board))
            (filter (cdr points) board))))


;;; recebe uma lista de pontos e o tabuleiro
;;; devolve apenas os pontos singleton do tabuleiro 
(defun filter-single-points (points board)
    (if (not (typep points 'cons))
        (if (and (not (equalp (which-color points board) 0)) 
                 (not (equalp (which-color points board) nil)) 
                 (equal 1 (list-length (check-group points board))))
            (list points))
        (if (and (not (equalp (which-color (car points) board) 0)) 
                 (not (equalp (which-color (car points) board) nil)) 
                 (equal 1 (list-length (check-group (car points) board)))) 
            (cons (car points) (filter-single-points (cdr points) board))
            (filter-single-points (cdr points) board))))


;;; recebe a coordenadas de controlo, o total de linhas 
;;; e o total de colunas do tabuleiro e retorna todas as 
;;; posicoes possiveis do tabuleiro
(defun all-points (i j num-rows num-columns)
    (if (equal j (1- num-columns))
        (if (equal i (1- num-rows))
            (make-point :i i :j j)
            (cons (make-point :i i :j j) (all-points (1+ i) 0 num-rows num-columns)))
        (cons (make-point :i i :j j) (all-points i (1+ j) num-rows num-columns))))