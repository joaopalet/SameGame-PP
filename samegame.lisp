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

(defun point-in-list (p points) 
    (loop for point in points
        ;;; i equals
        (setf iFlag (equal (point-i point) (point-i p)))
        ;;; j equals
        (setf jFlag (equal (point-j point) (point-j p)))
        (if (and iFlag jFlag) T)
    )
    nil
)

;; (write (point-in-list (make-point :i i :j j) ((make-point :i i :j j))))

;;; recebe uma posicao e um tabuleiro e devolve as posições adjacentes
;;; que tem a mesma cor
(defun check-group (i j board)


    (setf queue (list (make-point :i i :j j)))
    (setf visited (list (make-point :i i :j j)))
    (setf group (list (make-point :i i :j j)))


    (loop while (> (list-length queue) 0)

        (setf ball (pop queue))




        (if (check-up i j board)
            (progn 
                (push (make-point :i (- i 1) :j j) group)
                (write "up")
            )
        )
        (if (check-down i j board)
            (progn 
                (push (make-point :i (+ i 1) :j j) group)
                (write "down")
            )
        )
        (if (check-left i j board)
            (progn 
                (push (make-point :i i :j (- j 1)) group)
                (write "left")
            )
        )
        (if (check-right i j board)
            (progn 
                (push (make-point :i i :j (+ j 1)) group)
                (write "right")
            )
        )
        group    
    )

)


(terpri)
(terpri)
;; (resolve-same-game problem_1 strategy_1)
;; (write (which-color 3 9 problem_1))
;; (write (check-group 1 3 problem_1))
