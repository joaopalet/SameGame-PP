;;;; xxxx
(in-package :user)

;;;; compile and load
(compile-file "procura.lisp")
(load "setup")
(load "procura")

;;;; main function
(defun resolve-same-game (problem strategy)
    (write problem)
    (write strategy)
)

;;;; funcao que devolve a cor de uma determinada posicao
(defun which-color (row column board)
    ;; linha correspondente
    (setf row (nth row board))
    ;; cor 
    (nth column row)
)

;; (resolve-same-game problem_1 strategy_1)
;; (write (which-color 3 9 problem_1))