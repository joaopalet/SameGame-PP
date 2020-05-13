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

;; (resolve-same-game problem_1 strategy_1)