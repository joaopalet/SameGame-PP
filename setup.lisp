;;;; xxxx
(in-package :user)

;;;; defining problems
(setf problem_1 '((2 1 3 2 3 3 2 3 3 3) 
                  (1 3 2 2 1 3 3 2 2 2) 
                  (1 3 1 3 2 2 2 1 2 1) 
                  (1 3 3 3 1 3 1 1 1 3)))

(setf problem_2 '((4 3 3 1 2 5 1 2 1 5) (2 4 4 4 1 5 2 4 1 2) (5 2 4 1 4 5 1 2 5 4) (1 3 1 4 2 5 2 5 4 5)))

(setf problem_3 '((3 3 3 2 1 2 3 1 3 1) (1 1 2 3 3 1 1 1 3 1) (3 3 1 2 1 1 3 2 1 1) (3 3 2 3 3 1 3 3 2 2)
(3 2 2 2 3 3 2 1 2 2) (3 1 2 2 2 2 1 2 1 3) (2 3 2 1 2 1 1 2 2 1) (2 2 3 1 1 1 3 2 1 3)
(1 3 3 1 1 2 3 1 3 1) (2 1 2 2 1 3 1 1 2 3) (2 1 1 3 3 3 1 2 3 1) (1 2 1 1 3 2 2 1 2 2)
(2 1 3 2 1 2 1 3 2 3) (1 2 1 3 1 2 2 3 2 3) (3 3 1 2 3 1 1 2 3 1)))

(setf problem_4 '((2 0 3 2 3 3 2 3 3 3) (1 0 2 2 1 3 3 2 2 2) (1 0 1 3 2 2 2 1 2 1) (1 0 3 3 1 3 1 1 1 3)))
(setf problem_5 '((2 0 3 2 3 3 2 0 0 0) (1 0 2 2 1 3 3 0 0 0) (1 0 1 3 2 2 2 0 0 0) (1 0 3 3 1 3 1 0 0 0)))
(setf problem_6 '((1 1 1 1) 
                  (1 2 3 3)
                  (1 2 3 1)))

(setf problem_7 '((1 1 1 1) 
                  (1 1 1 1)
                  (1 1 1 1)))

;;;; defining strategies
(setf strategy_1 "melhor.abordagem")
(setf strategy_2 "a*.melhor.heuristica.alternativa")
(setf strategy_3 "sondagem.iterativa")
(setf strategy_4 "abordagem.alternativa")