;; (defun point-up (point board)
;;     (if (> (point-i point) 0)
;;         (make-point :i (1+ (point-i point)) :j (point-j point))))

;; (defun point-down (point board)
;;     (if (>= (point-i point) (1- (list-length board)))
;;         (make-point :i (1- (point-i point)) :j (point-j point))))

;; (defun point-left (point board)
;;     (if (> (point-j point) 0)
;;         (make-point :i (point-i point) :j (1- (point-j point)))))

;; (defun point-right (point board)
;;     (if (>= (point-j point) (1- (list-length (car board))))
;;         (make-point :i (point-i point) :j (1+ (point-j point)))))

;; (defun check-group (point board visited color)
;;     (if (and point (equalp (which-color (point-i point) (point-j point) board) color) (not (point-in-list point visited)))
;;         (append (list point) (check-group (point-up    point board) board (cons point visited) color)
;;                              (check-group (point-down  point board) board (cons point visited) color)
;;                              (check-group (point-left  point board) board (cons point visited) color)
;;                              (check-group (point-right point board) board (cons point visited) color))
;;         nil))
