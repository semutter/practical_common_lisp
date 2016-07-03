;;;; test file for some trial

;; read-from-string

(defun calc (n)
  (do ((flag nil)
       (i 0)
       (j 0))
      ((or flag (> (* i j) n))
       (list i j))
    (cond ((and (< j 10) (< i 10)) (incf i))
          ((and (< j 10) (= i 10)) (incf j) (setf i 0))
          (t (setf flag t i 20 j 20)))))

(+ 1 2)
(calc 10)
