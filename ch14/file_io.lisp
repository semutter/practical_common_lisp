(let ((in (open "/home/fenghan/practical_common_lisp/ch14/string.txt" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))
