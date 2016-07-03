;;;; chapter09
;;;; building a unit test framework

;; (defun test-+ ()
;;   (and
;;    (= (+ 1 2) 3)
;;    (= (+ 1 2 3) 6)
;;    (= (+ -1 -3) -4)))
;;
;; (defun test-+ ()
;;   (format t "~:[FAIL~;pass~] ... ~a~%"  (= (+ 1 2) 3) ' (= (+ 1 2) 3))
;;   (format t "~:[FAIL~;pass~] ... ~a~%"  (= (+ 1 2 3) 6) ' (= (+ 1 2 3) 6))
;;   (format t "~:[FAIL~;pass~] ... ~a~%"  (= (+ -1 -3) -4) ' (= (+ -1 -3) -4))
;;   )
;;

;; (defun test-+ ()
;;   (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
;;   (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
;;   (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
;;
;; (defmacro check (form)
;;   `(report-result ,form ',form))
;;

;; (defmacro check (&body forms)
;;   `(progn
;;      ,@(loop for form in forms collect `(report-result ,form ',form))))
;;

;; (defparameter *a* 1)
;;
;; (defun test-+ ()
;;   (check (= (+ (* *a* (* *a* 1)) 2) 3))
;;   (check (= (+ *a* 2 3) 6))
;;   (check (= (+ -1 -3) -4)))
;;

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))


(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; (defun test-+ ()
;;   (let ((*test-name* 'test-+))
;;     (check
;;       (= (+ 1 2) 3)
;;       (= (+ 1 2 3) 6)
;;       (= (+ -1 -3) -4))))
;;
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (test-arithmetic))
