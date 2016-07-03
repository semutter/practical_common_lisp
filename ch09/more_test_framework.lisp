(defvar *test-name* nil)
(defvar *success-test* 0)
(defvar *fail-test* 0)


(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


(defmacro deftest (name parameters &body body)
  " an abstraction of defintion for test function"
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,(with-gensyms (res succ fail)
                      `(let* ((,res (progn ,@body))
                              (,succ (first ,res))
                              (,fail (second ,res)))
                         (format t "~:[BAD~;GOOD~] ... ~a~% success ~d ~ttotal ~d~%"
                                 (= ,fail 0) *test-name* ,succ (+ ,fail ,succ))
                         ,res)))))




;; (defun test-+ ()
;;   (let ((*test-name* (append *test-name* (list 'test-+))))
;;     (let* ((result
;;             (check
;;               (= (+ 1 2) 3)
;;               (= (+ 2 3) 5)))
;;            (a (first result))
;;            (b (second result)))
;;                                         ;(list result a a b b)
;;       (format t "~:[BAD~;GOOD~] ~a~% success ~d ~t total ~d~%"
;;               (= b 0) *test-name* a (+ a b)))))
;;

(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

;; (defmacro combine-results (&body forms)
;;   (with-gensyms (success-test-number fail-test-number)
;;     `(let ((,success-test-number 0)
;;            (,fail-test-number 0))
;;        ,(loop for f in forms collect ,f)
;;        (,success-test-number ,fail-test-number))))
;;
(defun combine-results (&rest forms)
  (loop for (a b) in forms
     summing a into ta
     summing b into tb
     finally (return (list ta tb))))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  (if result '(1 0) '(0 1)))

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
