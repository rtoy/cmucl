(defun bar (x)
  (let (a)
    (declare (fixnum a))
    (setq a (foo x))
    a))

(defun foo (e l)
  (do ((current l (cdr current))
       ((atom current) nil))
      (when (eq (car current) e) (return current))))

(defun raz (foo)
  (let ((x (case foo
	     (:this 13)
	     (:that 9)
	     (:the-other 42))))
    (declare (fixnum x))
    (foo x)))

