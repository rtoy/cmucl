;; Bootstrap file for the ef-flush changes.  Just need to change the
;; value of +ef-max+

(in-package "STREAM")

(handler-bind
    ((error (lambda (c)
	      (declare (ignore c))
	      (invoke-restart 'continue))))
  (defconstant +ef-max+ 11))

