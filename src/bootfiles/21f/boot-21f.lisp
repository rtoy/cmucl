;; For #318.  Define new standard-char type.
(in-package "KERNEL")
(ext:without-package-locks
(define-type-class standard-char)
(defstruct (standard-char-type
	    (:include ctype
	     (class-info (type-class-or-lose 'standard-char))
	     (:enumerable t))
	    (:constructor %make-standard-char-type ())
	    (:copier nil)))

(defun make-standard-char-type ()
  (%make-standard-char-type))
)
