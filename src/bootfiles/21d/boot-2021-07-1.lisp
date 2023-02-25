;; Bootstrap file
;;
;; Use "bin/build.sh -B boot-2021-07-1" to build this.

(in-package "KERNEL")
(ext:without-package-locks
  (handler-bind
      ((error (lambda (c)
		;;(declare (ignore c))
		(describe c)
		(invoke-restart 'lisp::unintern-conflicting-symbols))))
    (export '(DOUBLE-FLOAT-INT-EXPONENT
	      SINGLE-FLOAT-INT-EXPONENT))))

