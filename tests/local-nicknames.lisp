;;; Tests from package-local nicknames that aren't covered by tests
;;; from trivial-package-local-nicknames.

(defpackage :local-nicknames-tests
  (:use :cl :lisp-unit))

(in-package "LOCAL-NICKNAMES-TESTS")

(define-test issue.675.defpackage-local-nicknames-cumulative
    (:tag :issues)
  ;; Multiple :LOCAL-NICKNAMES options in one DEFPACKAGE form
  ;; accumulate; the later option must not discard the earlier one.
  (unwind-protect
       (let ((p (eval '(defpackage "ISSUE-LOCAL-NICK"
			 (:local-nicknames (:a :common-lisp))
			 (:local-nicknames (:b :extensions))))))
	 (assert-equal
	  '(("A" . "COMMON-LISP") ("B" . "EXTENSIONS"))
	  (sort (mapcar #'(lambda (entry)
			    (cons (car entry) (package-name (cdr entry))))
			(ext:package-local-nicknames p))
		#'string< :key #'car)))
    (let ((p (find-package "ISSUE-LOCAL-NICK")))
      (when p (delete-package p)))))
