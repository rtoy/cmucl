;;; Tests for deftype documention and location info

(defpackage :deftype-tests
  (:use :cl :lisp-unit))

(in-package "DEFTYPE-TESTS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Capture the package.
  (defvar *test-package* *package*))

;; Various types for testing.
(deftype issue.495.no-args ()
  '(integer 0 10))

(deftype issue.495.optional (&optional low high)
  `(float ,low ,high))

(deftype issue.495.required (low high)
  `(integer ,low ,high))

(deftype issue.495.with-doc ()
  "A small non-negative integer."
  '(integer 0 10))

(define-test issue.495.lambda-list
  (:tag :issues)
  ;; lambda-list should be empty.  Make sure the lambda-list is NIL
  ;; and that it was actually recorded.
  (multiple-value-bind (result recorded-p)
      (c::info :type :lambda-list 'issue.495.no-args)
    (assert-equal nil result)
    (assert-true recorded-p))
  ;; lambda-list should match
  (assert-equal '(&optional low high)
		(c::info :type :lambda-list 'issue.495.optional))
  (assert-equal '(low high)
		(c::info :type :lambda-list 'issue.495.required)))

(define-test issue.495.doc
  (:tag :issues)
  ;; Test that the docstring is returned as expected.
  (assert-equal "A small non-negative integer."
		(documentation 'issue.495.with-doc 'type))
  (assert-equal "A small non-negative integer."
		(c::info :type :documentation 'issue.495.with-doc)))

(define-test issue.495.expander
  (:tag :issues)
  ;; Test that the type expander produces the expected output.
  (assert-equal '(integer 0 10)
		(funcall (c::info :type :expander 'issue.495.no-args)
			 '(issue.495.no-args)))
  (assert-equal '(float * *)
		(funcall (c::info :type :expander 'issue.495.optional)
			 '(issue.495.optional))))

(define-test issue.495.describe
  (:tag :issues)
  ;; Just make sure describe prints some string.  We're not testing
  ;; the contents of the string.
  (assert-true
   (stringp (with-output-to-string (*standard-output*)
	      (describe 'issue.495.no-args))))
  (assert-true
   (stringp (with-output-to-string (*standard-output*)
	      (describe 'issue.495.optional))))
  (assert-true
   (stringp (with-output-to-string (*standard-output*)
	      (describe 'issue.495.required)))))
  
(define-test issue.495.source-location
  (:tag :issues)
  ;; The source-location info is non-NIL only if we compile the
  ;; deftype form.
  ;;
  ;; Create a temp file that contains the deftype.  Compile it and
  ;; load it into this lisp.
  (with-input-from-string (s (format nil
				     "(in-package ~A)~%(deftype issue.495.locn () 'integer)"
				     (package-name *test-package*)))
    (ext:compile-from-stream s))
  ;; The source-location should be stored only in the :deftype, not
  ;; :defvar.
  (assert-true (c::info :source-location :deftype 'issue.495.locn))
  (assert-false (c::info :source-location :defvar 'issue.495.locn)))
