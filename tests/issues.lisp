;;; Tests from gitlab issues

(defpackage :issues-tests
  (:use :cl :lisp-unit))

(in-package "ISSUES-TESTS")

(defun square (x)
  (expt x 2))

(define-compiler-macro square (&whole form arg)
  (declare (ignore arg))
  form)

(define-test issue.1.a
    (:tag :issues)
  (assert-equal
   '(square x)
   (funcall (compiler-macro-function 'square) '(square x) nil)))

(define-test issue.1.b
    (:tag :issues)
  (assert-equal
   '(square x)
   (funcall (compiler-macro-function 'square) '(funcall #'square x) nil)))

(define-test issue.4
  (:tag :issues)
  ;; Compile up two versions of elt.  F-LIST should get transformed to
  ;; LISP::LISP-ELT*, and F-VEC should be converted to AREF.  Both of
  ;; thse should signal errors.
  (let ((f-list (compile nil '(lambda (list n)
				(declare (type list list)
					 (optimize (speed 1) (safety 1) (compilation-speed 1)
						   (space 1) (debug 1)))
			       (elt list n))))
	(f-vec (compile nil '(lambda (vec n)
			       (declare (type (simple-array * (*)) vec)
					(optimize (speed 1) (safety 1) (compilation-speed 1)
						  (space 1) (debug 1)))
			      (elt vec n)))))
    ;; Errors because the index is beyond the end of the sequence
    (assert-error 'lisp::index-too-large-error (funcall f-list (list 0 1 2) 3))
    (assert-error 'type-error (funcall f-vec (make-array 3 :initial-contents '(0 1 2)) 3))
    ;; Errors because the index is negative.
    (assert-error 'type-error (funcall f-list (list 0 1 2) -1))
    (assert-error 'type-error (funcall f-vec (make-array 3 :initial-contents '(0 1 2)) -1))))

(define-test issue.4.setters
  (:tag :issues)
  ;; Compile up two versions of (SETF ELT).  F-LIST should get transformed to
  ;; %SETELT, and F-VEC should be converted to (SETF AREF).  Both of
  ;; thse should signal errors.
  (let ((s-list (compile nil '(lambda (list n new)
				(declare (type list list))
				(setf (elt list n) new))))
	(s-vec (compile nil '(lambda (vec n new)
			       (declare (type (simple-array * (*)) vec))
			       (setf (elt vec n) new)))))
    ;; Errors because the index is beyond the end of the sequence
    (assert-error 'type-error (funcall s-list (list 0 1 2) 3 99))
    (assert-error 'type-error (funcall s-vec (make-array 3 :initial-contents '(0 1 2)) 3 99))
    ;; Errors because the index is negative.
    (assert-error 'type-error (funcall s-list (list 0 1 2) -1 99))
    (assert-error 'type-error (funcall s-vec (make-array 3 :initial-contents '(0 1 2)) -1 99))))


;; Functions for testing issue-3
(defun sqr (x)
  (expt x 2))

(define-compiler-macro sqr (x)
  `(expt ,x 2))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-square-check (&body body &environment env)
  (let ((text (if (compiler-macro-function 'sqr env)
                  "Yes"
                  "No")))
    `(progn
       (format t "SQUARE compiler macro present: ~A.~%" ,text)
       ,@body))))


(defun test/absent ()
  (with-square-check
    (sqr 2)))

(defun test/present ()
  (flet ((sqr (x)
           (print (expt x 3))))
    (with-square-check
      (sqr 2))))

(define-test issue.3
    (:tag :issues)
  (assert-prints "SQUARE compiler macro present: Yes."
		 (test/absent))
  (assert-prints "SQUARE compiler macro present: No.

8"
		 (test/present)))

(defmacro xpop (place &environment env)
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
      (if ,(cdr new) (error "Can't expand this."))
      (prog1 (car ,(car new))
    (setq ,(car new) (cdr ,(car new)))
    ,setter))))

(defsetf frob (x) (value) 
     `(setf (car ,x) ,value))

(define-test issue.7
    (:tag :issues)
  (assert-error 'error
		(let ((z (list 1 2)))
		  (flet ((frob (x) (cdr x)))
		    (xpop (frob z))))))

(define-test issue.10-unsigned-byte-4
    (:tag :issues)
  (macrolet
      ((compiled-test-function (constant-index)
	 ;; Compile the test function from the issue.
	 (compile nil `(lambda (v x)
			 (declare (type (integer 0 5) v)
				  (optimize (safety 0)))
			 (setf (aref (the (simple-array (integer 0 5) (1)) x)
				     ,constant-index)
			       (the (integer 0 5) v))
			 x)))
       (make-tests ()
	 ;; Create a set of tests for a set of fixed constant indices,
	 ;; one test for each constant index from 0 to 15.
	 (let (tests)
	   (dotimes (k 16)
	     (push 
	      `(assert-equal 1
			     (aref (funcall (compiled-test-function ,k)
					    1
					    (make-array 16 :element-type '(integer 0 5) :initial-element 0))
				   ,k))
	      tests))
	   `(progn ,@(nreverse tests)))))
    (make-tests)))

(define-test issue.10-unsigned-byte-2
    (:tag :issues)
  (macrolet
      ((compiled-test-function (constant-index)
	 ;; Compile the test function from the issue.
	 (compile nil `(lambda (v x)
			 (declare (type (integer 0 2) v)
				  (optimize (safety 0)))
			 (setf (aref (the (simple-array (integer 0 2) (1)) x)
				     ,constant-index)
			       (the (integer 0 2) v))
			 x)))
       (make-tests ()
	 ;; Create a set of tests for a set of fixed constant indices,
	 ;; one test for each constant index from 0 to 31.
	 (let (tests)
	   (dotimes (k 32)
	     (push 
	      `(assert-equal 1
			     (aref (funcall (compiled-test-function ,k)
					    1
					    (make-array 32 :element-type '(integer 0 2) :initial-element 0))
				   ,k))
	      tests))
	   `(progn ,@(nreverse tests)))))
    (make-tests)))

(define-test issue.10-unsigned-byte-1
    (:tag :issues)
  (macrolet
      ((compiled-test-function (constant-index)
	 ;; Compile the test function from the issue.
	 (compile nil `(lambda (v x)
			 (declare (type (integer 0 1) v)
				  (optimize (safety 0)))
			 (setf (aref (the (simple-array (integer 0 1) (1)) x)
				     ,constant-index)
			       (the (integer 0 1) v))
			 x)))
       (make-tests ()
	 ;; Create a set of tests for a set of fixed constant indices,
	 ;; one test for each constant index from 0 to 31.
	 (let (tests)
	   (dotimes (k 64)
	     (push 
	      `(assert-equal 1
			     (aref (funcall (compiled-test-function ,k)
					    1
					    (make-array 64 :element-type '(integer 0 1) :initial-element 0))
				   ,k))
	      tests))
	   `(progn ,@(nreverse tests)))))
    (make-tests)))

(define-test issue.22
    (:tag :issues)
  (let ((tester (compile nil '(lambda (x)
			       (coerce x 'float)))))
    (assert-eql 1.0 (funcall tester 1))
    (assert-eql 2f0 (funcall tester 2f0))
    (assert-eql 3d0 (funcall tester 3d0))
    (assert-eql 4w0 (funcall tester 4w0))))
    
(define-test issue.25a
    (:tag :issues)
  ;; The original test from issue 25, modified slightly for lisp-unit
  ;; testing.
  (let* ((in-string (format nil "A line.~%And another.~%")))
    (with-output-to-string (out-stream nil)
      (with-input-from-string (in-stream in-string)
	(ext:run-program "cat" nil
			 :wait t
			 :input in-stream
			 :output out-stream))
      (let ((out-string (get-output-stream-string out-stream)))
	(assert-eql (length in-string) (length out-string))
	(assert-equal in-string out-string)))))

(define-test issue.25b
    (:tag :issues)
  ;; Modified test to verify that we only write the low 8-bits of each
  ;; string character to run-program.
  (let* ((in-string (concatenate 'string '(#\greek_small_letter_alpha
					   #\greek_small_letter_beta)))
	 (expected (map 'string #'(lambda (c)
				    (code-char (ldb (byte 8 0) (char-code c))))
			in-string)))
    (with-output-to-string (out-stream nil)
      (with-input-from-string (in-stream in-string)
	(ext:run-program "cat" nil
			 :wait t
			 :input in-stream
			 :output out-stream))
      (let ((out-string (get-output-stream-string out-stream)))
	(assert-eql (length out-string) (length out-string))
	;; For comparison, convert the strings to codes so failures are easier to read
	(assert-equal (map 'list #'char-code out-string)
		      (map 'list #'char-code expected))))))

(define-test issue.25c
    (:tag :issues)
  ;; Modified test to verify that each octet read from run-program is
  ;; read into the low 8-bits of each character of the resulting
  ;; string.
  (let* ((in-string (concatenate 'string '(#\greek_small_letter_alpha
					   #\greek_small_letter_beta)))
	 (expected (stream:string-encode in-string :utf16-be))
	 (path #p"issue25c.txt"))
    (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf16-be)
      (write-string in-string s)
      (force-output s)
      (file-position s 0)
      (with-open-file (s1 path :direction :input :element-type '(unsigned-byte 8))
	(with-output-to-string (out-stream)
	  (ext:run-program "cat" nil
			   :wait t
			   :input s1
			   :output out-stream)
	  (let ((out-string (get-output-stream-string out-stream)))
	    (assert-equal (length out-string) (length expected))
	    (assert-equal (map 'list #'char-code out-string)
			  (map 'list #'char-code expected))))))))


(define-test issue.25d
    (:tag :issues)
  ;; The original test from issue 25, but using non-ascii characters
  ;; and using string-encode/decode to verify that the output and the
  ;; input match.
  (let* ((in-string (concatenate 'string '(#\greek_small_letter_alpha
					   #\greek_small_letter_beta
					   #\greek_small_letter_gamma
					   #\greek_small_letter_delta
					   #\greek_small_letter_epsilon
					   #\greek_small_letter_zeta
					   #\greek_small_letter_eta
					   #\greek_small_letter_theta
					   #\greek_small_letter_iota
					   #\greek_small_letter_kappa
					   #\greek_small_letter_lamda))))
    (with-output-to-string (out-stream nil)
      (with-input-from-string (in-stream (stream:string-encode in-string :utf8))
	(ext:run-program "cat" nil
			 :wait t
			 :input in-stream
			 :output out-stream))
      (let ((out-string (stream:string-decode (get-output-stream-string out-stream)
					      :utf8)))
	(assert-eql (length in-string) (length out-string))
	(assert-equal in-string out-string)))))



(define-test issue.30
    (:tag :issues)
  (let* ((test-file #.(merge-pathnames #p"resources/issue-30.lisp" cl:*load-pathname*))
	 (fasl-file (compile-file-pathname test-file)))
    ;; Compiling and loading the test file should succeed without
    ;; errors.
    (assert-true (pathnamep test-file))
    (assert-true (pathnamep fasl-file))
    (assert-equalp (list fasl-file nil nil)
		  (multiple-value-list (compile-file test-file :load t)))))
