;;; Tests from gitlab issues

(defpackage :issues-tests
  (:use :cl :lisp-unit))

(in-package "ISSUES-TESTS")

(defparameter *test-path*
  (merge-pathnames (make-pathname :name :unspecific :type :unspecific
                                  :version :unspecific)
                   *load-truename*)
  "Path to where this file is.")

(defun square (x)
  (expt x 2))

(define-compiler-macro square (&whole form arg)
  (declare (ignore arg))
  form)

(defparameter *tmp-dir*
  (merge-pathnames (make-pathname :directory '(:relative "tmp")
				  :name :unspecific
				  :type :unspecific
                                  :version :unspecific)
                   *load-truename*)
  "Directory for temporary test files.")

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

(define-test issue.25c-setup
    (:tag :issues)
  ;; Get the external format before running the test issue.25c.  See
  ;; issue #161
  ;; (https://gitlab.common-lisp.net/cmucl/cmucl/-/issues/161).
  (assert-true (stream::find-external-format :utf16-be)))

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

(define-test issue.24
    (:tag :issues)
  (let* ((test-file #.(merge-pathnames #p"resources/issue-24.lisp" cl:*load-pathname*)))
    (assert-true (compile-file test-file :load t))))

(define-test issue.32
    (:tag :issues)
  (assert-error 'kernel:simple-program-error
		(ext:run-program "cat" nil
				 :before-execve t)))

(define-test mr.15
    (:tag :issues)
  (let (directories files)
    (dolist (entry (directory (merge-pathnames "resources/mr.15/*.*" *test-path*)
                              :check-for-subdirs t
                              :follow-links nil
                              :truenamep nil))
      (let ((filename (pathname-name entry))
            (directory (first (last (pathname-directory entry)))))
        (if filename
            (push filename files)
            (push directory directories))))
    (assert-true (null (set-difference files
				       '("file" "link-to-dir"
					 "link-to-dir-in-dir" "link-to-file")
				       :test #'string-equal)))
    (assert-true (null (set-difference directories
				       '(".dir" "dir")
				       :test #'string-equal)))))

(define-test issue.36
    (:tag :issues)
  (loop for k from 1 to 24 do
    (assert-equal 0 (encode-universal-time 0 0 (- 24 k) 31 12 1899 k))))

(define-test issue.26
    (:tag :issues)
  (let ((start-time (get-universal-time)))
    (let ((p (ext:run-program "/usr/bin/env" '("sleep" "1") :wait nil)))
      (sleep 5)
      ;; For this test to be valid, the process must have finished
      ;; with a successful exit.
      (assert-true (eq (ext:process-status p) :exited))
      (assert-true (zerop (ext:process-exit-code p)))

      ;; We expect to have slept for at least 5 sec, but since
      ;; get-universal-time only has an accuracy of 1 sec, just verify
      ;; more than 3 sec have elapsed.
      (assert-true (>= (- (get-universal-time) start-time) 3)))))

(defun issue-41-tester (stop-signal)
  (let* ((p (ext:run-program "/bin/sleep" '("5") :wait nil))
	 (pid (ext:process-pid p)))
    (flet ((external-kill (pid signal)
	     (ext:run-program "/usr/bin/env"
			  (list "kill"
				(format nil "-~D" signal)
				(format nil "~D" pid)))))
      (assert-eql :running (ext:process-status p))

      (external-kill pid stop-signal)
      (sleep 1)
      (assert-eql :stopped (ext:process-status p))

      (external-kill pid unix:sigcont)
      (sleep 1)
      (assert-eql :continued (ext:process-status p))

      (external-kill pid stop-signal)
      (sleep 1)
      (assert-eql :stopped (ext:process-status p))

      (external-kill pid unix:sigcont)
      (sleep 1)
      (assert-eql :continued (ext:process-status p))

      (sleep 5)
      (assert-eql :exited (ext:process-status p)))))

;; For some reason this used to work with linux CI but now doesn't.
;; But this test passes on my Fedora and debian systems.  See issue
;; #64.  So until we figure this out, disable this test when we're
;; running a pipeline with linux, but otherwise enable it.  The
;; pipeline defines the envvar GITLAB_CI so check for that.
;;
;; This also fails on Darwin CI now.  Let's just disable the test if
;; running on CI.
;;
;; It would be better if lisp-unit had a way of marking tests as known
;; failures, but it doesn't.
#+#.(cl:if (cl:and (unix:unix-getenv "GITLAB_CI")) '(or) '(and))
(define-test issue.41.1
    (:tag :issues)
  (issue-41-tester unix:sigstop))

#+nil
(define-test issue.41.2
    (:tag :issues)
  (issue-41-tester unix:sigtstp))

(define-test issue.45
  (:tag :issues)
  ;; This depends on run-tests to setup the test directory correctly!
  (let* ((test-dir #p"test-tmp/")
	 (test-dir-name (namestring test-dir)))
    (flet ((do-test (program)
	     (with-output-to-string (s)
	       (let ((process
		      (ext:run-program program
				       (list test-dir-name)
				       :wait t :output s)))
		 ;; Verify process exited without error and that we
		 ;; got the expected output.
		 (assert-eql 0
			     (ext:process-exit-code process))
		 (assert-equal "ls-link
"
			       (get-output-stream-string s))))))
      ;; Test that absolute paths work.
      (do-test "/bin/ls")
      ;; Test that unspecfied path works.  This depends on "ls" being
      ;; somewhere in PATH.
      (do-test "ls")
      ;; Test that relative path to program works. (Issue #45).
      (do-test (concatenate 'string
			    "./"
			    test-dir-name
			    "ls-link")))))

(define-test issue.47
  (:tag :issues)
  (with-standard-io-syntax
    (assert-equal "`(,@VARS ,@VARS)"
		  (with-output-to-string (s)
		    (write (read-from-string "`(,@vars ,@vars)")
			   :pretty t
			   :stream s)))))

(define-test issue.59
  (:tag :issues)
  (let ((f (compile nil #'(lambda (z)
			    (declare (type (double-float -2d0 0d0) z))
			    (nth-value 2 (decode-float z))))))
    (assert-equal -1d0 (funcall f -1d0))))

(define-test issue.59.1-double
  (:tag :issues)
  (dolist (entry '(((-2d0 2d0) (-1073 2))
		   ((-2d0 0d0) (-1073 2))
		   ((0d0 2d0) (-1073 2))
		   ((1d0 4d0) (1 3))
		   ((-4d0 -1d0) (1 3))
		   (((0d0) (10d0)) (-1073 4))
		   ((-2f0 2f0) (-148 2))
		   ((-2f0 0f0) (-148 2))
		   ((0f0 2f0) (-148 2))
		   ((1f0 4f0) (1 3))
		   ((-4f0 -1f0) (1 3))
		   ((0f0) (10f0)) (-148 4)))
    (destructuring-bind ((arg-lo arg-hi) (result-lo result-hi))
	entry
      (assert-equalp (c::specifier-type `(integer ,result-lo ,result-hi))
		     (c::decode-float-exp-derive-type-aux
		      (c::specifier-type `(double-float ,arg-lo ,arg-hi)))))))

(define-test issue.59.1-double
  (:tag :issues)
  (dolist (entry '(((-2d0 2d0) (-1073 2))
		   ((-2d0 0d0) (-1073 2))
		   ((0d0 2d0) (-1073 2))
		   ((1d0 4d0) (1 3))
		   ((-4d0 -1d0) (1 3))
		   (((0d0) (10d0)) (-1073 4))
		   (((0.5d0) (4d0)) (0 3))))
    (destructuring-bind ((arg-lo arg-hi) (result-lo result-hi))
	entry
      (assert-equalp (c::specifier-type `(integer ,result-lo ,result-hi))
		     (c::decode-float-exp-derive-type-aux
		      (c::specifier-type `(double-float ,arg-lo ,arg-hi)))
		     arg-lo
		     arg-hi))))

(define-test issue.59.1-float
  (:tag :issues)
  (dolist (entry '(((-2f0 2f0) (-148 2))
		   ((-2f0 0f0) (-148 2))
		   ((0f0 2f0) (-148 2))
		   ((1f0 4f0) (1 3))
		   ((-4f0 -1f0) (1 3))
		   (((0f0) (10f0)) (-148 4))
		   (((0.5f0) (4f0)) (0 3))))
    (destructuring-bind ((arg-lo arg-hi) (result-lo result-hi))
	entry
      (assert-equalp (c::specifier-type `(integer ,result-lo ,result-hi))
		     (c::decode-float-exp-derive-type-aux
		      (c::specifier-type `(single-float ,arg-lo ,arg-hi)))
		     arg-lo
		     arg-hi))))

(define-test issue.60
  (:tag :issues)
  (let ((c14 (compile nil #'(lambda (x)
			      (fround (the (member 1.0 2d0) x))))))
    (assert-equalp
     (values 1.0 0.0)
     (funcall c14 1.0))
    (assert-equalp
     (values 2d0 0d0)
     (funcall c14 2d0))))

(define-test issue.58
  (:tag :issues)
  (let ((c9 (compile nil #'(lambda (x)
			     (= (the (eql 1.0d0) x) #c(1/2 1/2))))))
    (assert-false (funcall c9 1.d0))))

(define-test issue.61
  (:tag :issues)
  ;; Verifies that the compiler doesn't segfault and that we return
  ;; the correct value.
  (assert-false
   (funcall (compile nil '(lambda () (array-has-fill-pointer-p #*10))))))

(define-test issue.62
  (:tag :issues)
  ;; Verifies that the compiler doesn't segfault and that we return
  ;; the correct value.
  (assert-false
   (funcall (compile nil '(lambda () (array-displacement "aaaaaaaa"))))))

(define-test issue.101
    (:tag :issues)
  ;; Verifies that we don't get unexpected overflow.  The actual value
  ;; is not really important.  The important part is no overflow is
  ;; signaled.
  ;;
  ;; See https://gitlab.common-lisp.net/cmucl/cmucl/-/issues/101 for
  ;; more details.
  (assert-equalp
   3.0380154777955097d205
   (expt 1.7976931348623157d308 0.6666)))

(define-test issue.121
    (:tag :issues)
  ;; Output should only have one newline character in it.  Previously,
  ;; we printed two.
  (assert-equalp
   (concatenate 'string "xxx" (string #\Newline))
   (let ((a (make-array 0 :element-type 'character :fill-pointer 0
                              :adjustable t)))
           (with-output-to-string (s a)
             (format s "xxx")
             (terpri s)
             (fresh-line s))
           a)))

(define-test issue.127
    (:tag :issues)
  ;; Let's just start at uid 10000 and keep going up until we fail.
  ;; There should be no segfaults when we find an invalid uid.
  (loop for uid from 10000
	with user-info = (unix:unix-getpwuid uid)
	while user-info
	finally (assert-false user-info)))

(define-test issue.132.1
    (:tag :issues)
  ;; From a message on cmucl-imp 2008/06/01.  If "d1" is a directory,
  ;; (rename "d1" "d2") should rename the directory "d1" to "d2".
  ;; Previously that produced an error trying to rename "d1" to
  ;; "d1/d2".
  ;;
  ;; Create the test directory (that is a subdirectory of "dir").
  (assert-true (ensure-directories-exist "dir/orig-dir/"))
  (let ((*default-pathname-defaults* (merge-pathnames "dir/" (ext:default-directory))))
    (multiple-value-bind (defaulted-new-name old-truename new-truename)
	;; Rename "dir/orig-dir" to "orig/new-dir".
	(rename-file "orig-dir/" "new-dir")
      (let ((orig (merge-pathnames
		   (make-pathname :directory '(:relative "orig-dir"))))
	    (new (merge-pathnames
		  (make-pathname :directory '(:relative "new-dir")))))
	;; Ensure that the rename worked and that the returned values
	;; have the expected values.
	(assert-true defaulted-new-name)
	(assert-equalp old-truename orig)
	(assert-equalp new-truename new)))))

(define-test issue.132.2
    (:tag :issues)
  (assert-true (ensure-directories-exist "dir/orig.dir/"))
  (let ((*default-pathname-defaults* (merge-pathnames "dir/" (ext:default-directory))))
    (multiple-value-bind (defaulted-new-name old-truename new-truename)
	;; Rename "dir/orig.dir" to "orig/new-dir".  Since the
	;; original name has a pathname-name of "orig" and a
	;; pathname-type of "dir", the new file name is merged to
	;; produce a pathname-name of "new" with a pathname-type of
	;; "dir".
	(rename-file "orig.dir" "new")
      (let ((orig (merge-pathnames
		   (make-pathname :directory '(:relative "orig.dir"))))
	    (new (merge-pathnames
		  (make-pathname :directory '(:relative "new.dir")))))
	;; Ensure that the rename worked and that the returned values
	;; have the expected values.
	(assert-true defaulted-new-name)
	(assert-equalp old-truename orig)
	(assert-equalp new-truename new)))))

(define-test issue.132.3
    (:tag :issues)
  (assert-true (ensure-directories-exist "dir/orig.dir/"))
  (let ((*default-pathname-defaults* (merge-pathnames "dir/" (ext:default-directory))))
    (multiple-value-bind (defaulted-new-name old-truename new-truename)
	;; Rename "dir/orig.dir/" to "orig/new".  Note that the
	;; original name is "orig.dir/" which marks a directory so
	;; that when we merge the new name with the old to fill in
	;; missing components, there are none because the old name is
	;; a directory with no pathname-name or pathname-type, so the
	;; new name stays the same.
	(rename-file "orig.dir/" "new")
      (let ((orig (merge-pathnames
		   (make-pathname :directory '(:relative "orig.dir"))))
	    (new (merge-pathnames
		  (make-pathname :directory '(:relative "new")))))
	;; Ensure that the rename worked and that the returned values
	;; have the expected values.
	(assert-true defaulted-new-name)
	(assert-equalp old-truename orig)
	(assert-equalp new-truename new)))))

(define-test issue.134
    (:tag :issues)
  ;; Verify that we can compute (3+4*%i)^%i (in Maxima format).  This
  ;; can be written analytically as
  ;; %i*%e^-atan(4/3)*sin(log(5))+%e^-atan(4/3)*cos(log(5)), so use
  ;; %this as the reference value.
  (let ((answer (complex (* (cos (log 5w0))
			    (exp (- (atan (float (/ 4 3) 0w0)))))
			 (* (sin (log 5w0))
			    (exp (- (atan (float (/ 4 3) 0w0))))))))
    (flet ((relerr (actual true)
	     ;; Return the relative error between ACTUAL and TRUE
	     (/ (abs (- actual true))
		(abs true))))
      (dolist (test '((#c(3 4) 3.5918w-8)
		      (#c(3.0 4) 3.5918w-8)
		      (#c(3d0 4) 9.2977w-17)
		      (#c(3w0 4) 0w0)))
	(destructuring-bind (base eps)
	    test
	  (let* ((value (expt base #c(0 1)))
		 (err (relerr value answer)))
	    (assert-true (<= err eps) base err eps)))))))

(define-test issue.130
    (:tag :issues)
  ;; Just verify that file-author works.  In particular "." should
  ;; work and not return NIL.
  (assert-true (file-author "."))
  (assert-true (file-author "bin/build.sh"))
  (assert-true
   (file-author
    (merge-pathnames 
     (concatenate 'string
		  ;; Write the test file name this way so
		  ;; that it's independent of the encoding
		  ;; used to load this file.  The name is
		  ;; "안녕하십니까".
		  '(#\Hangul_Syllable_An #\Hangul_Syllable_Nyeong #\Hangul_Syllable_Ha
		    #\Hangul_Syllable_Sib #\Hangul_Syllable_Ni #\Hangul_Syllable_Gga)
		  ".txt")
     *test-path*))))

(define-test issue.135
    (:tag :issues)
  (assert-equalp "./" (ext:unix-namestring "."))
  (unwind-protect
       (progn
	 ;; Create a test file in the current directory.
	 ;; unix-namestring requires files to exist to be able to
	 ;; return the namestring.
	 (with-open-file (f1 "foo.txt"
			     :direction :output
			     :if-exists :supersede)
	   (print "foo" f1))
	 ;; Check unix-namestring and verify that converting the
	 ;; namestring to a pathname results in the same pathname
	 ;; object as expected.
	 (let ((foo (ext:unix-namestring "foo.txt")))
	   (assert-equalp "foo.txt" foo)
	   (assert-equalp (make-pathname :name "foo" :type "txt")
			  (pathname foo)))
	 (let ((bar (ext:unix-namestring "src/code/filesys.lisp")))
	   (assert-equalp "./src/code/filesys.lisp" bar)
	   (assert-equalp (make-pathname :directory '(:relative "src" "code")
					 :name "filesys"
					 :type "lisp")
			  (pathname bar))))
    (assert-true (delete-file "foo.txt"))))

(define-test issue.139-default-external-format
    (:tag :issues)
  (assert-eq :utf-8 stream:*default-external-format*)
  ;; Find the alias for :locale, and verify it exists and verify that
  ;; the system streams have that format.
  (let ((locale-format (gethash :locale stream::*external-format-aliases*)))
    (assert locale-format)
    (assert-eq locale-format (stream-external-format sys:*stdin*))
    (assert-eq locale-format (stream-external-format sys:*stdout*))
    (assert-eq locale-format (stream-external-format sys:*stderr*))
    ;; sys:*tty* can either be an fd-stream or a two-way-stream.
    (etypecase sys:*tty*
      (system:fd-stream
       (assert-eq locale-format (stream-external-format sys:*tty*)))
      (two-way-stream
       (assert-eq locale-format
		  (stream-external-format (two-way-stream-input-stream sys:*tty*)))
       (assert-eq locale-format
		  (stream-external-format (two-way-stream-output-stream sys:*tty*)))))))

(define-test issue.139-default-external-format-read-file
    (:tag :issues)
  (let ((string (concatenate 'string
			     ;; This is "hello" in Korean
			     '(#\Hangul_syllable_an
			       #\Hangul_Syllable_Nyeong
			       #\Hangul_Syllable_Ha
			       #\Hangul_Syllable_Se
			       #\Hangul_Syllable_Yo))))
    ;; Test that opening a file for reading uses the the default :utf8
    ;; encoding.
    (with-open-file (s (merge-pathnames "utf8.txt"
					*test-path*)
		       :direction :input)
      ;; The first line should be "hello" in Hangul.
      (assert-equal (map 'list #'char-name string)
		    (map 'list #'char-name (read-line s))))))

(define-test issue.139-default-external-format-write-file
    (:tag :issues)
  ;; Test that opening a file for writing uses the default :utf8.
  ;; First write something out to the file.  Then read it back in
  ;; using an explicit format of utf8 and verifying that we got the
  ;; right contents.
  (let ((string (concatenate 'string
			     ;; This is "hello" in Korean
			     '(#\Hangul_syllable_an
			       #\Hangul_Syllable_Nyeong
			       #\Hangul_Syllable_Ha
			       #\Hangul_Syllable_Se
			       #\Hangul_Syllable_Yo))))
    (with-open-file (s (merge-pathnames "out-utf8.txt"
					*test-path*)
		       :direction :output
		       :if-exists :supersede)
      (write-line string s))
    (with-open-file (s (merge-pathnames "out-utf8.txt"
					*test-path*)
		       :direction :input
		       :external-format :utf-8)
      (assert-equal (map 'list #'char-name string)
		    (map 'list #'char-name (read-line s))))))
  
(define-test issue.139-locale-external-format
    (:tag :issues)
  ;; Just verify that :locale format exists
  (assert-true (stream::find-external-format :locale nil)))

;;; Test stream-external-format for various types of streams.

(define-test issue.140.two-way-stream
    (:tag :issues)
  (ensure-directories-exist *tmp-dir*)
  (with-open-file (in (merge-pathnames "issues.lisp" cmucl-test-runner::*load-path*)
		      :direction :input
		      :external-format :utf-8)
    (with-open-file (out (merge-pathnames "output.tst" *tmp-dir*)
			 :direction :output
			 :external-format :utf-8
			 :if-exists :supersede)
      (let ((two-way-stream (make-two-way-stream in out)))
	(assert-error 'type-error
		      (stream-external-format two-way-stream))))))

;; Test synonym-stream returns the format of the underlying stream.
(define-test issue.140.synonym-stream
    (:tag :issues)
  (with-open-file (s (merge-pathnames "issues.lisp" cmucl-test-runner::*load-path*)
		     :direction :input
		     :external-format :iso8859-1)
    (let ((syn (make-synonym-stream '*syn-stream*)))
      (setf syn s)
      (assert-equal :iso8859-1 (stream-external-format syn)))))

(define-test issue.140.broadcast-stream
    (:tag :issues)
  ;; Create 3 output streams.  The exact external formats aren't
  ;; really important here as long as they're different for each file
  ;; so we can tell if we got the right answer.
  (with-open-file (s1 (merge-pathnames "broad-1" *tmp-dir*)
		      :direction :output
		      :if-exists :supersede
		      :external-format :latin1)
    (with-open-file (s2 (merge-pathnames "broad-2" *tmp-dir*)
			:direction :output
			:if-exists :supersede
			:external-format :utf-8)
      (with-open-file (s3 (merge-pathnames "broad-3" *tmp-dir*)
			  :direction :output
			  :if-exists :supersede
			  :external-format :utf-16)
	;; The format must be the value from the last stream.
	(assert-equal :utf-16
		      (stream-external-format
		       (make-broadcast-stream s1 s2 s3)))))))

(define-test issue.150
    (:tag :issues)
  (let ((ext:*gc-verbose* nil)
	(*compile-print* nil))
    (assert-true (stream::find-external-format :euckr))
    (assert-true (stream::find-external-format :cp949))))

(define-test issue.154
    (:tag :issues)
  (let ((old-locale intl::*locale*)
	(locale "en_US.UTF-8@piglatin")
	(piglatin-text "Ethay izesay ofway away eamstray inway-ufferbay."))
    (unwind-protect
	 (progn
	   (assert-equal locale (intl:setlocale "en_US.UTF-8@piglatin"))
	   (print (intl::find-domain "cmucl" intl::*locale*))
	   (assert-equal piglatin-text (intl:dgettext "cmucl" "The size of a stream in-buffer."))
	   )
      (intl:setlocale old-locale))))

(define-test issue.158
    (:tag :issues)
  (let* ((name (string #\Hangul_Syllable_Gyek))
	 (path (make-pathname :directory (list :relative name)
			      :name name
			      :type name)))
    ;; Enable this when we implement normalization for Darwin
    #+(and nil darwin)
    (let ((expected '(4352 4456 4543)))
      ;; Tests that on Darwin the Hangul pathname has been normalized
      ;; correctly.  We fill in the directory, name, and type components
      ;; with the same thing since it shouldn't really matter.
      ;;
      ;; The expected value is the conjoining jamo for the character
      ;; #\Hangul_Syllable_Gyek.
      (assert-equal (map 'list #'char-code (second (pathname-directory path)))
		    expected)
      (assert-equal (map 'list #'char-code (pathname-name path))
		    expected)
      (assert-equal (map 'list #'char-code (pathname-type path))
		    expected))
    #-darwin
    (let ((expected (list (char-code #\Hangul_Syllable_Gyek))))
      ;; For other OSes, just assume that the pathname is unchanged.
      (assert-equal (map 'list #'char-code (second (pathname-directory path)))
		    expected)
      (assert-equal (map 'list #'char-code (pathname-name path))
		    expected)
      (assert-equal (map 'list #'char-code (pathname-type path))
		    expected))))

(define-test issue.158.dir
    (:tag :issues)
  (flet ((get-file ()
	   ;; This assumes that there is only one file in resources/darwin
	   (let ((files (directory (merge-pathnames "resources/darwin/*.txt" *test-path*))))
	     (assert-equal (length files) 1)
	     (first files))))
    (let ((f (get-file))
	  (expected-name "안녕하십니까"))
      #+darwin
      (assert-equal (pathname-name f)
		    (unicode::decompose-hangul expected-name))
      #-darwin
      (assert-equal (pathname-name f) expected-name))))
    


(define-test issue.166
    (:tag :issues)
  ;; While this tests for the correct return value, the problem was
  ;; that the compiler was miscompiling the function below and causing
  ;; an error when the function run.
  (let ((f (compile nil #'(lambda ()
			    (nth-value 1 (integer-decode-float least-positive-double-float))))))
    (assert-equal -1126 (funcall f))))



(define-test issue.167.single
    (:tag :issues)
  (let ((df-min-expo (nth-value 1 (decode-float least-positive-single-float)))
	(df-max-expo (nth-value 1 (decode-float most-positive-single-float))))
    ;; Verify that the min exponent for kernel:single-float-exponent
    ;; is the actual min exponent from decode-float.
    (assert-true (typep df-min-expo 'kernel:single-float-exponent))
    (assert-true (typep (1+ df-min-expo) 'kernel:single-float-exponent))
    (assert-false (typep (1- df-min-expo) 'kernel:single-float-exponent))

    ;; Verify that the max exponent for kernel:single-float-exponent
    ;; is the actual max exponent from decode-float.
    (assert-true (typep df-max-expo 'kernel:single-float-exponent))
    (assert-true (typep (1- df-max-expo) 'kernel:single-float-exponent))
    (assert-false (typep (1+ df-max-expo) 'kernel:single-float-exponent)))

  ;; Same as for decode-float, but for integer-decode-float.
  (let ((idf-min-expo (nth-value 1 (integer-decode-float least-positive-single-float)))
	(idf-max-expo (nth-value 1 (integer-decode-float most-positive-single-float))))
    (assert-true (typep idf-min-expo 'kernel:single-float-int-exponent))
    (assert-true (typep (1+ idf-min-expo) 'kernel:single-float-int-exponent))
    (assert-false (typep (1- idf-min-expo) 'kernel:single-float-int-exponent))

    (assert-true (typep idf-max-expo 'kernel:single-float-int-exponent))
    (assert-true (typep (1- idf-max-expo) 'kernel:single-float-int-exponent))
    (assert-false (typep (1+ idf-max-expo) 'kernel:single-float-int-exponent))))

(define-test issue.167.double
    (:tag :issues)
  (let ((df-min-expo (nth-value 1 (decode-float least-positive-double-float)))
	(df-max-expo (nth-value 1 (decode-float most-positive-double-float))))
    ;; Verify that the min exponent for kernel:double-float-exponent
    ;; is the actual min exponent from decode-float.
    (assert-true (typep df-min-expo 'kernel:double-float-exponent))
    (assert-true (typep (1+ df-min-expo) 'kernel:double-float-exponent))
    (assert-false (typep (1- df-min-expo) 'kernel:double-float-exponent))

    ;; Verify that the max exponent for kernel:double-float-exponent
    ;; is the actual max exponent from decode-float.
    (assert-true (typep df-max-expo 'kernel:double-float-exponent))
    (assert-true (typep (1- df-max-expo) 'kernel:double-float-exponent))
    (assert-false (typep (1+ df-max-expo) 'kernel:double-float-exponent)))

  ;; Same as for decode-float, but for integer-decode-float.
  (let ((idf-min-expo (nth-value 1 (integer-decode-float least-positive-double-float)))
	(idf-max-expo (nth-value 1 (integer-decode-float most-positive-double-float))))
    (assert-true (typep idf-min-expo 'kernel:double-float-int-exponent))
    (assert-true (typep (1+ idf-min-expo) 'kernel:double-float-int-exponent))
    (assert-false (typep (1- idf-min-expo) 'kernel:double-float-int-exponent))

    (assert-true (typep idf-max-expo 'kernel:double-float-int-exponent))
    (assert-true (typep (1- idf-max-expo) 'kernel:double-float-int-exponent))
    (assert-false (typep (1+ idf-max-expo) 'kernel:double-float-int-exponent))))

(define-test issue.192.device
  (assert-true (equal (make-pathname :device :unspecific)
		      (make-pathname :device nil)))
  (assert-true (equal (make-pathname :device nil)
		      (make-pathname :device :unspecific))))

(define-test issue.192.name
  (assert-true (equal (make-pathname :name :unspecific)
		      (make-pathname :name nil)))
  (assert-true (equal (make-pathname :name nil)
		      (make-pathname :name :unspecific))))

(define-test issue.192.type
  (assert-true (equal (make-pathname :type :unspecific)
		      (make-pathname :type nil)))
  (assert-true (equal (make-pathname :type nil)
		      (make-pathname :type :unspecific))))

(define-test issue.192.version
  (assert-true (equal (make-pathname :version :newest)
		      (make-pathname :version nil)))
  (assert-true (equal (make-pathname :version nil)
		      (make-pathname :version :newest)))
  (assert-true (equal (make-pathname :version :unspecific)
		      (make-pathname :version nil)))
  (assert-true (equal (make-pathname :version nil)
		      (make-pathname :version :unspecific)))
  (assert-true (equal (make-pathname :version :unspecific)
		      (make-pathname :version :newest)))
  (assert-true (equal (make-pathname :version :newest)
		      (make-pathname :version :unspecific)))
)

(define-test issue.216.enough-namestring-relative-dir
    (:tag :issues)
  (let ((pathname #p"foo/bar.lisp"))
  (dolist (defaults '(#p"/tmp/zot/" #p"/tmp/zot/foo/"))
    (let ((enough (enough-namestring pathname defaults)))
      ;; This is the condition from the CLHS entry for enough-namestring
      (assert-equal (merge-pathnames enough defaults)
		    (merge-pathnames (parse-namestring pathname nil defaults) defaults))))))

(define-test issue.242-load-foreign
  ;; load-foreign apparently returns NIL if it succeeds.
  (assert-true (eql nil (ext:load-foreign (merge-pathnames "test-return.o" *test-path*)))))

(alien:def-alien-variable "test_arg" c-call:int)

(define-test issue.242.test-alien-return-signed-char
  (:tag :issues)
  (flet ((fun (n)
	   (setf test-arg n)
	   (alien:alien-funcall
	    (alien:extern-alien "int_to_signed_char"
				(function c-call:char))))
	 (sign-extend (n)
	   (let ((n (ldb (byte 8 0) n)))
	     (if (> n #x7f)
		 (- n #x100)
		 n))))
    (dolist (x '(99 -99 1023 -1023))
      (assert-equal (sign-extend x) (fun x) x))))

(define-test issue.242.test-alien-return-signed-short
  (:tag :issues)
  (flet ((fun (n)
	   (setf test-arg n)
	   (alien:alien-funcall
	    (alien:extern-alien "int_to_short"
				(function c-call:short))))
	 (sign-extend (n)
	   (let ((n (ldb (byte 16 0) n)))
	     (if (> n #x7fff)
		 (- n #x10000)
		 n))))
    (dolist (x '(1023 -1023 100000 -100000))
      (assert-equal (sign-extend x) (fun x) x))))

(define-test issue.242.test-alien-return-signed-int
  (:tag :issues)
  (flet ((fun (n)
	   (setf test-arg n)
	   (alien:alien-funcall
	    (alien:extern-alien "int_to_int"
				(function c-call:int)))))
    (dolist (x '(1023 -1023 #x7fffffff #x-80000000))
      (assert-equal x (fun x) x))))

(define-test issue.242.test-alien-return-unsigned-char
  (:tag :issues)
  (flet ((fun (n)
	   (setf test-arg n)
	   (alien:alien-funcall
	    (alien:extern-alien "int_to_unsigned_char"
				(function c-call:unsigned-char))))
	 (expected (n)
	   (ldb (byte 8 0) n)))
    (dolist (x '(99 -99 1023 -1023))
      (assert-equal (expected x) (fun x) x))))

(define-test issue.242.test-alien-return-unsigned-short
  (:tag :issues)
  (flet ((fun (n)
	   (setf test-arg n)
	   (alien:alien-funcall
	    (alien:extern-alien "int_to_unsigned_short"
				(function c-call:unsigned-short))))
	 (expected (n)
	   (ldb (byte 16 0) n)))
    (dolist (x '(1023 -1023 100000 -100000))
      (assert-equal (expected x) (fun x) x))))

(define-test issue.242.test-alien-return-unsigned-int
  (:tag :issues)
  (flet ((fun (n)
	   (setf test-arg n)
	   (alien:alien-funcall
	    (alien:extern-alien "int_to_unsigned_int"
				(function c-call:unsigned-int))))
	 (expected (n)
	   (ldb (byte 32 0) n)))
    (dolist (x '(1023 -1023 #x7fffffff #x-80000000))
      (assert-equal (expected x) (fun x) x))))

(define-test issue.242.test-alien-return-bool
  (:tag :issues)
  (flet ((fun (n)
	   (setf test-arg n)
	   (alien:alien-funcall
	    (alien:extern-alien "int_to_bool"
				(function c-call:char))))
	 (expected (n)
	   (if (zerop n)
	       0
	       1)))
    (dolist (x '(0 1 1000))
      (assert-equal (expected x) (fun x) x))))

(define-test issue.242.test-alien-return-bool.2
  (:tag :issues)
  (flet ((fun (n)
	   (setf test-arg n)
	   (alien:alien-funcall
	    (alien:extern-alien "int_to_bool"
				(function alien:boolean))))
	 (expected (n)
	   (not (zerop n))))
    (dolist (x '(0 1 1000))
      (assert-equal (expected x) (fun x) x))))

(define-test issue.333.if-does-not-exist
    (:tag :issues)
  ;; "unknown.lisp" should be a file that doesn't exist.  Verify that
  ;; if :IF-DOES-NOT-EXIST is non-NIL we signal an error from LOAD.
  ;; The first case is the old default value of :ERROR.
  (assert-error 'file-error
                 (load "unknown.lisp" :if-does-not-exist :error))
  ;; :IF-DOES-NOT-EXIST is a generalized BOOLEAN, so non-NIL will
  ;; signal an error too.
  (assert-error 'file-error
                 (load "unknown.lisp" :if-does-not-exist t))
  (assert-false (load "unknown.lisp" :if-does-not-exist nil)))

(define-test issue.339.646-external-format
    (:tag :issues)
  ;; Just verify that the external format :646 exists and is the same
  ;; as :iso646-us.
  (assert-true (stream::find-external-format :646 nil))
  (assert-true (eq (stream::find-external-format :646 nil)
		   (stream::find-external-format :iso646-us nil))))

