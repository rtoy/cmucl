;;; -*- Package: USER -*-
;;;
;;;    Set up package environment and search lists for compiler.  Also some
;;; compilation utilities.
;;;
(in-package "USER")


;;; DUMP-PACKAGE-STATE  --  Public
;;;
(defun dump-package-state (packages file)
  (declare (type (or list package symbol string) packages)
	   (type (or pathname symbol string) file))
  (let* ((packages (lisp::package-listify packages)))
    (collect ((forms))
      (dolist (pkg packages)
	(let ((nicks (package-nicknames pkg))
	      (name (package-name pkg))
	      (shad (package-shadowing-symbols pkg)))
	  (forms `(if (find-package ,name)
		      (rename-package ,name ,name ',nicks)
		      (make-package ,name :nicknames ',nicks :use nil)))
	  (when shad
	    (forms `(shadow ',(mapcar #'string shad) ,name)))))

      (dolist (pkg packages)
	(forms `(use-package ',(mapcar #'package-name
				       (package-use-list pkg))
			     ,(package-name pkg))))

      (dolist (old packages)
	(collect ((exports))
	  (let ((imports (make-hash-table :test #'eq)))
	    (do-symbols (sym old)
	      (let ((pkg (symbol-package sym))
		    (name (symbol-name sym)))
		(multiple-value-bind (found how)
				     (find-symbol name old)
		  (assert (and (eq found sym) how))
		  (cond
		   ((not pkg)
		    (warn "Not dumping uninterned symbol ~S." sym))
		   ((eq how :inherited))
		   (t
		    (unless (eq pkg old)
		      (pushnew name (gethash pkg imports) :test #'string=))
		    (when (eq how :external)
		      (exports name)))))))
	    (collect ((import-froms))
	      (maphash #'(lambda (pkg raw-names)
			   (let ((names (sort (delete-duplicates raw-names
								 :test
								 #'string=)
					      #'string<))
				 (pkg-name (package-name pkg)))
			     (when names
			       (import-froms `(:import-from ,pkg-name ,@names))
			       (dolist (name names)
				 (forms `(intern ,name ,pkg-name))))))
		       imports)
	      (forms `(defpackage ,(package-name old)
			,@(import-froms)
			,@(when (exports)
			    `((:export
			       ,@(sort (delete-duplicates (exports)
							  :test #'string=)
				       #'string<))))))))))

      (with-open-file (s file :direction :output :if-exists :new-version)
	(dolist (form (forms))
	  (write form :stream s :pretty t)
	  (terpri s)))))

  (values))
  

;;; COPY-PACKAGES  --  Public
;;;
(defun copy-packages (packages)
  "Rename all the of the Named packages to OLD-Name, and then create new
  packages for each name that have the same names, nicknames, imports, shadows
  and exports.  If any of the OLD-Name packages already exist, then we quietly
  do nothing."
  (let* ((packages (lisp::package-listify packages))
	 (names (mapcar #'package-name packages))
	 (new-names (mapcar #'(lambda (x)
				(concatenate 'string "OLD-" x))
			    names)))
    (unless (some #'find-package new-names)
      (collect ((new-packages))
	(flet ((trans-pkg (x)
		 (or (cdr (assoc x (new-packages))) x)))
	  (loop for pkg in packages and new in new-names do
	    (let ((nicks (package-nicknames pkg))
		  (name (package-name pkg)))
	      (rename-package pkg new)
	      (let ((new-pkg (make-package name :nicknames nicks :use nil))
		    (shad (package-shadowing-symbols pkg)))
		(when shad
		  (shadow shad new-pkg))
		(new-packages (cons pkg new-pkg)))))
	  
	  (loop for (old . new) in (new-packages) do
	    (dolist (use (package-use-list old))
	      (use-package (trans-pkg use) new)))
	  
	  (loop for (old . new) in (new-packages) do
	    (do-symbols (sym old)
	      (let ((pkg (symbol-package sym))
		    (name (symbol-name sym)))
		(multiple-value-bind (found how)
				     (find-symbol name old)
		  (assert (and (eq found sym) how))
		  (cond
		   ((not pkg)
		    (warn "Not copying uninterned symbol ~S." sym))
		   ((or (eq how :inherited)
			(and (eq how :internal) (eq pkg old))))
		   (t
		    (let* ((npkg (trans-pkg pkg))
			   (nsym (intern name npkg)))
		      (multiple-value-bind (ignore new-how)
					   (find-symbol name new)
			(declare (ignore ignore))
			(unless new-how (import nsym new)))
		      (when (eq how :external)
			(export nsym new)))))))))))))
  (values))


;;;; Compile utility:

;;; Switches:
;;;
(defvar *interactive* t) ; Batch compilation mode?

(defvar *log-file* nil)
(defvar *last-file-position*)

(defmacro with-compiler-log-file ((name &rest wcu-keys) &body forms)
  `(if *interactive*
       (with-compilation-unit (,@wcu-keys)
	 ,@forms)
       (let ((*log-file* (open ,name :direction :output
			       :if-exists :append
			       :if-does-not-exist :create)))
	 (unwind-protect
	     (let ((*error-output* *log-file*)
		   (*last-file-position* (file-position *log-file*)))
	       (with-compilation-unit (,@wcu-keys)
		 ,@forms))
	   (close *log-file*)))))


(defun comf (name &key always-once proceed load output-file assem)
  (declare (ignore always-once))
  (when (and *log-file*
	     (> (- (file-position *log-file*) *last-file-position*) 10000))
    (setq *last-file-position* (file-position *log-file*))
    (force-output *log-file*))

  (let* ((src (merge-pathnames name (make-pathname :type "lisp")))
	 (obj (if output-file
		  (pathname output-file)
		  (make-pathname :defaults src
				 :type
				 (if assem
				     "assem"
				     (c:backend-fasl-file-type c:*backend*))))))

    (unless (and (probe-file obj)
		 (>= (file-write-date obj) (file-write-date src)))
      (write-line name)
      (format *error-output* "~2&Start time: ~A, compiling ~A.~%"
	      (ext:format-universal-time nil (get-universal-time))
	      name)
      (catch 'blow-this-file
	(with-simple-restart
	    (continue "Blow this file")
	  (cond
	   (*interactive*
	    (if assem
		(c::assemble-file src :output-file obj)
		(compile-file src  :error-file nil  :output-file obj))
	    (when load
	      (load obj :verbose t)))
	   (t
	    (handler-bind
		((error #'(lambda (condition)
			    (unless (typep condition 'c::compiler-error)
			      (format *error-output* "~2&~A~2&"
				      condition)
			      (when proceed
				(format *error-output* "Proceeding...~%")
				(continue))
			      (format *error-output* "Aborting...~%")
			      (handler-case
				  (let ((*debug-io* *error-output*))
				    (debug:backtrace))
				(error (condition)
				       (declare (ignore condition))
				       (format t "Error in backtrace!~%")))
			      (format t "Error abort.~%")
			      (return-from comf)))))
	      (if assem
		  (c::assemble-file src :output-file obj)
		  (compile-file src  :error-file nil  :output-file obj))
	      (when load
		(load obj :verbose t))))))))))



;;; CAT-IF-ANYTHING-CHAGNED

(defun cat-if-anything-changed (output-file &rest input-files)
  (flet ((add-correct-type (pathname)
	   (make-pathname :type (c:backend-fasl-file-type c:*target-backend*)
			  :defaults pathname)))
    (let* ((output-file (add-correct-type output-file))
	   (write-date (file-write-date output-file))
	   (input-namestrings
	    (mapcar #'(lambda (file)
			(let ((file (add-correct-type file)))
			  (let ((src-write-date (file-write-date file)))
			    (unless src-write-date
			      (error "Missing file: ~S" file))
			    (when (and write-date
				       (> src-write-date write-date))
			      (setf write-date nil)))
			  (unix-namestring file)))
		    input-files)))
      (cond ((null write-date)
	     (format t "~S out of date.~%" (namestring output-file))
	     (run-program "/bin/cat" input-namestrings
			  :output output-file
			  :if-output-exists :supersede
			  :error t))
	    (t
	     (format t "~S up to date.~%" (namestring output-file)))))))
