;;; -*- Package: USER -*-
;;;
;;;    Set up package environment and search lists for compiler.  Also some
;;; compilation utilities.
;;;
(in-package "USER")

#+new-compiler
(proclaim '(optimize (debug-info 2)))

(in-package "EXT")
(export '(debug *gc-verbose*))

(in-package "EVAL")
(export '(internal-eval interpreted-function-p
			interpreted-function-lambda-expression
			interpreted-function-closure
			interpreted-function-name
			interpreted-function-arglist
			make-interpreted-function))
#-new-compiler
(import '*eval-stack-top* (find-package "LISP"))

#-new-compiler
(defmacro indirect-value (value-cell)
  `(car ,value-cell))

#-new-compiler
(defmacro eval-stack-local (fp offset)
  `(svref *eval-stack* (+ ,fp ,offset)))

#-new-compiler
(in-package "C" :use '("EXTENSIONS" "SYSTEM" "LISP"))

#-new-compiler
(export '(compile-for-eval lambda-eval-info-frame-size
	  lambda-eval-info-args-passed lambda-eval-info-entries
	  entry-node-info-st-top entry-node-info-nlx-tag
	  *compile-time-define-macros*))

#-new-compiler
(setq clc::*peep-enable* t)
#-new-compiler
(setq clc::*inline-enable* t)
#-new-compiler
(setq ext:*safe-defstruct-accessors* nil)

#-new-compiler
(import '(lisp::boolean lisp::enumeration))

;;; ### system patch...
#-new-compiler
(load "/../fred/usr/ram/hash.fasl")

(defun zap-sym (name pkg)
  (let ((found (find-symbol name (find-package pkg))))
    (when (and found
	       (eq (symbol-package found) (find-package pkg)))
      (unintern found pkg))))

#-new-compiler
(progn
  (zap-sym "ABORT" "C")
  (zap-sym "CONCAT-PNAMES" "LISP")
  (zap-sym "ARG" "LISP")
  (zap-sym "VAR" "LISP")
  (zap-sym "ONCE-ONLY" "COMPILER")
  (zap-sym "UNIX-PIPE" "COMPILER")
  (zap-sym "MAKE-UNIX-PIPE" "MACH")
  (zap-sym "UNIX-PIPE-P" "MACH"))
  
#-new-compiler
(let ((sym (find-symbol "%CHARACTER-TYPE" (find-package "SYSTEM"))))
  (when sym
    (makunbound sym)
    (unintern sym (find-package "SYSTEM"))))


#-new-compiler
(in-package "EXTENSIONS")
#-new-compiler
(export '(info clear-info define-info-class define-info-type))
#-new-compiler
(export '(ignorable truly-the maybe-inline))
#-new-compiler
(export '(unix-pipe make-unix-pipe unix-pipe-p))
#-new-compiler
(export '(lisp::with-compilation-unit lisp::debug-info) "LISP")

#-new-compiler
(export '(system::%g-vector-structure-name-slot
	  system::find-if-in-closure
	  system::*file-input-handlers*)
	"SYSTEM")

#-new-compiler
(let ((found (find-symbol "CONCAT-PNAMES" (find-package "LISP"))))
  (when found
    (unintern found (find-package "LISP"))))

#-new-compiler
(in-package "LISP")
#-new-compiler
(import '(
	  ct-a-val-sap ct-a-val-type ct-a-val-offset ct-a-val-size
	  ct-a-val-p ct-a-val make-ct-a-val ct-a-val-alien
	  check<= check= %alien-indirect %bind-aligned-sap
	  naturalize-integer deport-integer naturalize-boolean deport-boolean
	  sap-ref-8 sap-ref-16 sap-ref-32
	  signed-sap-ref-8 signed-sap-ref-16 signed-sap-ref-32 int-sap sap-int
	  %set-sap-ref-8 %set-sap-ref-16 %set-sap-ref-32
	  %set-alien-access %standard-char-p %string-char-p
	  
	  *alien-eval-when* make-alien alien-type alien-size alien-address
	  copy-alien dispose-alien defalien alien-value
	  alien-bind defoperator alien-index alien-indirect
	  bits bytes words long-words port perq-string
	  boolean defenumeration enumeration
	  system-area-pointer pointer alien alien-access
	  alien-assign alien-sap define-alien-stack
	  with-stack-alien null-terminated-string c-procedure
	  unstructured record-size
	  )
	(find-package "C"))

(export 'function-lambda-expression)

;;; Hack to prevent SETF from expanding these macros out of the environment,
;;; since these are functions in the new system.
;;;
#-new-compiler
(dolist (x '(sap-ref-8 sap-ref-16 sap-ref-32))
  (fmakunbound x))

(in-package "C")
(define-condition parse-unknown-type (condition)
		  (specifier))

(in-package "USER")

;;; Hack until real definition exists:
;;;
#-new-compiler
(defmacro with-compilation-unit (glue &rest body)
  (declare (ignore glue))
  `(let ((lisp::*in-compilation-unit* t))
     (declare (special lisp::*in-compilation-unit*))
     ,@body))
;;;
;;; So the real WCU won't die in bootstrap env.
#-new-compiler
(defvar lisp::*in-compilation-unit* nil)
#-new-compiler
(defun c::print-summary (a b)
  (declare (ignore a b)))


#-new-compiler
(setq lisp::*maximum-interpreter-error-checking* nil)


(setq *bytes-consed-between-gcs* 1500000)

(setq *gc-notify-before*
      #'(lambda (&rest foo)
	  (declare (ignore foo))
	  (write-char #\. *terminal-io*)
	  (force-output *terminal-io*)))

(setq *gc-notify-after* #'list)


;;;; Compile utility:

;;; Switches:
;;;
(defvar *interactive* nil) ; Batch compilation mode?
(defvar *new-compile* t) ; Use new compiler?

(defvar *log-file* nil)
(defvar *last-file-position*)
(defvar *compiled-files* (make-hash-table :test #'equal))


(defmacro with-compiler-log-file ((name) &body forms)
  `(if *interactive*
       (with-compilation-unit ()
	 ,@forms)
       (let ((*log-file* (open ,name :direction :output
			       :if-exists :append
			       :if-does-not-exist :create)))
	 (unwind-protect
	     (let ((*error-output* *log-file*)
		   (*last-file-position* (file-position *log-file*)))
	       (with-compilation-unit ()
		 ,@forms))
	   (close *log-file*)))))


(proclaim '(special lisp::*bootstrap-defmacro*))

(defun comf (name &key always-once proceed load output-file
		  ((:bootstrap-macros lisp::*bootstrap-defmacro*) nil))
  #+new-compiler
  (declare (ignore always-once))
  (when (and *log-file*
	     (> (- (file-position *log-file*) *last-file-position*) 10000))
    (setq *last-file-position* (file-position *log-file*))
    (force-output *log-file*))

  (let* ((src (pathname (concatenate 'string name ".lisp")))
	 (obj (if output-file
		  (pathname output-file)
		  (make-pathname :defaults src
				 :type (if *new-compile* "nfasl" "fasl"))))
	 (compiler #+new-compiler #'compile-file
		   #-new-compiler (if *new-compile*
				      #'c::ncompile-file
				      #'compile-file))
	 (obj-pn (probe-file obj)))

    (unless (and obj-pn
		 (>= (file-write-date obj-pn) (file-write-date src))
		 #+nil
		 (equalp (pathname-directory
			  (lisp::sub-probe-file (first (search-list src))))
			 (pathname-directory obj-pn))
		 #-new-compiler
		 (or (gethash src *compiled-files*)
		     (not always-once)))
      (write-line name)
      (format *error-output* "~2&Start time: ~A, compiling ~A.~%"
	      (ext:format-universal-time nil (get-universal-time))
	      name)
      (cond
       (*interactive*
	(funcall compiler src  :error-file nil  :output-file obj)
	(when load
	  (load name :verbose t)))
       (t
	(handler-bind ((error #'(lambda (condition)
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
				  (return-from comf))))
	  (funcall compiler src  :error-file nil  :output-file obj)
	  (when load
	    (load name :verbose t)))))
      (setf (gethash src *compiled-files*) t))

    ;; Only set after compilation so that it can be bound around the call.
    (setq lisp::*bootstrap-defmacro* nil)))
