;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/lispinit.lisp,v 1.35 1993/05/22 14:01:25 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Initialization stuff for CMU Common Lisp, plus some other random functions
;;; that we don't have any better place for.
;;; 
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package "LISP" :use '("SYSTEM" "DEBUG"))

(export '(most-positive-fixnum most-negative-fixnum sleep
	  ++ +++ ** *** // ///))

(in-package "SYSTEM" :nicknames '("SYS"))
(export '(compiler-version scrub-control-stack))

(in-package "EXTENSIONS")
(export '(quit *prompt*))

(in-package "LISP")

;;; Make the error system enable interrupts.

(defconstant most-positive-fixnum #.vm:target-most-positive-fixnum
  "The fixnum closest in value to positive infinity.")

(defconstant most-negative-fixnum #.vm:target-most-negative-fixnum
  "The fixnum closest in value to negative infinity.")


;;; Random information:

(defvar *lisp-implementation-version* "4.0(?)")


;;; Must be initialized in %INITIAL-FUNCTION before the DEFVAR runs...
(declaim
  #-gengc
  (special *gc-inhibit* *already-maybe-gcing*
	   *need-to-collect-garbage* *gc-verbose*
	   *before-gc-hooks* *after-gc-hooks*
	   unix::*interrupts-enabled*
	   unix::*interrupt-pending*
	   *type-system-initialized*)
  #+gengc
  (special *gc-verbose* *before-gc-hooks* *after-gc-hooks*
	   *type-system-initialized*))


;;;; Random magic specials.


;;; These are filled in by Genesis.

#-gengc
(progn

(defvar *current-catch-block*)
(defvar *current-unwind-block*)
(defvar *free-interrupt-context-index*)

); #-gengc progn


;;; %Initial-Function is called when a cold system starts up.  First we zoom
;;; down the *Lisp-Initialization-Functions* doing things that wanted to happen
;;; at "load time."  Then we initialize the various subsystems and call the
;;; read-eval-print loop.  The top-level Read-Eval-Print loop is executed until
;;; someone (most likely the Quit function) throws to the tag
;;; %End-Of-The-World.  We quit this way so that all outstanding cleanup forms
;;; in Unwind-Protects will get executed.

(proclaim '(special *lisp-initialization-functions*
		    *load-time-values*))

(eval-when (compile)
  (defmacro print-and-call (name)
    `(progn
       (%primitive print ,(symbol-name name))
       (,name))))

(defun %initial-function ()
  "Gives the world a shove and hopes it spins."
  #-gengc (setf *already-maybe-gcing* t)
  #-gengc (setf *gc-inhibit* t)
  #-gengc (setf *need-to-collect-garbage* nil)
  (setf *gc-verbose* #-gengc t #+gengc nil)
  (setf *before-gc-hooks* nil)
  (setf *after-gc-hooks* nil)
  #-gengc (setf unix::*interrupts-enabled* t)
  #-gengc (setf unix::*interrupt-pending* nil)
  (setf *type-system-initialized* nil)
  (%primitive print "In initial-function, and running.")

  ;; Many top-level forms call INFO, (SETF INFO).
  (print-and-call c::globaldb-init)

  ;; Set up the fdefn database.
  (print-and-call fdefn-init)

  ;; Some of the random top-level forms call Make-Array, which calls Subtypep
  (print-and-call typedef-init)
  (print-and-call class-init)
  (print-and-call type-init)

  (let ((funs (nreverse *lisp-initialization-functions*)))
    (%primitive print "Calling top-level forms.")
    (dolist (fun funs)
      (typecase fun
	(function
	 (funcall fun))
	(cons
	 (case (car fun)
	   (:load-time-value
	    (setf (svref *load-time-values* (third fun)) 
		  (funcall (second fun))))
	   (:load-time-value-fixup
	    (setf (sap-ref-32 (second fun) 0)
		  (get-lisp-obj-address
		   (svref *load-time-values* (third fun)))))
	   (t
	    (%primitive print
			"Bogus fixup in *lisp-initialization-functions*")
	    (%halt))))
	(t
	 (%primitive print
		     "Bogus function in *lisp-initialization-functions*")
	 (%halt)))))
  (makunbound '*lisp-initialization-functions*)	; So it gets GC'ed.
  (makunbound '*load-time-values*)

  ;; Only do this after top level forms have run, 'cause thats where
  ;; deftypes are.
  (setf *type-system-initialized* t)

  (print-and-call os-init)
  (print-and-call filesys-init)

  (print-and-call reader-init)
  (print-and-call backq-init)
  (print-and-call sharp-init)
  ;; After the various reader subsystems have done their thing to the standard
  ;; readtable, copy it to *readtable*.
  (setf *readtable* (copy-readtable std-lisp-readtable))

  (print-and-call stream-init)
  (print-and-call loader-init)
  (print-and-call package-init)
  (print-and-call kernel::signal-init)
  (setf (alien:extern-alien "internal_errors_enabled" alien:boolean) t)
  (set-floating-point-modes :traps '(:overflow :underflow :invalid
					       :divide-by-zero))
  ;; This is necessary because some of the initial top level forms might
  ;; have changed the compliation policy in strange ways.
  (print-and-call c::proclaim-init)

  (print-and-call kernel::class-finalize)

  (%primitive print "Done initializing.")

  #-gengc (setf *already-maybe-gcing* nil)
  #+gengc (setf *gc-verbose* t)
  (terpri)
  (princ "CMU Common Lisp kernel core image ")
  (princ (lisp-implementation-version))
  (princ ".")
  (terpri)
  (princ "[You are in the LISP package.]")
  (terpri)
  (catch '%end-of-the-world
    (loop
     (%top-level)
     (write-line "You're certainly a clever child.")))
  (unix:unix-exit 0))


;;;; Initialization functions:

(defun reinit ()
  (without-interrupts
   (without-gcing
    (os-init)
    (stream-reinit)
    (kernel::signal-init)
    (gc-init)
    (setf (alien:extern-alien "internal_errors_enabled" alien:boolean) t)
    (set-floating-point-modes :traps
			      '(:overflow :underflow :invalid
					  :divide-by-zero)))))



;;;; Miscellaneous external functions:

;;; Quit gets us out, one way or another.

(defun quit (&optional recklessly-p)
  "Terminates the current Lisp.  Things are cleaned up unless Recklessly-P is
  non-Nil."
  (if recklessly-p
      (unix:unix-exit 0)
      (throw '%end-of-the-world nil)))


(defun sleep (n)
  "This function causes execution to be suspended for N seconds.  N may
  be any non-negative, non-complex number."
  (when (or (not (realp n))
	    (minusp n))
    (error "Invalid argument to SLEEP: ~S.~%~
            Must be a non-negative, non-complex number."
	   n))
  (multiple-value-bind (sec usec)
		       (if (integerp n)
			   (values n 0)
			   (values (truncate n)
				   (truncate (* n 1000000))))
    (unix:unix-select 0 0 0 0 sec usec))
  nil)


;;;; SCRUB-CONTROL-STACK


(defconstant bytes-per-scrub-unit 2048)

(defun scrub-control-stack ()
  "Zero the unused portion of the control stack so that old objects are not
   kept alive because of uninitialized stack variables."
  (declare (optimize (speed 3) (safety 0))
	   (values (unsigned-byte 20)))
  (labels
      ((scrub (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (cond ((= offset bytes-per-scrub-unit)
		(look (sap+ ptr bytes-per-scrub-unit) 0 count))
	       (t
		(setf (sap-ref-32 ptr offset) 0)
		(scrub ptr (+ offset vm:word-bytes) count))))
       (look (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (cond ((= offset bytes-per-scrub-unit)
		count)
	       ((zerop (sap-ref-32 ptr offset))
		(look ptr (+ offset vm:word-bytes) count))
	       (t
		(scrub ptr offset (+ count vm:word-bytes))))))
    (let* ((csp (sap-int (c::control-stack-pointer-sap)))
	   (initial-offset (logand csp (1- bytes-per-scrub-unit))))
      (declare (type (unsigned-byte 32) csp))
      (scrub (int-sap (- csp initial-offset))
	     (* (floor initial-offset vm:word-bytes) vm:word-bytes)
	     0))))



;;;; TOP-LEVEL loop.

(defvar / nil
  "Holds a list of all the values returned by the most recent top-level EVAL.")
(defvar // nil "Gets the previous value of / when a new value is computed.")
(defvar /// nil "Gets the previous value of // when a new value is computed.")
(defvar * nil "Holds the value of the most recent top-level EVAL.")
(defvar ** nil "Gets the previous value of * when a new value is computed.")
(defvar *** nil "Gets the previous value of ** when a new value is computed.")
(defvar + nil "Holds the value of the most recent top-level READ.")
(defvar ++ nil "Gets the previous value of + when a new value is read.")
(defvar +++ nil "Gets the previous value of ++ when a new value is read.")
(defvar - nil "Holds the form curently being evaluated.")
(defvar *prompt* "* "
  "The top-level prompt string.  This also may be a function of no arguments
   that returns a simple-string.")
(defvar *in-top-level-catcher* nil
  "True if we are within the Top-Level-Catcher.  This is used by interrupt
  handlers to see whether it is o.k. to throw.")

(defun interactive-eval (form)
  "Evaluate FORM, returning whatever it returns but adjust ***, **, *, +++, ++,
  +, ///, //, /, and -."
  (setf - form)
  (let ((results (multiple-value-list (eval form))))
    (setf /// //
	  // /
	  / results
	  *** **
	  ** *
	  * (car results)))
  (setf +++ ++
	++ +
	+ -)
  (unless (boundp '*)
    ;; The bogon returned an unbound marker.
    (setf * nil)
    (cerror "Go on with * set to NIL."
	    "EVAL returned an unbound marker."))
  (values-list /))


(defconstant eofs-before-quit 10)

(defun %top-level ()
  "Top-level READ-EVAL-PRINT loop.  Do not call this."
  (let  ((* nil) (** nil) (*** nil)
	 (- nil) (+ nil) (++ nil) (+++ nil)
	 (/// nil) (// nil) (/ nil)
	 (magic-eof-cookie (cons :eof nil))
	 (number-of-eofs 0))
    (loop
      (with-simple-restart (abort "Return to Top-Level.")
	(catch 'top-level-catcher
	  (unix:unix-sigsetmask 0)
	  (let ((*in-top-level-catcher* t))
	    (loop
	      (scrub-control-stack)
	      (fresh-line)
	      (princ (if (functionp *prompt*)
			 (funcall *prompt*)
			 *prompt*))
	      (force-output)
	      (let ((form (read *standard-input* nil magic-eof-cookie)))
		(cond ((not (eq form magic-eof-cookie))
		       (let ((results
			      (multiple-value-list (interactive-eval form))))
			 (dolist (result results)
			   (fresh-line)
			   (prin1 result)))
		       (setf number-of-eofs 0))
		      ((eql (incf number-of-eofs) 1)
		       (let ((stream (make-synonym-stream '*terminal-io*)))
			 (setf *standard-input* stream)
			 (setf *standard-output* stream)
			 (format t "~&Received EOF on *standard-input*, ~
				    switching to *terminal-io*.~%")))
		      ((> number-of-eofs eofs-before-quit)
		       (format t "~&Received more than ~D EOFs; Aborting.~%"
			       eofs-before-quit)
		       (quit))
		      (t
		       (format t "~&Received EOF.~%")))))))))))



;;; %Halt  --  Interface
;;;
;;;    A convenient way to get into the assembly level debugger.
;;;
(defun %halt ()
  (%primitive halt))
