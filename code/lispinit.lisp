;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/lispinit.lisp,v 1.50 1997/09/15 16:47:04 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Initialization stuff for CMU Common Lisp, plus some other random functions
;;; that we don't have any better place for.
;;; 
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package :lisp)

(export '(most-positive-fixnum most-negative-fixnum sleep
	  ++ +++ ** *** // ///))

(in-package :system)
(export '(compiler-version scrub-control-stack))

(in-package :extensions)
(export '(quit *prompt*))

(in-package :lisp)

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
(defvar *current-unwind-protect-block*)
(defvar *free-interrupt-context-index*)

); #-gengc progn


;;;; Random stuff that needs to be in the cold load which would otherwise be
;;;; byte-compiled.
;;;;
(defvar hi::*in-the-editor* nil)

;;;; Called by defmacro expanders...

;;; VERIFY-KEYWORDS -- internal
;;;
;;; Determine if key-list is a valid list of keyword/value pairs.  Do not
;;; signal the error directly, 'cause we don't know how it should be signaled.
;;; 
(defun verify-keywords (key-list valid-keys allow-other-keys)
  (do ((already-processed nil)
       (unknown-keyword nil)
       (remaining key-list (cddr remaining)))
      ((null remaining)
       (if (and unknown-keyword
		(not allow-other-keys)
		(not (lookup-keyword :allow-other-keys key-list)))
	   (values :unknown-keyword (list unknown-keyword valid-keys))
	   (values nil nil)))
    (cond ((not (and (consp remaining) (listp (cdr remaining))))
	   (return (values :dotted-list key-list)))
	  ((null (cdr remaining))
	   (return (values :odd-length key-list)))
	  ((member (car remaining) already-processed)
	   (return (values :duplicate (car remaining))))
	  ((or (eq (car remaining) :allow-other-keys)
	       (member (car remaining) valid-keys))
	   (push (car remaining) already-processed))
	  (t
	   (setf unknown-keyword (car remaining))))))

(defun lookup-keyword (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return (cadr remaining)))))
;;;
(defun keyword-supplied-p (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return t))))

(in-package "CONDITIONS")

(defvar *break-on-signals* nil
  "When (typep condition *break-on-signals*) is true, then calls to SIGNAL will
   enter the debugger prior to signalling that condition.")

(defun signal (datum &rest arguments)
  "Invokes the signal facility on a condition formed from datum and arguments.
   If the condition is not handled, nil is returned.  If
   (TYPEP condition *BREAK-ON-SIGNALS*) is true, the debugger is invoked before
   any signalling is done."
  (let ((condition (coerce-to-condition datum arguments
					'simple-condition 'signal))
        (*handler-clusters* *handler-clusters*))
    (let ((obos *break-on-signals*)
	  (*break-on-signals* nil))
      (when (typep condition obos)
	(break "~A~%Break entered because of *break-on-signals* (now NIL.)"
	       condition)))
    (loop
      (unless *handler-clusters* (return))
      (let ((cluster (pop *handler-clusters*)))
	(dolist (handler cluster)
	  (when (typep condition (car handler))
	    (funcall (cdr handler) condition)))))
    nil))

;;; COERCE-TO-CONDITION is used in SIGNAL, ERROR, CERROR, WARN, and
;;; INVOKE-DEBUGGER for parsing the hairy argument conventions into a single
;;; argument that's directly usable by all the other routines.
;;;
(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((typep datum 'condition)
	 (if arguments
	     (cerror "Ignore the additional arguments."
		     'simple-type-error
		     :datum arguments
		     :expected-type 'null
		     :format-control "You may not supply additional arguments ~
				     when giving ~S to ~S."
		     :format-arguments (list datum function-name)))
	 datum)
        ((symbolp datum) ;Roughly, (subtypep datum 'condition).
         (apply #'make-condition datum arguments))
        ((or (stringp datum) (functionp datum))
	 (make-condition default-type
                         :format-control datum
                         :format-arguments arguments))
        (t
         (error 'simple-type-error
		:datum datum
		:expected-type '(or symbol string)
		:format-control "Bad argument to ~S: ~S"
		:format-arguments (list function-name datum)))))

(defun error (datum &rest arguments)
  "Invokes the signal facility on a condition formed from datum and arguments.
   If the condition is not handled, the debugger is invoked."
  (kernel:infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments
					  'simple-error 'error))
	  (debug:*stack-top-hint* debug:*stack-top-hint*))
      (unless (and (condition-function-name condition) debug:*stack-top-hint*)
	(multiple-value-bind
	    (name frame)
	    (kernel:find-caller-name)
	  (unless (condition-function-name condition)
	    (setf (condition-function-name condition) name))
	  (unless debug:*stack-top-hint*
	    (setf debug:*stack-top-hint* frame))))
      (let ((debug:*stack-top-hint* nil))
	(signal condition))
      (invoke-debugger condition))))

;;; CERROR must take care to not use arguments when datum is already a
;;; condition object.
;;;
(defun cerror (continue-string datum &rest arguments)
  (kernel:infinite-error-protect
    (with-simple-restart
	(continue "~A" (apply #'format nil continue-string arguments))
      (let ((condition (if (typep datum 'condition)
			   datum
			   (coerce-to-condition datum arguments
						'simple-error 'error)))
	    (debug:*stack-top-hint* debug:*stack-top-hint*))
	(unless (and (condition-function-name condition)
		     debug:*stack-top-hint*)
	  (multiple-value-bind
	      (name frame)
	      (kernel:find-caller-name)
	    (unless (condition-function-name condition)
	      (setf (condition-function-name condition) name))
	    (unless debug:*stack-top-hint*
	      (setf debug:*stack-top-hint* frame))))
	(with-condition-restarts condition (list (find-restart 'continue))
	  (let ((debug:*stack-top-hint* nil))
	    (signal condition))
	  (invoke-debugger condition)))))
  nil)

(defun break (&optional (datum "Break") &rest arguments)
  "Prints a message and invokes the debugger without allowing any possibility
   of condition handling occurring."
  (kernel:infinite-error-protect
    (with-simple-restart (continue "Return from BREAK.")
      (let ((debug:*stack-top-hint*
	     (or debug:*stack-top-hint*
		 (nth-value 1 (kernel:find-caller-name)))))
	(invoke-debugger
	 (coerce-to-condition datum arguments 'simple-condition 'break)))))
  nil)

(defun warn (datum &rest arguments)
  "Warns about a situation by signalling a condition formed by datum and
   arguments.  While the condition is being signaled, a muffle-warning restart
   exists that causes WARN to immediately return nil."
  (kernel:infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments
					  'simple-warning 'warn)))
      (check-type condition warning "a warning condition")
      (restart-case (signal condition)
	(muffle-warning ()
	  :report "Skip warning."
	  (return-from warn nil)))
      (format *error-output* "~&~@<Warning:  ~3i~:_~A~:>~%" condition)))
  nil)

(in-package "LISP")


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
#+nil
(defun hexstr(thing)
  (let ((addr (kernel:get-lisp-obj-address thing))
	(str (make-string 10)))
    (setf (char str 0) #\0
	  (char str 1) #\x)
    (dotimes (i 8)
      (let* ((nib (ldb (byte 4 0) addr))
	     (chr (char "0123456789abcdef" nib)))
	(declare (type (unsigned-byte 4) nib)
		 (base-char chr))
	(setf (char str (- 9 i)) chr
	      addr (ash addr -4))))
    str))

(defun %initial-function ()
  "Gives the world a shove and hopes it spins."
  (%primitive print "In initial-function, and running.")
  #-gengc (setf *already-maybe-gcing* t)
  #-gengc (setf *gc-inhibit* t)
  #-gengc (setf *need-to-collect-garbage* nil)
  (setf *gc-verbose* #-gengc t #+gengc nil)
  (setf *before-gc-hooks* nil)
  (setf *after-gc-hooks* nil)
  #-gengc (setf unix::*interrupts-enabled* t)
  #-gengc (setf unix::*interrupt-pending* nil)
  (setf *type-system-initialized* nil)
  (setf *break-on-signals* nil)
  #+gengc (setf conditions::*handler-clusters* nil)

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
    (dolist (fun funs) #+nil (%primitive print (hexstr fun))
      (typecase fun
	(function
	 (funcall fun))
	(cons
	 (case (car fun)
	   (:load-time-value
	    (setf (svref *load-time-values* (third fun)) 
		  (funcall (second fun))))
	   (:load-time-value-fixup
	    #-gengc
	    (setf (sap-ref-32 (second fun) 0)
		  (get-lisp-obj-address
		   (svref *load-time-values* (third fun))))
	    #+gengc
	    (do-load-time-value-fixup (second fun) (third fun) (fourth fun)))
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
  ;; Note: sharpm and backq not yet loaded, so this is not the final RT.
  (setf *readtable* (copy-readtable std-lisp-readtable))

  (print-and-call stream-init)
  (print-and-call loader-init)
  (print-and-call package-init)
  (print-and-call kernel::signal-init)
  (setf (alien:extern-alien "internal_errors_enabled" alien:boolean) t)

  (set-floating-point-modes :traps '(:overflow #-x86 :underflow :invalid
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
  (let ((wot 
	 (catch '%end-of-the-world
	   (loop
	     (%top-level)
	     (write-line "You're certainly a clever child.")))))
    (unix:unix-exit wot)))

#+gengc
(defun do-load-time-value-fixup (object offset index)
  (declare (type index offset))
  (macrolet ((lose (msg)
	       `(progn
		  (%primitive print ,msg)
		  (%halt))))
    (let ((value (svref *load-time-values* index)))
      (typecase object
	(list
	 (case offset
	   (0 (setf (car object) value))
	   (1 (setf (cdr object) value))
	   (t (lose "Bogus offset in cons cell."))))
	(instance
	 (setf (%instance-ref object (- offset vm:instance-slots-offset))
	       value))
	(code-component
	 (setf (code-header-ref object offset) value))
	(simple-vector
	 (setf (svref object (- offset vm:vector-data-offset)) value))
	(t
	 (lose "Unknown kind of object for load-time-value fixup."))))))


;;;; Initialization functions:

;;; Print seems to not like x86 NPX denormal floats like
;;; least-negative-single-float, so the :underflow exceptions
;;; is disabled by default. Joe User can explicitly enable them
;;; if desired.

(defun reinit ()
  (without-interrupts
   (without-gcing
    (os-init)
    (stream-reinit)
    (kernel::signal-init)
    (gc-init)
    (setf (alien:extern-alien "internal_errors_enabled" alien:boolean) t)
    (set-floating-point-modes :traps
			      '(:overflow #-x86 :underflow :invalid
					  :divide-by-zero)))))



;;;; Miscellaneous external functions:

;;; Quit gets us out, one way or another.

(defun quit (&optional recklessly-p)
  "Terminates the current Lisp.  Things are cleaned up unless Recklessly-P is
  non-Nil."
  (if recklessly-p
      (unix:unix-exit 0)
      (throw '%end-of-the-world 0)))


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

#-x86
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

#+x86 ;; Stack grows downwards
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
	 (let ((loc (int-sap (- (sap-int ptr) (+ offset vm:word-bytes)))))
	   (cond ((= offset bytes-per-scrub-unit)
		  (look (int-sap (- (sap-int ptr) bytes-per-scrub-unit))
			0 count))
		 (t ;; need to fix bug in %set-stack-ref
		  (setf (sap-ref-32 loc 0) 0)
		  (scrub ptr (+ offset vm:word-bytes) count)))))
       (look (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (let ((loc (int-sap (- (sap-int ptr) offset))))
	   (cond ((= offset bytes-per-scrub-unit)
		  count)
		 ((zerop (stack-ref loc 0))
		  (look ptr (+ offset vm:word-bytes) count))
		 (t
		  (scrub ptr offset (+ count vm:word-bytes)))))))
    (let* ((csp (sap-int (c::control-stack-pointer-sap)))
	   (initial-offset (logand csp (1- bytes-per-scrub-unit))))
      (declare (type (unsigned-byte 32) csp))
      (scrub (int-sap (+ csp initial-offset))
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
		       (if *batch-mode*
			   (quit)
			   (let ((stream (make-synonym-stream '*terminal-io*)))
			     (setf *standard-input* stream)
			     (setf *standard-output* stream)
			     (format t "~&Received EOF on *standard-input*, ~
					switching to *terminal-io*.~%"))))
		      ((> number-of-eofs eofs-before-quit)
		       (format t "~&Received more than ~D EOFs; Aborting.~%"
			       eofs-before-quit)
		       (quit))
		      (t
		       (format t "~&Received EOF.~%")))))))))))

(defun %handled-top-level ()
  "Wrap %top-level read-eval-print loop in an error handler for *batch-mode*."
  (handler-case
      (progn
	(%top-level)
	(quit))
    (error (cond)
      (format *error-output* "Error in batch processing:~%~A" cond)
      (throw '%end-of-the-world 1))))


;;; %Halt  --  Interface
;;;
;;;    A convenient way to get into the assembly level debugger.
;;;
(defun %halt ()
  (%primitive halt))
