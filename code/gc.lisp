;;; -*- Mode: Lisp; Package: LISP; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/gc.lisp,v 1.6 1991/03/17 14:25:29 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/gc.lisp,v 1.6 1991/03/17 14:25:29 wlott Exp $
;;; 
;;; Garbage collection and allocation related code.
;;;
;;; Written by Christopher Hoover, Rob MacLachlan, Dave McDonald, et al.
;;; New code for MIPS port by Christopher Hoover.
;;; 

(in-package "EXTENSIONS")
(export '(*before-gc-hooks* *after-gc-hooks* gc gc-on gc-off
	  *bytes-consed-between-gcs* *gc-verbose* *gc-inhibit-hook*
	  *gc-notify-before* *gc-notify-after* get-bytes-consed))

(in-package "LISP")
(export '(room))


;;;; DYNAMIC-USAGE and friends.

(proclaim '(special *read-only-space-free-pointer*
		    *static-space-free-pointer*))

(macrolet ((frob (lisp-fun c-var-name)
	     `(progn
		(def-c-variable ,c-var-name (unsigned-byte 32))
		(defun ,lisp-fun ()
		  (system:alien-access ,(intern (string-upcase c-var-name)))))))
  (frob read-only-space-start "read_only_space")
  (frob static-space-start "static_space")
  (frob dynamic-0-space-start "dynamic_0_space")
  (frob dynamic-1-space-start "dynamic_1_space")
  (frob control-stack-start "control_stack")
  (frob binding-stack-start "binding_stack")
  (frob current-dynamic-space-start "current_dynamic_space"))

(defun dynamic-usage ()
  (- (system:sap-int (c::dynamic-space-free-pointer))
     (current-dynamic-space-start)))

(defun static-space-usage ()
  (- (* lisp::*static-space-free-pointer* vm:word-bytes)
     (static-space-start)))

(defun read-only-space-usage ()
  (- (* lisp::*read-only-space-free-pointer* vm:word-bytes)
     (read-only-space-start)))

(defun control-stack-usage ()
  (- (system:sap-int (c::control-stack-pointer-sap)) (control-stack-start)))

(defun binding-stack-usage ()
  (- (system:sap-int (c::binding-stack-pointer-sap)) (binding-stack-start)))


(defun current-dynamic-space ()
  (let ((start (current-dynamic-space-start)))
    (cond ((= start (dynamic-0-space-start))
	   0)
	  ((= start (dynamic-1-space-start))
	   1)
	  (t
	   (error "Oh no.  The current dynamic space is missing!")))))


;;;; Room.

(defun room-maximal-info ()
  (format t "The current dynamic space is ~D.~%" (current-dynamic-space))
  (format t "Dynamic Space Usage:    ~10:D bytes.~%" (dynamic-usage))
  (format t "Read-Only Space Usage:  ~10:D bytes.~%" (read-only-space-usage))
  (format t "Static Space Usage:     ~10:D bytes.~%" (static-space-usage))
  (format t "Control Stack Usage:    ~10:D bytes.~%" (control-stack-usage))
  (format t "Binding Stack Usage:    ~10:D bytes.~%" (binding-stack-usage)))

(defun room-minimal-info ()
  (format t "Dynamic Space Usage:    ~10:D bytes.~%" (dynamic-usage)))

(defun room-intermediate-info ()
  (format t "Dynamic Space Usage:   ~10:D bytes.~%" (dynamic-usage))
  (format t "Read-Only Space Usage: ~10:D bytes.~%" (read-only-space-usage))
  (format t "Static Space Usage:    ~10:D bytes.~%" (static-space-usage)))

(defun room (&optional (verbosity :default))
  "Prints to *STANDARD-OUTPUT* information about the state of internal
  storage and its management.  The optional argument controls the
  verbosity of ROOM.  If it is T, ROOM prints out a maximal amount of
  information.  If it is NIL, ROOM prints out a minimal amount of
  information.  If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information."
  (fresh-line)
  (case verbosity
    ((t)
     (room-maximal-info))
    ((nil)
     (room-minimal-info))
    (:default
     (room-intermediate-info))
    (t
     (error "No way man!  The optional argument to ROOM must be T, NIL, ~
     or :DEFAULT.~%What do you think you are doing?"))))


;;;; GET-BYTES-CONSED.

;;;
;;; Internal State
;;; 
(defvar *last-bytes-in-use* nil)
(defvar *total-bytes-consed* 0)

;;;
;;; GET-BYTES-CONSED -- Exported
;;; 
(defun get-bytes-consed ()
  "Returns the number of bytes consed since the first time this function
  was called.  The first time it is called, it returns zero."
  (cond ((null *last-bytes-in-use*)
	 (setq *last-bytes-in-use* (dynamic-usage))
	 (setq *total-bytes-consed* 0))
	(t
	 (let ((bytes (dynamic-usage)))
	   (incf *total-bytes-consed* (- bytes *last-bytes-in-use*))
	   (setq *last-bytes-in-use* bytes))))
  *total-bytes-consed*)


;;;; Variables and Constants.

;;; The default value of *BYTES-CONSED-BETWEEN-GCS* and *GC-TRIGGER*.
;;; 
(defconstant default-bytes-consed-between-gcs 2000000)

;;; This variable is the user-settable variable that specifices the
;;; minimum amount of dynamic space which must be consed before a GC
;;; will be triggered.
;;; 
(defvar *bytes-consed-between-gcs* default-bytes-consed-between-gcs
  "This number specifies the minimum number of bytes of dynamic space
  that must be consed before the next gc will occur.")

;;; Internal trigger.  When the dynamic usage increases beyond this
;;; amount, the system notes that a garbage collection needs to occur by
;;; setting *NEED-TO-COLLECT-GARBAGE* to T.  It starts out as NIL meaning
;;; nobody has figured out what it should be yet.
;;; 
(defvar *gc-trigger* nil)


;;;
;;; The following specials are used to control when garbage collection
;;; occurs.
;;; 

;;; 
;;; *GC-INHIBIT*
;;;
;;; When non-NIL, inhibits garbage collection.
;;; 
(defvar *gc-inhibit* nil)

;;;
;;; *ALREADY-MAYBE-GCING*
;;;
;;; This flag is used to prevent recursive entry into the garbage
;;; collector.
;;; 
(defvar *already-maybe-gcing* nil)

;;; When T, indicates that the dynamic usage has exceeded the value
;;; *GC-TRIGGER*.
;;; 
(defvar *need-to-collect-garbage* nil)


;;;; GC Hooks.

;;;
;;; *BEFORE-GC-HOOKS*
;;; *AFTER-GC-HOOKS*
;;;
;;; These variables are a list of functions which are run before and
;;; after garbage collection occurs.
;;;
(defvar *before-gc-hooks* nil
  "A list of functions that are called before garbage collection occurs.
  The functions should take no arguments.")
;;; 
(defvar *after-gc-hooks* nil
  "A list of functions that are called after garbage collection occurs.
  The functions should take no arguments.")

;;;
;;; *GC-INHIBIT-HOOK*
;;; 
;;; This hook is invoked whenever SUB-GC intends to GC (unless the GC
;;; was explicitly forced by calling EXT:GC).  If the hook function
;;; returns NIL then the GC procedes; otherwise, the GC is inhibited and
;;; *GC-INHIBIT* and *NEED-TO-COLLECT-GARBAGE* are left bound to T.
;;; Presumably someone will call GC-ON later to collect the garbage.
;;;
(defvar *gc-inhibit-hook* nil
  "Should be bound to a function or NIL.  If it is a function, this
  function should take one argument, the current amount of dynamic
  usage.  The function should return NIL if garbage collection should
  continue and non-NIL if it should be inhibited.  Use with caution.")



;;;
;;; *GC-VERBOSE*
;;;
(defvar *gc-verbose* t
  "When non-NIL, causes the functions bound to *GC-NOTIFY-BEFORE* and
  *GC-NOTIFY-AFTER* to be called before and after a garbage collection
  occurs respectively.")


(defun default-gc-notify-before (bytes-in-use)
  (system:beep *standard-output*)
  (format t "~&[GC threshold exceeded with ~:D bytes in use.  ~
  Commencing GC.]~%" bytes-in-use)
  (finish-output))
;;;
(defparameter *gc-notify-before* #'default-gc-notify-before
  "This function bound to this variable is invoked before GC'ing (unless
  *GC-VERBOSE* is NIL) with the current amount of dynamic usage (in
  bytes).  It should notify the user that the system is going to GC.")

(defun default-gc-notify-after (bytes-retained bytes-freed new-trigger)
  (format t "[GC completed with ~:D bytes retained and ~:D bytes freed.]~%"
	  bytes-retained bytes-freed)
  (format t "[GC will next occur when at least ~:D bytes are in use.]~%"
	  new-trigger)
  (system:beep *standard-output*)
  (finish-output))
;;;
(defparameter *gc-notify-after* #'default-gc-notify-after
  "The function bound to this variable is invoked after GC'ing (unless
  *GC-VERBOSE* is NIL) with the amount of dynamic usage (in bytes) now
  free, the number of bytes freed by the GC, and the new GC trigger
  threshold.  The function should notify the user that the system has
  finished GC'ing.")


;;;; Internal GC

(def-c-routine ("collect_garbage" collect-garbage) (int))

(def-c-routine ("set_auto_gc_trigger" set-auto-gc-trigger)
	       (void)
  (dynamic-usage unsigned-long))

(def-c-routine ("clear_auto_gc_trigger" clear-auto-gc-trigger)
	       (void))


(defun %gc ()
  (let ((old-usage (dynamic-usage)))
    (collect-garbage)
    (let ((new-bytes (dynamic-usage)))
      (when *last-bytes-in-use*
	(incf *total-bytes-consed* (- old-usage *last-bytes-in-use*))
	(setq *last-bytes-in-use* new-bytes)))))


;;;
;;; *INTERNAL-GC*
;;;
;;; This variables contains the function that does the real GC.  This is
;;; for low-level GC experimentation.  Do not touch it if you do not
;;; know what you are doing.
;;; 
(defvar *internal-gc* #'%gc)


;;;; SUB-GC

;;;
;;; CAREFULLY-FUNCALL -- Internal
;;;
;;; Used to carefully invoke hooks.
;;; 
(defmacro carefully-funcall (function &rest args)
  `(handler-case (funcall ,function ,@args)
     (error (cond)
       (warn "(FUNCALL ~S~{ ~S~}) lost:~%~A" ',function ',args cond)
       nil)))

;;;
;;; SUB-GC -- Internal
;;;
;;; SUB-GC decides when and if to do a garbage collection.  The
;;; VERBOSE-P flag controls whether or not the notify functions are
;;; called.  The FORCE-P flags controls if a GC should occur even if the
;;; dynamic usage is not greater than *GC-TRIGGER*.
;;; 
(defun sub-gc (verbose-p force-p)
  (unless *already-maybe-gcing*
    (let* ((*already-maybe-gcing* t)
	   (pre-gc-dyn-usage (dynamic-usage)))
      (unless (integerp *bytes-consed-between-gcs*)
	(warn "The value of *BYTES-CONSED-BETWEEN-GCS*, ~S, is not an ~
	integer.  Reseting it to 2000000" *bytes-consed-between-gcs*)
	(setf *bytes-consed-between-gcs* default-bytes-consed-between-gcs))
      (when *gc-trigger*
	(when (> *bytes-consed-between-gcs* *gc-trigger*)
	  (setf *gc-trigger* *bytes-consed-between-gcs*))
	(when (> pre-gc-dyn-usage *gc-trigger*)
	  (setf *need-to-collect-garbage* t)))
      (when (or force-p
		(and *need-to-collect-garbage* (not *gc-inhibit*)))
	(setf *gc-inhibit* t) ; Set *GC-INHIBIT* to T before calling the hook
	(when (and (not force-p)
		   *gc-inhibit-hook*
		   (carefully-funcall *gc-inhibit-hook* pre-gc-dyn-usage))
	  (return-from sub-gc nil))
	(setf *gc-inhibit* nil) ; Reset *GC-INHIBIT*
	(without-interrupts
	 (let ((*standard-output* *terminal-io*))
	   (when verbose-p
	     (carefully-funcall *gc-notify-before* pre-gc-dyn-usage))
	   (dolist (hook *before-gc-hooks*)
	     (carefully-funcall hook))
	   (when *gc-trigger*
	     (clear-auto-gc-trigger))
	   (funcall *internal-gc*)
	   (let* ((post-gc-dyn-usage (dynamic-usage))
		  (bytes-freed (- pre-gc-dyn-usage post-gc-dyn-usage)))
	     (setf *need-to-collect-garbage* nil)
	     (setf *gc-trigger*
		   (+ post-gc-dyn-usage *bytes-consed-between-gcs*))
	     (set-auto-gc-trigger *gc-trigger*)
	     (dolist (hook *after-gc-hooks*)
	       (carefully-funcall hook))
	     (when verbose-p
	       (carefully-funcall *gc-notify-after*
				  post-gc-dyn-usage bytes-freed
				  *gc-trigger*))))))))
  nil)

;;;
;;; MAYBE-GC -- Internal
;;; 
;;; This routine is called by the allocation miscops to decide if a GC
;;; should occur.  The argument, object, is the newly allocated object
;;; which must be returned to the caller.
;;; 
(defun maybe-gc (&optional object)
  (sub-gc *gc-verbose* nil)
  object)

;;;
;;; GC -- Exported
;;;
;;; This is the user advertised garbage collection function.
;;; 
(defun gc (&optional (verbose-p *gc-verbose*))
  "Initiates a garbage collection.  The optional argument, VERBOSE-P,
  which defaults to the value of the variable *GC-VERBOSE* controls
  whether or not GC statistics are printed."
  (sub-gc verbose-p t))


;;;; Auxiliary Functions.


(defun gc-on ()
  "Enables the garbage collector."
  (setq *gc-inhibit* nil)
  (unless *gc-trigger*
    (setf *gc-trigger* *bytes-consed-between-gcs*)
    (set-auto-gc-trigger *gc-trigger*))
  (when *need-to-collect-garbage*
    (sub-gc *gc-verbose* nil))
  nil)

(defun gc-off ()
  "Disables the garbage collector."
  (setq *gc-inhibit* t)
  nil)
