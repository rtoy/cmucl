;;; -*- Mode: Lisp; Package: LISP; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (Scott.Fahlman@CS.CMU.EDU). 
;;; **********************************************************************
;;;
;;; Garbage collection and allocation related code.
;;;
;;; Written by Christopher Hoover, Rob MacLachlan, Dave McDonald, et al.
;;; 

(in-package "EXTENSIONS")
(export '(*before-gc-hooks* *after-gc-hooks* gc gc-on gc-off
	  *bytes-consed-between-gcs* *gc-verbose* *gc-inhibit-hook*
	  *gc-notify-before* *gc-notify-after* get-bytes-consed))

(in-package "LISP")
(export '(room))



;;;; Room.

(defvar alloctable-address (int-sap %fixnum-alloctable-address)
  "A system area pointer that addresses the the alloctable.")

(defun alloc-ref (index)
  (logior (%primitive 16bit-system-ref alloctable-address (1+ index))
	  (ash (logand %type-space-mask
		       (%primitive 16bit-system-ref alloctable-address index))
	       16)))

(defun space-usage (type)
  (let ((base (ash type %alloc-ref-type-shift)))
    (values (alloc-ref base)
	    (alloc-ref (+ base 8))
	    (alloc-ref (+ base 12)))))

(defconstant type-space-names
  '#("Bignum" "Ratio" "Complex" "Short-Float" "Short-Float" "Long-Float"
     "String" "Bit-Vector" "Integer-Vector" "Code-Vector" "General-Vector"
     "Array" "Function" "Symbol" "List"))

(defun room-header ()
  (fresh-line)
  (princ "       Type        |  Dynamic  |   Static  | Read-Only |   Total")
  (terpri)
  (princ "-------------------|-----------|-----------|-----------|-----------")
  (terpri))

(defun room-summary (dynamic static read-only)
  (princ "-------------------|-----------|-----------|-----------|-----------")
  (format t "~% Totals:           |~10:D |~10:D |~10:D =~10:D~%" 
	  dynamic static read-only (+ static dynamic read-only)))

(defun describe-one-type (type dynamic static read-only)
  (declare (fixnum type dynamic static read-only))
  (format t "~18A |~10:D |~10:D |~10:D |~10:D~%"
	  (elt (the simple-vector type-space-names)
	       (the fixnum (- type (the fixnum %first-pointer-type))))
	  dynamic static read-only (the fixnum (+ static dynamic read-only))))

(defun room (&optional (x t) (object nil argp))
  "Displays information about storage allocation.
  If X is true then information is displayed broken down by types.
  If Object is supplied then just display information for objects of
  that type."
  (when x
    (let ((type (%primitive get-type object)))
      (when (or (> type %last-pointer-type)
		(< type %first-pointer-type))
	(error "Objects of type ~S have no allocated storage."
	       (type-of object)))
      (room-header)
      (cond
       (argp
	(multiple-value-bind (dyn stat ro)
			     (space-usage type)
	  (describe-one-type type dyn stat ro)))
       (t
        (let ((cum-dyn 0)
	      (cum-stat 0)
	      (cum-ro 0))
	  (do ((type %first-pointer-type (1+ type)))
	      ((= type (1+ %last-pointer-type)))
	    (if (not (or (eq type %short-+-float-type)
			 (eq type %short---float-type)))
		(multiple-value-bind (dyn stat ro)
				     (space-usage type)
		  (describe-one-type type dyn stat ro)
		  (incf cum-dyn dyn) (incf cum-stat stat) (incf cum-ro ro))))
	  (room-summary cum-dyn cum-stat cum-ro)))))))


;;;; DYNAMIC-USAGE.

;;; 
;;; DYNAMIC-USAGE -- Interface
;;;
;;; Return the number of bytes of dynamic storage allocated.
;;;
(defun dynamic-usage ()
  "Returns the number of bytes of dynamic storage currently allocated."
  (system:%primitive dynamic-space-in-use))


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
;;; setting *NEED-TO-COLLECT-GARBAGE* to T.
;;; 
(defvar *gc-trigger* default-bytes-consed-between-gcs)



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


;;;; Stack grovelling:

;;; VECTOR-ALLOC-END  --  Internal
;;;
;;;    Return a pointer to past the end of the memory allocated for a
;;; vector-like object.
;;;
(defun vector-alloc-end (vec)
  (%primitive pointer+
	      vec
	      (* (%primitive vector-word-length vec) %word-size)))


(defvar *gc-debug* nil)

;;; PRINT-RAW-ADDR  --  Interface
;;;
;;;    Print the full address of an arbitary object.
;;;
(defun print-raw-addr (x &optional (stream *standard-output*))
  (let ((fix (%primitive make-fixnum x)))
    (format stream "~4,'0X~4,'0X "
	    (logior (ash (%primitive get-type x) 11)
		    (ash (%primitive get-space x) 9)
		    (ash fix -16))
	    (logand fix #xFFFF))))


;;; GC-GROVEL-STACK  --  Internal
;;;
;;;    Locate all raw pointers on stack stack, and clobber them with something
;;; that won't cause GC to gag.  We return a list of lists of the form:
;;;    (object offset stack-location*),
;;; 
;;; where Object is some valid vector-like object pointer and Offset is an
;;; offset to be added to Object.  The result of this addition should be stored
;;; into each Stack-Location after GC completes.  We clobber the stack
;;; locations with Offset for no particular reason (might aid debugging.)
;;;
;;; There are three major steps in the algorithm:
;;;
;;;  1] Find all the distinct vector-like pointers on the stack, building a
;;;     list of all the locations that each pointer is stored in.  We do this
;;;     using two hash-tables: the one for code pointers is separate, since
;;;     they must be special-cased.
;;;
;;;     Note that we do our scan downward from the current CONT, and thus don't
;;;     scan our own frame.  We don't want to modify the frame for the running
;;;     function, as this is apt to cause problems.  It isn't necessary to
;;;     grovel the current frame because we return before GC happens.
;;;
;;;  2] Sort all of the vector-like pointers (other than code vectors), and
;;;     scan through this list finding raw pointers based on the assumption
;;;     that we will always see the true pointer to the vector header before
;;;     any raw pointers into that vector.  This exploits our GC invariant that
;;;     when an indexing temp is in use, the true object pointer must be live
;;;     on the stack or in a register.  [By now, any register indexing temp
;;;     will have been saved on the stack.]
;;;
;;;     During this scan, we also note any true vector pointers that point to a
;;;     function object.
;;;
;;;     Whenever we locate a raw vector pointer, we create a fixup for the
;;;     locations holding that pointer and then clobber the locations.
;;;
;;;  3] Iterate over all code pointers, clobbering the locations and
;;;     making fixups for those pointers that point inside some function object
;;;     that appears on the stack.  This exploits our GC invariant that a
;;;     *valid* code pointer only appears on the stack when some containing
;;;     function object also appears on the stack.  Note that *invalid* code
;;;     pointers may appear in the stack garbage unaccompanied by any function
;;;     object.  Such isolated code pointers are set to 0.  (Code pointers in
;;;     the heap must always point to the code vector header, and are always
;;;     considered valid.)
;;;
;;;     This different invariant for code pointers allows us to throw around
;;;     raw code pointers without clearing them when they are no longer needed.
;;;
(defun gc-grovel-stack ()
  (let ((vec-table (make-hash-table :test #'eq))
	(code-table (make-hash-table :test #'eq))
	(base (%primitive make-immediate-type 0 %control-stack-type))
	(fixups ()))
    ;;
    ;; Find all vector-like objects on the stack, putting code vectors in a
    ;; separate table. (step 1)
    (do ((sp (%primitive pointer+ (%primitive current-cont)
			 (- %stack-increment))
	     (%primitive pointer+ sp (- %stack-increment))))
	((%primitive pointer< sp base))
      (let* ((el (%primitive read-control-stack sp))
	     (el-type (%primitive get-type el)))
	
	(when (and *gc-debug* (simple-vector-p el))
	  (let ((hdr (%primitive read-control-stack el)))
	    (unless (and (fixnump hdr) (> hdr 0)
			 (<= (length el) #xFFFF)
			 (<= (%primitive get-vector-subtype el)
			     3))
	      (format t "Suspicious G-vector ")
	      (print-raw-addr el)
	      (format t "at ")
	      (print-raw-addr sp)
	      (terpri))))

	(when (and (< (%primitive get-space el) %static-space)
		   (<= %string-type el-type %function-type))
	  (push sp (gethash el
			    (if (eq el-type %code-type)
				code-table
				vec-table))))))

    (let ((vecs ())
	  (functions ()))
      (maphash #'(lambda (k v)
		   (declare (ignore v))
		   (push k vecs))
	       vec-table)

      (setq vecs
	    (sort vecs
		  #'(lambda (x y)
		      (%primitive pointer< x y))))

      ;;
      ;; Iterate over non-code vector-like pointers in order (step 2.)
      (loop
	(unless vecs (return))
	(let* ((base (pop vecs))
	       (end (vector-alloc-end base)))
	  
	  (when (and (= (%primitive get-type base) %function-type)
		     (<= %function-entry-subtype
			 (%primitive get-vector-subtype base)
			 %function-constants-subtype))
	    (push base functions))

	  (loop
	    (unless vecs (return))
	    (let ((next (first vecs)))
	      (unless (%primitive pointer< next end) (return))
	      (pop vecs)
	      
	      (let ((offset (%primitive pointer- next base))
		    (sps (gethash next vec-table)))
		(dolist (sp sps)
		  (%primitive write-control-stack sp offset))
		(push (list* base offset sps) fixups))))))

      ;;
      ;; Iterate over all code pointers (step 3.)
      (maphash #'(lambda (code-ptr sps)
		   (dolist (fun functions
				(dolist (sp sps)
				  (%primitive write-control-stack sp 0)))
		     (let* ((base (%primitive header-ref fun
					      %function-code-slot))
			    (end (vector-alloc-end base)))
		       (when (and (not (%primitive pointer< code-ptr base))
				  (%primitive pointer< code-ptr end))
			 (let ((offset (%primitive pointer- code-ptr base)))
			   (dolist (sp sps)
			     (%primitive write-control-stack sp offset))
			   (push (list* base offset sps) fixups))
			 (return)))))
	       code-table)

      (when *gc-debug*
	(dolist (f fixups)
	  (terpri)
	  (print-raw-addr (first f))
	  (format t "~X " (second f))
	  (dolist (sp (cddr f))
	    (print-raw-addr sp)))
	(terpri))

      fixups)))


;;; GC-FIXUP-STACK  --  Internal
;;;
;;;    Given a list of GC fixups as returned by GC-GROVEL-STACK, fix up all the
;;; raw pointers on the stack.
;;;
(defun gc-fixup-stack (fixups)
  (dolist (fixup fixups)
    (let ((new (%primitive pointer+ (first fixup) (second fixup))))
      (dolist (sp (cddr fixup))
	(%primitive write-control-stack sp new)))))


;;;; Internal GC

;;; %GC  --  Internal
;;;
;;; %GC is the real garbage collector.  What we do:
;;;  -- Call GC-GROVEL-STACK to locate any raw pointers on the stack.
;;;  -- Invoke the COLLECT-GARBAGE miscop, adding the amount of garbage
;;;     collected to *total-bytes-consed*.
;;;  -- Invalidate & revalidate the old spaces to free up their memory.
;;;  -- Call GC-FIXUP-STACK to restore raw pointers on the stack.
;;;
;;; *** Warning: the stack *including the current frame* is in a somewhat
;;; altered state until after GC-FIXUP-STACK is called.  Don't change a single
;;; character from the start of this function until after call to
;;; GC-FIXUP-STACK unless you really know what you are doing.
;;;
;;; It is important that we not do anything that creates raw pointers between
;;; the time we call GC-GROVEL-STACK and the time we invoke COLLECT-GARBAGE.
;;; In particular, this means no function calls.  All raw pointers on the stack
;;; have been trashed, so we cannot use any raw pointers until they have been
;;; regenerated.  In particular, we cannot return from this function, since the
;;; return PC is a raw pointer.
;;;
;;; We also can't expect the value of any variables allocated between the
;;; grovel and fixup to persist after the fixup, since the value that variable
;;; held at grovel time may have been a pointer that needed to be fixed.
;;;
(defun %gc ()
  (let* ((oldspace-base (ash (%primitive newspace-bit) 25))
	 (old-bytes (system:%primitive dynamic-space-in-use))
	 (result nil)
	 (fixups (gc-grovel-stack)))
    (%primitive clear-registers)
    (setq result (%primitive collect-garbage))
    (let ((new-bytes (system:%primitive dynamic-space-in-use)))
      (when *last-bytes-in-use*
	(incf *total-bytes-consed* (- old-bytes *last-bytes-in-use*))
	(setq *last-bytes-in-use* new-bytes)))
    (gc-fixup-stack fixups)
    (do* ((i %first-pointer-type (1+ i))
	  (this-space (logior oldspace-base (ash i 27))
		      (logior oldspace-base (ash i 27)))
	  (losing-gr nil))
	 ((= i (1+ %last-pointer-type))
	  (when losing-gr
	    (system:gr-error "While reclaiming VM" losing-gr)))
      (let ((gr (mach:vm_deallocate *task-self* this-space
				    (- #x2000000 8192))))
	(unless (eql gr mach:kern-success) (setq losing-gr gr)))
      (let ((gr (mach:vm_allocate *task-self* this-space
				  (- #x2000000 8192) nil)))
	(unless (eql gr mach:kern-success) (setq losing-gr gr))))
    result))

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
      (when (> *bytes-consed-between-gcs* *gc-trigger*)
	(setf *gc-trigger* *bytes-consed-between-gcs*))
      (when (> pre-gc-dyn-usage *gc-trigger*)
	(setf *need-to-collect-garbage* t))
      (when (or force-p
		(and *need-to-collect-garbage* (not *gc-inhibit*)))
	(setf *gc-inhibit* t) ; Set *GC-INHIBIT* to T before calling the hook
	(when (and (not force-p)
		   *gc-inhibit-hook*
		   (carefully-funcall *gc-inhibit-hook* pre-gc-dyn-usage))
	  (return-from sub-gc nil))
	(setf *gc-inhibit* nil) ; Reset *GC-INHIBIT*
	(multiple-value-bind
	    (winp old-mask)
	    (mach:unix-sigsetmask lockout-interrupts)
	  (unwind-protect
	      (progn
		(unless winp (warn "Could not set sigmask!"))
		(let ((*standard-output* *terminal-io*))
		  (when verbose-p
		    (carefully-funcall *gc-notify-before* pre-gc-dyn-usage))
		  (dolist (hook *before-gc-hooks*)
		    (carefully-funcall hook))
		  (funcall *internal-gc*)
		  (let* ((post-gc-dyn-usage (dynamic-usage))
			 (bytes-freed (- pre-gc-dyn-usage post-gc-dyn-usage)))
		    (setf *need-to-collect-garbage* nil)
		    (setf *gc-trigger*
			  (+ post-gc-dyn-usage *bytes-consed-between-gcs*))
		    (dolist (hook *after-gc-hooks*)
		      (carefully-funcall hook))
		    (when verbose-p
		      (carefully-funcall *gc-notify-after*
					 post-gc-dyn-usage bytes-freed
					 *gc-trigger*)))))
	    (when winp
	      (unless (values (mach:unix-sigsetmask old-mask))
		(warn "Could not restore sigmask!"))))))))
  nil)

;;;
;;; MAYBE-GC -- Internal
;;; 
;;; This routine is called by the allocation miscops to decide if a GC
;;; should occur.  The argument, object, is the newly allocated object
;;; which must be returned to the caller.
;;; 
(defun maybe-gc (object)
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
  (when *need-to-collect-garbage*
    (sub-gc *gc-verbose* nil))
  nil)

(defun gc-off ()
  "Disables the garbage collector."
  (setq *gc-inhibit* t)
  nil)
