;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
;;; $Id: multi-proc.lisp,v 1.8 1997/12/29 03:11:31 dtc Exp $
;;;
;;; **********************************************************************
;;;
;;; Stack-group and multi-process support for CMUCL x86.
;;;

(in-package "MULTIPROCESSING")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Handle the binding stack.

;;; Undo all the bindings in the bind stack, restoring the global
;;; values.
(defun unbind-binding-stack ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (kernel:binding-stack-pointer-sap))
	 (binding-stack 
	  (sys:int-sap (alien:extern-alien "binding_stack" alien:unsigned)))
	 (size (sys:sap- binding-stack-pointer binding-stack)))
    (declare (type (unsigned-byte 29) size))
    (do ((binding size))
	((zerop binding))
      (declare (type (unsigned-byte 29) binding))
      (decf binding 8)
      (let* ((value 
	      (kernel:make-lisp-obj
	       (sys:sap-int (sys:sap-ref-sap binding-stack binding))))
	     (symbol
	      (kernel:make-lisp-obj
	       (sys:sap-int (sys:sap-ref-sap binding-stack (+ binding 4))))))
	(cond ((symbolp symbol)
	       (let ((symbol-value (c::%primitive c:fast-symbol-value symbol)))
		 #+nil
		 (format t "Undoing: ~s ~s <-> ~s~%" symbol value symbol-value)
		 (kernel:%set-symbol-value symbol value)
		 (setf (sys:sap-ref-sap binding-stack binding)
		       (sys:int-sap (kernel:get-lisp-obj-address
				     symbol-value)))))
	      (t
	       #+nil
	       (format t "Ignoring undoing: ~s ~s~%" symbol value)))))))

;;; Re-apply the bindings in a binding stack after an
;;; unbind-binding-stack.
(defun rebind-binding-stack ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (kernel:binding-stack-pointer-sap))
	 (binding-stack 
	  (sys:int-sap (alien:extern-alien "binding_stack" alien:unsigned)))
	 (size (sys:sap- binding-stack-pointer binding-stack)))
    (declare (type (unsigned-byte 29) size))
    (do ((binding 0 (+ 8 binding)))
	((= binding size))
      (declare (type (unsigned-byte 29) binding))
      (let* ((value 
	      (kernel:make-lisp-obj
	       (sys:sap-int (sys:sap-ref-sap binding-stack binding))))
	     (symbol
	      (kernel:make-lisp-obj
	       (sys:sap-int (sys:sap-ref-sap binding-stack (+ binding 4))))))
	(cond ((symbolp symbol)
	       (let ((symbol-value (c::%primitive c:fast-symbol-value symbol)))
		 #+nil
		 (format t "Rebinding: ~s ~s <-> ~s~%"
			 symbol value symbol-value)
		 (kernel:%set-symbol-value symbol value)
		 (setf (sys:sap-ref-sap binding-stack binding)
		       (sys:int-sap (kernel:get-lisp-obj-address
				     symbol-value)))))
	      (t
	       #+nil
	       (format t "Ignoring rebinding: ~s ~s~%" symbol value)))))))

(defun save-binding-stack (binding-save-stack)
  (declare (type (simple-array t (*)) binding-save-stack)
	   (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (kernel:binding-stack-pointer-sap))
	 (binding-stack 
	  (sys:int-sap (alien:extern-alien "binding_stack" alien:unsigned)))
	 (size (sys:sap- binding-stack-pointer binding-stack))
	 (vector-size (truncate size 4)))
    (declare (type (unsigned-byte 29) size))
    ;; Grow binding-save-stack if necessary.
    (when (< (length binding-save-stack) vector-size)
      (setq binding-save-stack
	    (adjust-array binding-save-stack vector-size :element-type t)))
    ;; Save the stack.
    (do ((binding 0 (+ 4 binding))
	 (index 0 (1+ index)))
	((= binding size))
      (declare (type (unsigned-byte 29) binding index))
      (setf (aref binding-save-stack index)
	    (kernel:make-lisp-obj
	     (sys:sap-int (sys:sap-ref-sap binding-stack binding)))))
    (values binding-save-stack vector-size)))

(defun restore-binding-stack (new-binding-stack size)
  (declare (type (simple-array t (*)) new-binding-stack)
	   (type (unsigned-byte 29) size)
	   (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-size (* size 4))
	 (binding-stack (alien:extern-alien "binding_stack" alien:unsigned)))
    (declare (type (unsigned-byte 32) binding-stack-size binding-stack))
    (setf (kernel:binding-stack-pointer-sap)
	  (sys:int-sap (+ binding-stack binding-stack-size)))
    (do ((binding 0 (+ 4 binding))
	 (index 0 (1+ index)))
	((= binding binding-stack-size))
      (declare (type (unsigned-byte 29) binding index))
      (setf (sys:sap-ref-sap (sys:int-sap binding-stack) binding)
	    (sys:int-sap (kernel:get-lisp-obj-address
			  (aref new-binding-stack index))))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Alien-stack

;;; The Top of the Alien-stack.
(declaim (type (unsigned-byte 32) *alien-stack-top*))
(defvar *alien-stack-top* 0)

;;; Save the alien-stack.
(defun save-alien-stack (save-stack)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-stack)
	   (optimize (speed 3) (safety 0)))
  (let* ((alien-stack (kernel:get-lisp-obj-address x86::*alien-stack*))
	 (size (- *alien-stack-top* alien-stack))
	 (vector-size (ceiling size 4)))
    (declare (type (unsigned-byte 32) alien-stack)
	     (type (unsigned-byte 29) size))
    #+nil
    (format t "alien-stack ~x; size ~x~%" alien-stack size)
    ;; Grow save-stack if necessary.
    (when (< (length save-stack) vector-size)
      (setq save-stack
	    (adjust-array save-stack vector-size
			  :element-type '(unsigned-byte 32))))
    ;; Save the stack.
    (do ((index 0 (1+ index)))
	((>= index vector-size))
      (declare (type (unsigned-byte 29) index))
      (setf (aref save-stack index)
	    (sys:sap-ref-32 (sys:int-sap *alien-stack-top*)
			    (* 4 (- (1+ index))))))
    (values save-stack vector-size alien-stack)))

(defun restore-alien-stack (save-stack size alien-stack)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-stack)
	   (type (unsigned-byte 29) size)
	   (type (unsigned-byte 32) alien-stack)
	   (optimize (speed 3) (safety 0)))
  (setf x86::*alien-stack* (kernel:make-lisp-obj alien-stack))
  (do ((index 0 (1+ index)))
      ((>= index size))
    (declare (type (unsigned-byte 29) index))
    (setf (sys:sap-ref-32 (sys:int-sap *alien-stack-top*) (* 4 (- (1+ index))))
	  (aref save-stack index)))
  (values))

#+nil
(defun tst-alien ()
  (alien:with-alien ((buf (array char 256)))
    (format t "~s~%" buf)
    (multiple-value-bind (save-stack size alien-stack)
	(save-alien-stack (make-array 0 :element-type '(unsigned-byte 32)))
      (restore-alien-stack save-stack size alien-stack))
    (format t "~s~%" buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Interrupt contexts.

;;; Save the interrupt contexts.
(defun save-interrupt-contexts (save-vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-vector)
	   (optimize (speed 3) (safety 0)))
  (let* ((size lisp::*free-interrupt-context-index*))
    (declare (type (unsigned-byte 29) size))
    ;; Grow save-stack if necessary.
    (when (< (length save-vector) size)
      (setq save-vector
	    (adjust-array save-vector size :element-type '(unsigned-byte 32))))
    (alien:with-alien
	((lisp-interrupt-contexts (array alien:unsigned nil) :extern))
      (dotimes (index size)
	(setf (aref save-vector index)
	      (alien:deref lisp-interrupt-contexts index))))
    save-vector))

;;; Restore the interrupt contexts.
(defun restore-interrupt-contexts (save-vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-vector)
	   (optimize (speed 3) (safety 0)))
  (let* ((size lisp::*free-interrupt-context-index*))
    (declare (type (unsigned-byte 29) size))
    (alien:with-alien
	((lisp-interrupt-contexts (array alien:unsigned nil) :extern))
      (dotimes (index size)
	(setf (alien:deref lisp-interrupt-contexts index)
	      (aref save-vector index)))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

;;; The control stacks need special handling on the x86 as they
;;; contain conservative root. When placed in the *control-stacks*
;;; vector they will be scavenged for conservative roots by the
;;; garbage collector.
(declaim (type (simple-array (or null (simple-array (unsigned-byte 32) (*)))
			     (*)) x86::*control-stacks*))
(defvar x86::*control-stacks*
  (make-array 0 :element-type '(or null (unsigned-byte 32))
	      :initial-element nil))

;;; Stack-group stucture.
(defstruct (stack-group
	     (:constructor %make-stack-group)
	     (:print-function
	      (lambda (stack-group stream depth)
		(declare (type stack-group stack-group)
			 (stream stream)
			 (ignore depth))
		(format stream "#<Stack-group ~a, ~a>"
			(stack-group-name stack-group)
			(stack-group-state stack-group)))))
  ;; Must have a name.
  (name "Anonymous" :type simple-base-string)
  ;; State: :active or :inactive.
  (state :inactive :type (member :active :inactive))
  ;; The control stack; an index into *control-stacks*.
  (control-stack-id nil :type (or lisp::index null))
  ;; Binding stack.
  (binding-stack nil :type (or (simple-array t (*)) null))
  ;; Twice the number of bindings.
  (binding-stack-size 0 :type (unsigned-byte 29))
  ;; Current catch block, on the control stack.
  (current-catch-block 0 :type fixnum)
  ;; Unwind protect block, on the control stack.
  (current-unwind-protect-block 0 :type fixnum)
  ;; Alien stack
  (alien-stack nil :type (or (simple-array (unsigned-byte 32) (*)) null))
  (alien-stack-size 0 :type (unsigned-byte 29))
  (alien-stack-pointer 0 :type (unsigned-byte 32))
  ;; Eval-stack
  (eval-stack nil :type (or (simple-array t (*)) null))
  (eval-stack-top 0 :type fixnum)
  ;;
  ;; Interrupt contexts
  (interrupt-contexts nil :type (or (simple-array (unsigned-byte 32) (*))
				    null))
  ;; Resumer
  (resumer nil :type (or stack-group null)))

;;; The current stack group.
(declaim (type (or stack-group null) *current-stack-group*))
(defvar *current-stack-group* nil)

(declaim (type (or stack-group null) *initial-stack-group*))
(defvar *initial-stack-group* nil)

;;; Init-Stack-Groups -- Interface
;;;
;;; Setup the initial stack group.
;;;
(defun init-stack-groups ()
  ;; Grab the top of the alien-stack; it's currently stored at the top
  ;; of the control stack.
  (setf *alien-stack-top*
	(sys:sap-ref-32
	 (sys:int-sap (alien:extern-alien "control_stack_end" alien:unsigned))
	 -4))
  ;; Initialise the *control-stacks* vector.
  (setq x86::*control-stacks*
	(make-array 10 :element-type '(or null (unsigned-byte 32))
		    :initial-element nil))
  ;; Setup a control-stack for the initial stack-group.
  (setf (aref x86::*control-stacks* 0)
	(make-array 0 :element-type '(unsigned-byte 32)
		    :initial-element 0))
  ;; Make and return the initial stack group.
  (setf *current-stack-group*
	(%make-stack-group
	 :name "Initial"
	 :state :active
	 :control-stack-id 0
	 :binding-stack #()
	 :alien-stack (make-array 0 :element-type '(unsigned-byte 32))
	 :interrupt-contexts (make-array 0 :element-type '(unsigned-byte 32))
	 :eval-stack #()))
  (setf *initial-stack-group* *current-stack-group*))

;;; Inactivate-Stack-Group -- Internal
;;;
;;; Inactivate the stack group, cleaning its slot and freeing the
;;; control stack.
;;;
(defun inactivate-stack-group (stack-group)
  (declare (type stack-group stack-group))
  (setf (stack-group-state stack-group) :inactive)
  (let ((cs-id (stack-group-control-stack-id stack-group)))
    (when (and cs-id (aref x86::*control-stacks* cs-id))
      (setf (aref x86::*control-stacks* cs-id) nil)))
  (setf (stack-group-control-stack-id stack-group) nil)
  (setf (stack-group-binding-stack stack-group) nil)
  (setf (stack-group-binding-stack-size stack-group) 0)
  (setf (stack-group-current-catch-block stack-group) 0)
  (setf (stack-group-current-unwind-protect-block stack-group) 0)
  (setf (stack-group-alien-stack stack-group) nil)
  (setf (stack-group-alien-stack-size stack-group) 0)
  (setf (stack-group-alien-stack-pointer stack-group) 0)
  (setf (stack-group-eval-stack stack-group) nil)
  (setf (stack-group-eval-stack-top stack-group) 0)
  (setf (stack-group-resumer stack-group) nil))


;;; Make-Stack-Group -- Interface
;;;
;;; Fork a new stack-group from the *current-stack-group*. Execution
;;; continues with the *current-stack-group* returning the new stack
;;; group. Control may be transfer to the child by stack-group-resume
;;; and it executes the initial-function.
;;;
(defun make-stack-group (name initial-function &optional
			      (resumer *current-stack-group*))
  (declare (type simple-base-string name)
	   (type function initial-function)
	   (type stack-group resumer))
  (flet ((allocate-control-stack ()
	   (let* (;; Allocate a new control-stack ID.
		  (control-stack-id (position nil x86::*control-stacks*))
		  ;; Find the required stack size.
		  (control-stack-end
		   (alien:extern-alien "control_stack_end" alien:unsigned))
		  (control-stack-pointer (kernel:control-stack-pointer-sap))
		  (control-stack-size
		   (- control-stack-end (sys:sap-int control-stack-pointer)))
		  ;; Saved control stack needs three extra words. The
		  ;; stack pointer will be stored in the first
		  ;; element, and the frame pointer and return address
		  ;; push onto the bottom of the stack.
		  (control-stack
		   (make-array (+ (ceiling control-stack-size 4) 3)
			       :element-type '(unsigned-byte 32)
			       :initial-element 0)))
	     (declare (type (unsigned-byte 29) control-stack-size))
	     (unless control-stack-id
	       ;; Need to extend the *control-stacks* vector.
	       (setq control-stack-id (length x86::*control-stacks*))
	       (setq x86::*control-stacks*
		     (adjust-array x86::*control-stacks*
				   (* 2 (length x86::*control-stacks*))
				   :element-type '(or null (unsigned-byte 32))
				   :initial-element nil)))
	     (setf (aref x86::*control-stacks* control-stack-id) control-stack)
	     (values control-stack control-stack-id)))
	 (allocate-child-stack-group (control-stack-id)
	   ;; Save the interrupt-contexts while the size is still
	   ;; bound.
	   (let ((interrupt-contexts
		  (save-interrupt-contexts
		   (make-array 0 :element-type '(unsigned-byte 32)))))
	     ;; Save the binding stack.
	     (unbind-binding-stack)
	     (multiple-value-bind (binding-stack binding-stack-size)
		 (save-binding-stack #())
	       (rebind-binding-stack)
	       ;; Save the Alien stack
	       (multiple-value-bind (alien-stack alien-stack-size
						 alien-stack-pointer)
		   (save-alien-stack
		    (make-array 0 :element-type '(unsigned-byte 32)))
		 ;; Allocate a stack-group structure.
		 (%make-stack-group
		  :name name
		  :state :active
		  :control-stack-id control-stack-id
		  ;; Save the Eval stack.
		  :eval-stack (copy-seq (the (simple-array t (*))
					     kernel:*eval-stack*))
		  :eval-stack-top kernel:*eval-stack-top*
		  ;; Misc stacks.
		  :current-catch-block lisp::*current-catch-block*
		  :current-unwind-protect-block
		  lisp::*current-unwind-protect-block*
		  ;; Alien stack.
		  :alien-stack alien-stack
		  :alien-stack-size alien-stack-size
		  :alien-stack-pointer alien-stack-pointer
		  ;; Interrupt contexts
		  :interrupt-contexts interrupt-contexts
		  ;; Binding stack.
		  :binding-stack binding-stack
		  :binding-stack-size binding-stack-size
		  ;; Resumer
		  :resumer resumer))))))
    (let ((child-stack-group nil))
      (let ((unix::*interrupts-enabled* nil)
	    (lisp::*gc-inhibit* t))
	(multiple-value-bind (control-stack control-stack-id)
	    (allocate-control-stack)
	  (setq child-stack-group
		(allocate-child-stack-group control-stack-id))
	  ;; Fork the control-stack
	  (if (x86:control-stack-fork control-stack)
	      ;; Current-stack-group returns the child-stack-group.
	      child-stack-group
	      ;; Child starts.
	      (unwind-protect
		   (progn
		     (setq *current-stack-group* child-stack-group)
		     (assert (eq *current-stack-group*
				 (process-stack-group *current-process*)))
		     ;; Child start with interrupts and GC enabled.
		     (let ((unix::*interrupts-enabled* t)
			   (lisp::*gc-inhibit* nil))
		       (when unix::*interrupt-pending*
			 (unix::do-pending-interrupt))
		       (when lisp::*need-to-collect-garbage*
			 (lisp::maybe-gc))
		       (funcall initial-function)))
		(let ((resumer (stack-group-resumer child-stack-group)))
		  (inactivate-stack-group child-stack-group)
		  ;; Verify the resumer.
		  (unless (and resumer
			       (eq (stack-group-state resumer) :active))
		    (format t "*Resuming stack-group ~s instead of ~s~%"
			    *initial-stack-group* resumer)
		    (setq resumer *initial-stack-group*))
		  ;; Restore the resumer state.
		  (setq *current-stack-group* resumer)
		  ;; Eval-stack
		  (setf kernel:*eval-stack* (stack-group-eval-stack resumer))
		  (setf kernel:*eval-stack-top*
			(stack-group-eval-stack-top resumer))
		  ;; The binding stack.
		  (unbind-binding-stack)
		  (restore-binding-stack
		   (stack-group-binding-stack resumer)
		   (stack-group-binding-stack-size resumer))
		  (rebind-binding-stack)
		  ;; Misc stacks.
		  (setf lisp::*current-catch-block*
			(stack-group-current-catch-block resumer))
		  (setf lisp::*current-unwind-protect-block*
			(stack-group-current-unwind-protect-block resumer))
		  ;; The Alien stack
		  (restore-alien-stack
		   (stack-group-alien-stack resumer)
		   (stack-group-alien-stack-size resumer)
		   (stack-group-alien-stack-pointer resumer))
		  ;; Interrupt-contexts.
		  (restore-interrupt-contexts
		   (stack-group-interrupt-contexts resumer))
		  ;; 
		  (let ((new-control-stack
			 (aref x86::*control-stacks*
			       (stack-group-control-stack-id resumer))))
		    (declare (type (simple-array (unsigned-byte 32) (*))
				   new-control-stack))
		    (x86:control-stack-return new-control-stack)))))))
      (when (and unix::*interrupts-enabled* unix::*interrupt-pending*)
	(unix::do-pending-interrupt))
      (when (and lisp::*need-to-collect-garbage* (not lisp::*gc-inhibit*))
	(lisp::maybe-gc))
      child-stack-group)))


;;; Stack-Group-Resume -- Interface
;;;
;;; Transfer control to the given stack-group, resuming its execution,
;;; and saving the *current-stack-group*.
;;;
(defun stack-group-resume (new-stack-group)
  (declare (type stack-group new-stack-group))
  (assert (and (eq (stack-group-state new-stack-group) :active)
	       (not (eq new-stack-group *current-stack-group*))))
  (assert (eq new-stack-group (process-stack-group *current-process*)))
  (let ((unix::*interrupts-enabled* nil)
	(lisp::*gc-inhibit* t))
    (let* (;; Save the current stack-group on its stack.
	   (stack-group *current-stack-group*)
	   ;; Find the required stack size.
	   (control-stack-end
	    (alien:extern-alien "control_stack_end" alien:unsigned))
	   (control-stack-pointer (kernel:control-stack-pointer-sap))
	   (control-stack-size (- control-stack-end
				  (sys:sap-int control-stack-pointer)))
	   ;; Stack-save array needs three extra elements. The stack
	   ;; pointer will be stored in the first, and the frame
	   ;; pointer and return address push onto the bottom of the
	   ;; stack.
	   (save-stack-size (+ (ceiling control-stack-size 4) 3))
	   ;; The save-stack vector.
	   (control-stack (aref x86::*control-stacks*
				(stack-group-control-stack-id stack-group))))
      (declare (type (unsigned-byte 29) control-stack-size save-stack-size)
	       (type (simple-array (unsigned-byte 32) (*)) control-stack))
      ;; Increase the save-stack size if necessary.
      (when (> save-stack-size (length control-stack))
	(setf control-stack (adjust-array control-stack save-stack-size
					  :element-type '(unsigned-byte 32)
					  :initial-element 0))
	(setf (aref x86::*control-stacks*
		    (stack-group-control-stack-id stack-group))
	      control-stack))
      
      ;; Eval-stack
      (setf (stack-group-eval-stack stack-group) kernel:*eval-stack*)
      (setf (stack-group-eval-stack-top stack-group) kernel:*eval-stack-top*)
      (setf kernel:*eval-stack* (stack-group-eval-stack new-stack-group))
      (setf kernel:*eval-stack-top*
	    (stack-group-eval-stack-top new-stack-group))
      
      ;; Misc stacks.
      (setf (stack-group-current-catch-block stack-group)
	    lisp::*current-catch-block*)
      (setf (stack-group-current-unwind-protect-block stack-group)
	    lisp::*current-unwind-protect-block*)
      (setf lisp::*current-catch-block*
	    (stack-group-current-catch-block new-stack-group))
      (setf lisp::*current-unwind-protect-block*
	    (stack-group-current-unwind-protect-block new-stack-group))
      
      ;; Save the interrupt-contexts.
      (setf (stack-group-interrupt-contexts stack-group)
	    (save-interrupt-contexts
	     (stack-group-interrupt-contexts stack-group)))

      ;; The binding stack.
      (unbind-binding-stack)
      (multiple-value-bind (stack size)
	  (save-binding-stack (stack-group-binding-stack stack-group))
	(setf (stack-group-binding-stack stack-group) stack)
	(setf (stack-group-binding-stack-size stack-group) size))
      (restore-binding-stack (stack-group-binding-stack new-stack-group)
			     (stack-group-binding-stack-size new-stack-group))
      (rebind-binding-stack)
      
      ;; Restore the interrupt-contexts.
      (restore-interrupt-contexts
       (stack-group-interrupt-contexts new-stack-group))

      ;; The Alien stack
      (multiple-value-bind (save-stack size alien-stack)
	  (save-alien-stack (stack-group-alien-stack stack-group))
	(setf (stack-group-alien-stack stack-group) save-stack)
	(setf (stack-group-alien-stack-size stack-group) size)
	(setf (stack-group-alien-stack-pointer stack-group) alien-stack))
      (restore-alien-stack (stack-group-alien-stack new-stack-group)
			   (stack-group-alien-stack-size new-stack-group)
			   (stack-group-alien-stack-pointer new-stack-group))
      ;; 
      (let ((new-control-stack
	     (aref x86::*control-stacks*
		   (stack-group-control-stack-id new-stack-group))))
	(declare (type (simple-array (unsigned-byte 32) (*))
		       new-control-stack))
	(x86:control-stack-resume control-stack new-control-stack))
      ;; Thread returns.
      (setq *current-stack-group* stack-group)))
  (assert (eq *current-stack-group* (process-stack-group *current-process*)))
  (when (and unix::*interrupts-enabled* unix::*interrupt-pending*)
    (unix::do-pending-interrupt))
  (when (and lisp::*need-to-collect-garbage* (not lisp::*gc-inhibit*))
    (lisp::maybe-gc))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Double-float timing functions for use by the scheduler.

;;; The timing functions use double-floats here for accuracy. In most
;;; cases consing is avoided.

;;; Get-Real-Time
;;;
(declaim (inline get-real-time))
;;;
(defun get-real-time ()
  "Return the real time in seconds."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ignore seconds useconds)
      (unix:unix-gettimeofday)
    (declare (ignore ignore)
	     (type (unsigned-byte 32) seconds useconds))
    (+ (coerce seconds 'double-float)
       (* (coerce useconds 'double-float) 1d-6))))

;;; Get-Run-Time
;;;
(declaim (inline get-run-time))
;;;
(defun get-run-time ()
  "Return the run time in secondes"
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ignore utime-sec utime-usec stime-sec stime-usec)
      (unix:unix-fast-getrusage unix:rusage_self)
    (declare (ignore ignore)
	     (type (unsigned-byte 31) utime-sec stime-sec)
	     (type (mod 1000000) utime-usec stime-usec))
    (+ (coerce utime-sec 'double-float) (coerce stime-sec 'double-float)
       (* (+ (coerce utime-usec 'double-float)
	     (coerce stime-usec 'double-float))
	  1d-6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Multi-process support. The interface is based roughly on the
;;;; CLIM-SYS spec. and support needed for cl-http.

(defvar *multi-processing* t)

(defstruct (process
	     (:constructor %make-process)
	     (:predicate processp)
	     (:print-function
	      (lambda (process stream depth)
		(declare (type process process)
			 (stream stream)
			 (ignore depth))
		(format stream "#<Process ~a>" (process-name process)))))
  (name "Anonymous" :type simple-base-string)
  (state :killed :type (member :killed :active :inactive))
  (%whostate nil :type (or null simple-base-string))
  (initial-function nil :type (or null function))
  (initial-args nil :type list)
  (wait-function nil :type (or null function))
  (wait-timeout nil :type (or null fixnum))
  (wait-return-value nil :type t)
  (interrupts '() :type list)
  (stack-group nil :type (or null stack-group))
  ;;
  ;; The real and run times when the current process was last
  ;; scheduled or yielded.
  (scheduled-real-time (get-real-time) :type double-float)
  (scheduled-run-time (get-run-time) :type double-float)
  ;;
  ;; Accrued real and run times in seconds.
  (%real-time 0d0 :type double-float)
  (%run-time 0d0 :type double-float))

(defun process-whostate (process)
  (cond ((eq (process-state process) :killed)
	 "Killed")
	((process-wait-function process)
	 (or (process-%whostate process) "Run"))
	(t
	 "Run")))

(declaim (inline process-active-p))
(defun process-active-p (process)
  (eq (process-state process) :active))

(declaim (inline process-alive-p))
(defun process-alive-p (process)
  (let ((state (process-state process)))
    (or (eq state :active) (eq state :inactive))))

(declaim (type (or null process) *current-process*))
(defvar *current-process* nil)

(declaim (inline current-process))
(defun current-process ()
  "Returns the current process."
  *current-process*)

(declaim (list *all-processes*))
(defvar *all-processes* nil
  "A list of all alive processes.")

(declaim (inline all-processes))
(defun all-processes ()
  "Return a list of all the live processes."
  *all-processes*)

(declaim (type (or null process) *intial-process*))
(defvar *initial-process* nil)

(defvar *inhibit-scheduling* t)

(defmacro without-scheduling (&body body)
  `(let ((inhibit *inhibit-scheduling*))
    (unwind-protect
	 (progn
	   (setf *inhibit-scheduling* t)
	   ,@body)
      (setf *inhibit-scheduling* inhibit))))

(defmacro atomic-incf (reference &optional (delta 1))
  "Increaments the reference by delta in a single atomic operation"
  `(without-scheduling
    (incf ,reference ,delta)))

(defmacro atomic-decf (reference &optional (delta 1))
  "Decrements the reference by delta in a single atomic operation"
  `(without-scheduling
    (decf ,reference ,delta)))

(defmacro atomic-push (obj place)
  `(without-scheduling
    (push ,obj ,place)))

(defmacro atomic-pop (place)
  `(without-scheduling
    (pop ,place)))


;;; Make-Process
;;;
(defun make-process (function &key (name "Anonymous"))
  (declare (type (or null function) function))
  (cond ((null function)
	 ;; If function is nil then create a dead process; can be
	 ;; restarted with process-preset.
	 (%make-process :initial-function nil :name name :state :killed))
	(t
	 ;; Create a stack-group.
	 (let ((process
		(%make-process
		 :name name
		 :state :active
		 :initial-function function
		 :stack-group
		 (make-stack-group
		  name 
		  #'(lambda ()
		      (unwind-protect
			   (progn
			     (setf *inhibit-scheduling* nil)
			     (funcall function))
			(setf *inhibit-scheduling* t)
			;; About to return to the resumer's
			;; stack-group, which in this case is the
			;; initial process's stack-group.
			(setf (process-state *current-process*) :killed)
			(setf *all-processes*
			      (delete *current-process* *all-processes*))
			(setf (process-%whostate *current-process*) nil)
			(setf (process-wait-function *current-process*) nil)
			(setf (process-wait-timeout *current-process*) nil)
			(setf (process-wait-return-value *current-process*)
			      nil)
			(setf (process-interrupts *current-process*) nil)
			(setf *current-process* *initial-process*)))
		  *initial-stack-group*))))
	   (push process *all-processes*)
	   process))))

(defun process-interrupt (process function)
  "Interrupt process ard cause it to evaluate function."
  ;; Place the interrupt function at the end of process's interrupts
  ;; queue, to be called the next time the process is scheduled.
  (without-scheduling
   (setf (process-interrupts process)
	 (append (list function) (process-interrupts process))))
  (process-yield))

(defun destroy-process (process)
  (declare (type process process))
  (assert (not (eq process *current-process*)))
  (unless (eq (process-state process) :killed)
    ;; Place the a throw to end-of-the-world at the start of process's
    ;; interrupts queue, to be called the next time the process is
    ;; scheduled.
    (without-scheduling
     (push #'(lambda ()
	       (throw 'lisp::%end-of-the-world 0))
	   (process-interrupts process)))
    ;; Should we wait until it's dead?
    (process-yield)))

(defun restart-process (process)
  "Restart process by unwinding it to its initial state and calling its
  initial function."
  (destroy-process process)
  (process-wait "Waiting for process to die" 
		#'(lambda ()
		    (eq (process-state process) :killed)))
  ;; Create a new stack-group.
  (setf (process-stack-group process)
	(make-stack-group
	 (process-name process)
	 #'(lambda ()
	     (unwind-protect 
		  (progn
		    (setf *inhibit-scheduling* nil)
		    (apply (process-initial-function process)
			   (process-initial-args process)))
	       (setf *inhibit-scheduling* t)
	       ;; About to return to the resumer's stack-group, which
	       ;; in this case is the initial process's stack-group.
	       (setf (process-state *current-process*) :killed)
	       (setf *all-processes*
		     (delete *current-process* *all-processes*))
	       (setf (process-%whostate *current-process*) nil)
	       (setf (process-wait-function *current-process*) nil)
	       (setf (process-wait-timeout *current-process*) nil)
	       (setf (process-wait-return-value *current-process*) nil)
	       (setf (process-interrupts *current-process*) nil)
	       (setf *current-process* *initial-process*)))
	 *initial-stack-group*))
  (setf (process-%whostate process) nil)
  (setf (process-wait-function process) nil)
  (setf (process-wait-timeout process) nil)
  (setf (process-wait-return-value process) nil)
  (setf (process-interrupts process) nil)
  (setf (process-state process) :active)
  (push process *all-processes*)
  process)

(defun process-preset (process function &rest args)
  "Restart process, unwinding it to its initial state and call
  function with args."
  (setf (process-initial-function process) function)
  (setf (process-initial-args process) args)
  (restart-process process))

(declaim (inline disable-processes))
(defun disable-process (process)
  "Disable process from being runnable until enabled."
  (setf (process-state process) :inactive))

(declaim (inline enable-processes))
(defun enable-process (process)
  "Allow process to become runnable again after it has been disabled."
  (setf (process-state process) :active))

(defun process-wait (whostate predicate)
  "Causes the process to wait until predicate returns True."
  (assert (not *inhibit-scheduling*))
  (assert (not (process-wait-function *current-process*)))
  (setf (process-%whostate *current-process*) whostate)
  (setf (process-wait-timeout *current-process*) nil)
  (setf (process-wait-function *current-process*) predicate)
  (process-yield)
  (process-wait-return-value *current-process*))

(defun process-wait-with-timeout (whostate timeout predicate)
  (declare (type (unsigned-byte 29) timeout))
  "Causes the process to wait until predicate returns True, or the
  number of seconds specified by timeout has elapsed."
  (assert (not *inhibit-scheduling*))
  (assert (not (process-wait-function *current-process*)))
  (let ((timeout (+ (get-internal-real-time)
		    (* timeout internal-time-units-per-second))))
    (declare (fixnum timeout))
    (setf (process-%whostate *current-process*) whostate)
    (setf (process-wait-timeout *current-process*) timeout)
    (setf (process-wait-function *current-process*) predicate)
    (process-yield)
    (process-wait-return-value *current-process*)))

;;; The remaining processes in the scheduling queue for this cycle,
;;; the remainder of *all-processes*. The *current-process* is the
;;; first element of this list.
(defvar *remaining-processes* nil)

;;; The idle process will only run when there are no other runnable
;;; processes.
(defvar *idle-process* nil)

;;; Decide when the allow the idle process to run.
(defun run-idle-process-p ()
  ;; Check if there are any other runnable processes.
  (dolist (process *all-processes* t)
    (when (and (not (eq process *idle-process*))
	       (process-active-p process)
	       (not (process-wait-function process)))
      (return nil))))

;;; A useful idle process loop, waiting on events using the select
;;; based event server, and yielding periodically.
;;;
;;; Timeout for the idle loop, something acceptable by
;;; sever-all-events.
(defvar *idle-loop-timeout* 1)
;;;
(defun idle-process-loop ()
  (loop
   (sys:serve-all-events *idle-loop-timeout*)
   (process-yield)))

;;; Update the processes times for the current and new processes at a
;;; scheduling process switch.
;;;
(defun update-process-timers (current-process new-process)
  (declare (type process current-process new-process)
	   (optimize (speed 3) (safety 0)))
  (let ((real-time (get-real-time)))
    (incf (process-%real-time current-process)
	  (- real-time (process-scheduled-real-time current-process)))
    (setf (process-scheduled-real-time current-process) real-time)
    (setf (process-scheduled-real-time new-process) real-time))
  (let ((run-time (get-run-time)))
    (incf (process-%run-time current-process)
	  (- run-time (process-scheduled-run-time current-process)))
    (setf (process-scheduled-run-time current-process) run-time)
    (setf (process-scheduled-run-time new-process) run-time))
  (values))

;;; Process-Yield
;;;
;;; The Scheduler.
;;;
(defun process-yield ()
  (declare (optimize (speed 3)))
  "Allow other processes to run."
  (unless *inhibit-scheduling*
    ;; Catch any FP exceptions before entering the scheduler.
    (kernel:float-wait)
    ;; Inhibit recursive entry of the scheduler.
    (setf *inhibit-scheduling* t)
    (assert (eq (first *remaining-processes*) *current-process*))
    (assert (eq *current-stack-group* (process-stack-group *current-process*)))
    (loop
     ;; Rotate the queue.
     (setf *remaining-processes*
	   (or (rest *remaining-processes*) *all-processes*))
     
     (let ((next (first *remaining-processes*)))
       ;; Shouldn't see any :killed porcesses here.
       (assert (process-alive-p next))
       
       (cond
	 ;; New process at the head of the queue?
	 ((eq next *current-process*))
	 ;; Ignore inactive processes.
	 ((not (process-active-p next)))
	 ;; If the next process has pending interrupts then return to
	 ;; it to execute these.
	 ((process-interrupts next)
	  (setf *current-process* next)
	  (stack-group-resume (process-stack-group next)))
	 (t
	  ;; If not waiting then return.
	  (let ((wait-fn (process-wait-function next)))
	    (cond
	      ((null wait-fn)
	       ;; Skip the idle process if there are other runnable
	       ;; processes.
	       (when (or (not (eq next *idle-process*))
			 (run-idle-process-p))
		 (update-process-timers *current-process* next)
		 (setf *current-process* next)
		 (stack-group-resume (process-stack-group next))))
	      (t
	       ;; Check the wait function in the current context
	       ;; saving a stack-group switch; although
	       ;; *current-process* is setup.
	       (let ((current-process *current-process*))
		 (setf *current-process* next)
		 ;; Predicate true?
		 (let ((wait-return-value (funcall wait-fn)))
		   (cond (wait-return-value
			  ;; Flush the wait.
			  (setf (process-wait-return-value next) 
				wait-return-value)
			  (setf (process-wait-timeout next) nil)
			  (setf (process-wait-function next) nil)
			  (setf (process-%whostate next) nil)
			  (update-process-timers current-process next)
			  (stack-group-resume (process-stack-group next)))
			 (t
			  ;; Timeout?
			  (let ((timeout (process-wait-timeout next)))
			    (when (and timeout
				       (> (the fixnum (get-internal-real-time))
					  timeout))
			      ;; Flush the wait.
			      (setf (process-wait-return-value next) nil)
			      (setf (process-wait-timeout next) nil)
			      (setf (process-wait-function next) nil)
			      (setf (process-%whostate next) nil)
			      (update-process-timers current-process next)
			      (stack-group-resume
			       (process-stack-group next)))))))
		 ;; Restore the *current-process*.
		 (setf *current-process* current-process))))))))
    
     ;; May have just returned, or have cycled the queue.
     (let ((next (first *remaining-processes*)))
       ;; Tolerate :killed processes on the *remaining-processes* list
       ;; saving their deletion from this list when killed; will be
       ;; corrected when it cycles back to *all-processes*.
       (when (and (process-active-p next)
		  ;; Current process at the head of the queue?
		  (eq next *current-process*))
	 ;; Run any pending interrupts.
	 (let ((interrupt (pop (process-interrupts next))))
	   (declare (type (or null function) interrupt))
	   (cond (interrupt 
		  ;; Save and reset any wait reasons so that the
		  ;; interrupt can wait.
		  (let ((wait-function (process-wait-function next))
			(wait-timeout (process-wait-timeout next))
			(whostate (process-%whostate next)))
		    (setf (process-wait-function next) nil)
		    (setf (process-wait-timeout next) nil)
		    (setf (process-%whostate next) nil)
		    ;; Allow recursive scheduling during the interrupt
		    ;; processing. Only one interrupt is processed on
		    ;; each scheduler queue cycle. The process doesn't
		    ;; return until there are no interrupts.
		    (setf *inhibit-scheduling* nil)
		    (funcall interrupt)
		    (setf *inhibit-scheduling* t)
		    ;; Restore any wait reasons.
		    (setf (process-wait-function next) wait-function)
		    (setf (process-wait-timeout next) wait-timeout)
		    (setf (process-%whostate next) whostate)))
		 (t
		  ;; Check the wait function.
		  (let ((wait-fn (process-wait-function next)))
		    (cond
		      ((null wait-fn)
		       (when (or (not (eq next *idle-process*))
				 (run-idle-process-p))
			 (return)))
		      (t
		       ;; Predicate true?
		       (let ((return-value (funcall wait-fn)))
			 (when return-value
			   ;; Flush the wait.
			   (setf (process-wait-return-value next) return-value)
			   (setf (process-wait-timeout next) nil)
			   (setf (process-wait-function next) nil)
			   (setf (process-%whostate next) nil)
			   (return)))
		       ;; Timeout?
		       (let ((timeout (process-wait-timeout next)))
			 (when (and timeout
				    (> (the fixnum (get-internal-real-time))
				       timeout))
			   ;; Flush the wait.
			   (setf (process-wait-return-value next) nil)
			   (setf (process-wait-timeout next) nil)
			   (setf (process-wait-function next) nil)
			   (setf (process-%whostate next) nil)
			   (return))))))))))))
    (setf *inhibit-scheduling* nil)))

;;; Process-Real-Time
;;;
;;; The real time in seconds accrued while the process was scheduled.
;;;
(defun process-real-time (process)
  (declare (type process process))
  (if (eq process *current-process*)
      (without-scheduling
       (let ((real-time (get-real-time)))
	 (+ (process-%real-time process)
	    (- real-time (process-scheduled-real-time process)))))
      (process-%real-time process)))

;;; Process-Run-Time
;;;
;;; The run time in seconds accrued while the process was scheduled.
;;;
(defun process-run-time (process)
  (declare (type process process))
  (if (eq process *current-process*)
      (without-scheduling
       (let ((run-time (get-run-time)))
	 (+ (process-%run-time process)
	    (- run-time (process-scheduled-run-time process)))))
      (process-%run-time process)))

;;; Process-Idle-Time
;;;
;;; The real time in seconds elapsed since the process was last
;;; de-scheduled.
;;;
(defun process-idle-time (process)
  (declare (type process process))
  (if (eq process *current-process*)
      0
      (without-scheduling
       (let ((real-time (get-real-time)))
	 (- real-time (process-scheduled-real-time process))))))

;;; Start-Sigalrm-Yield
;;;
;;; Start a regular interrupt to switch processes. This may not be a
;;; good idea yet as the CMUCL code is not too interrupt safe.
;;;
(defun start-sigalrm-yield (&optional (sec 0) (usec 500000))
  (declare (fixnum sec usec))
  ;; Disable the gencgc pointer filter to improve interrupt safety.
  #+(and gencgc nil)
  (setf (alien:extern-alien "enable_pointer_filter" alien:unsigned) 0)
  (flet ((sigalrm-handler (signal code scp)
	   (declare (ignore signal code scp))
	   (cond ((<= lisp::*free-interrupt-context-index* 1)
		  #+nil (format t ".~%")
		  (process-yield))
		 (t
		  #+nil (format t "-~%")))))
    (sys:enable-interrupt :sigalrm #'sigalrm-handler))
  (unix:unix-setitimer :real sec usec 0 1)
  (values))

;;; Initialise the initial process, must be called before use of the
;;; other multi-process function.
;;;
(defun init-processes ()
  (unless *initial-process*
    (init-stack-groups)
    (setf *initial-process*
	  (%make-process
	   :name "Initial"
	   :state :active
	   :stack-group *initial-stack-group*))
    (setf *current-process* *initial-process*)
    (setf *all-processes* (list *initial-process*))
    (setf *remaining-processes* *all-processes*)
    ;;
    #+nil (start-sigalrm-yield)
    (setf *inhibit-scheduling* nil))
  ;;
  (values))

(pushnew 'init-processes ext:*after-save-initializations*)

;;; Show-Processes
;;;
(defun show-processes (&optional verbose)
  (dolist (process *all-processes*)
    (format t "~s ~s ~a~%" process (process-whostate process) 
	    (process-state process))
    (when verbose
      (format t "~4TRun time: ~,3f; Real time: ~,3f; Idle time: ~,3f~%"
	      (process-run-time process)
	      (process-real-time process)
	      (process-idle-time process)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Simple Locking.

(defstruct (lock
	     (:constructor make-lock (&optional name))
	     (:print-function
	      (lambda (lock stream depth)
		(declare (type lock lock)
			 (stream stream)
			 (ignore depth))
		(format stream "#<Lock ~a>" (lock-name lock)))))
  (name nil)
  (process nil :type (or null process)))

#-i486
(defun seize-lock (lock)
  (declare (type lock lock))
  (sys:without-interrupts
   (unless (lock-process lock)
     (setf (lock-process lock) *current-process*))))

#-i486
(defmacro with-lock-held ((lock &optional (whostate "Waiting for lock"))
			  &body body)
  (let ((orig-process (gensym)))
    `(let ((,orig-process (lock-process ,lock)))
      (unwind-protect
	   (progn
	     (unless (or (eq ,orig-process *current-process*)
			 (seize-lock ,lock))
	       (process-wait ,whostate
		     #'(lambda ()
			 (unless (lock-process ,lock)
			   (setf (lock-process ,lock) *current-process*)))))
	     ,@body)
	(unless (or (eq ,orig-process *current-process*)
		    (not (eq (lock-process ,lock) *current-process*)))
	  (setf (lock-process ,lock) nil))))))

;;; Fast locking for the i486 and above.
#+i486
(defmacro with-lock-held ((lock &optional (whostate "Waiting for lock"))
			  &body body)
  (let ((orig-process (gensym)))
    `(let ((,orig-process (lock-process ,lock)))
      (unwind-protect
	   (progn
	     (unless (or (eq ,orig-process *current-process*)
			 (null (kernel:%instance-set-conditional
				,lock 2 nil *current-process*)))
	       (process-wait ,whostate
			     #'(lambda ()
				 (null (kernel:%instance-set-conditional
					,lock 2 nil *current-process*)))))
	     ,@body)
	(unless (eq ,orig-process *current-process*)
	  (kernel:%instance-set-conditional ,lock 2 *current-process* nil))))))
