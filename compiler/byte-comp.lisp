;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/byte-comp.lisp,v 1.1 1992/07/24 04:08:16 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the noise to byte-compile stuff.  It uses the
;;; same front end as the real compiler, but generates a byte-code instead
;;; of native code.
;;;
;;; Written by William Lott
;;;

(in-package "C")

;;; ### Remaining work:
;;;
;;; - Add type-testing noise.  In other words, hack over checkgen to know
;;; about the byte-compiler.
;;; - Breakpoints/debugging info.
;;; 


;;;; Stuff to emit noise.

;;; Note: we use the regular assembler, but we don't use any ``instructions''
;;; because there is no way to keep our byte-code instructions seperate from
;;; the instructions used by the native backend.  Besides, we don't want to do
;;; any scheduling or anything like that, anyway.

(declaim (inline output-byte))
(defun output-byte (segment byte)
  (declare (type new-assem:segment segment)
	   (type (unsigned-byte 8) byte))
  (new-assem:emit-byte segment byte))

;;; OUTPUT-BYTE-WITH-OPERAND -- internal.
;;;
;;; Output a byte, logior'ing in a 4 bit immediate constant.  If that
;;; immediate won't fit, then emit it as the next 4 bytes.
;;; 
(defun output-byte-with-operand (segment byte operand)
  (declare (type new-assem:segment segment)
	   (type (unsigned-byte 8) byte)
	   (type (unsigned-byte 32) operand))
  (cond ((<= operand 14)
	 (output-byte segment (logior byte operand)))
	((<= operand 254)
	 (output-byte segment (logior byte 15))
	 (output-byte segment operand))
	(t
	 (output-byte segment (logior byte 15))
	 (output-byte segment #xff)
	 (output-byte segment (ldb (byte 8 24) operand))
	 (output-byte segment (ldb (byte 8 16) operand))
	 (output-byte segment (ldb (byte 8 8) operand))
	 (output-byte segment (ldb (byte 8 0) operand))))
  (undefined-value))

;;; OUTPUT-LABEL -- internal.
;;;
(defun output-label (segment label)
  (declare (type new-assem:segment segment)
	   (type new-assem:label label))
  (new-assem:assemble (segment)
    (new-assem:emit-label label)))

;;; OUTPUT-REFERENCE -- internal.
;;;
;;; Output a reference to LABEL.  If RELATIVE is NIL, then this reference
;;; can never be relative.
;;;
(defun output-reference (segment label)
  (declare (type new-assem:segment segment)
	   (type new-assem:label label))
  (new-assem:emit-back-patch
   segment
   3
   #'(lambda (segment posn)
       (declare (type new-assem:segment segment)
		(ignore posn))
       (let ((target (new-assem:label-position label)))
	 (assert (<= 0 target (1- (ash 1 24))))
	 (output-byte segment (ldb (byte 8 16) target))
	 (output-byte segment (ldb (byte 8 8) target))
	 (output-byte segment (ldb (byte 8 0) target))))))

;;; OUTPUT-BRANCH -- internal.
;;;
;;; Output some branch byte-sequence.
;;; 
(defun output-branch (segment kind label)
  (declare (type new-assem:segment segment)
	   (type (unsigned-byte 8) kind)
	   (type new-assem:label label))
  (new-assem:emit-chooser
   segment 4 1
   #'(lambda (segment posn delta)
       (when (<= (- (ash 1 7))
		 (- (new-assem:label-position label posn delta) posn 2)
		 (1- (ash 1 7)))
	 (new-assem:emit-chooser
	  segment 2 1
	  #'(lambda (segment posn delta)
	      (declare (ignore segment) (type index posn delta))
	      (when (zerop (- (new-assem:label-position label posn delta)
			      posn 2))
		;; Don't emit anything, because the branch is to the following
		;; instruction.
		t))
	  #'(lambda (segment posn)
	      ;; We know we fit in one byte.
	      (declare (type new-assem:segment segment)
		       (type index posn))
	      (output-byte segment (logior kind 1))
	      (output-byte segment
			   (ldb (byte 8 0)
				(- (new-assem:label-position label) posn 2)))))
	 t))
   #'(lambda (segment posn)
       (declare (type new-assem:segment segment)
		(ignore posn))
       (let ((target (new-assem:label-position label)))
	 (assert (<= 0 target (1- (ash 1 24))))
	 (output-byte segment kind)
	 (output-byte segment (ldb (byte 8 16) target))
	 (output-byte segment (ldb (byte 8 8) target))
	 (output-byte segment (ldb (byte 8 0) target))))))

;;; BYTE-OUTPUT-LENGTH -- internal interface.
;;;
;;; Called by the dumping stuff to find out exactly how much noise it needs
;;; to dump.
;;;
(defun byte-output-length (segment)
  (declare (type new-assem:segment segment))
  (new-assem:finalize-segment segment))



;;;; System constants, Xops, and inline functions.

(defvar *system-constants* (make-array 256))

(eval-when (compile eval)
  (defmacro def-system-constant (index form)
    `(setf (svref *system-constants* ,index)
	   ,form)))

(def-system-constant 0 nil)
(def-system-constant 1 t)


(defparameter *xop-names*
  '(breakpoint
    dup
    make-closure
    merge-unknown-values
    default-unknown-values
    throw
    catch
    breakup
    return-from
    tagbody
    go
    unwind-protect
    fdefn-function-or-lose))

(defun xop-index-or-lose (name)
  (or (position name *xop-names* :test #'eq)
      (error "Unknown XOP ~S" name)))


(defstruct inline-function-info
  function
  number
  type)

(defparameter *inline-functions*
  (let ((number -1))
    (mapcar #'(lambda (stuff)
		(destructuring-bind (name arg-types result-type) stuff
		  (make-inline-function-info
		   :function name
		   :number (incf number)
		   :type (specifier-type
			  `(function ,arg-types ,result-type)))))
	    '((+ (fixnum fixnum) fixnum)
	      (- (fixnum fixnum) fixnum)
	      (make-value-cell (t) t)
	      (value-cell-ref (t) t)
	      (value-cell-setf (t t) t)
	      (symbol-value (symbol) t)
	      (setf-symbol-value (t symbol) t)
	      (%byte-special-bind (t symbol) (values))
	      (%byte-special-unbind () (values))
	      (cons-unique-tag () t)))))

(defun inline-function-number-or-lose (function)
  (let ((info (find function *inline-functions*
		    :key #'inline-function-info-function)))
    (if info
	(inline-function-info-number info)
	(error "Unknown inline function: ~S" function))))



;;;; Annotations hung off the IR1 while compiling.


(defstruct byte-component-info
  (constants (make-array 10 :adjustable t :fill-pointer 0)))


(defstruct byte-lambda-info
  (label nil :type (or null label))
  (stack-size 0 :type index)
  (interesting t :type (member t nil)))

(defun block-interesting (block)
  (byte-lambda-info-interesting (lambda-info (block-home-lambda block))))

(defstruct byte-lambda-var-info
  (argp nil :type (member t nil))
  (offset 0 :type index))

(defstruct byte-nlx-info
  (stack-slot nil :type (or null index))
  (label (new-assem:gen-label) :type new-assem:label)
  (duplicate nil :type (member t nil)))

(defstruct (byte-block-info
	    (:include block-annotation)
	    (:print-function %print-byte-block-info)
	    (:constructor make-byte-block-info
			  (block &key produces produces-sset consumes
			    total-consumes nlx-entries nlx-entry-p)))
  (label (new-assem:gen-label) :type new-assem:label)
  ;;
  ;; A list of the continuations that this block pushes onto the stack.
  (produces nil :type list)
  ;;
  ;; An SSET of the produces for faster set manipulations.
  (produces-sset (make-sset) :type sset)
  ;;
  ;; A list of the continuations that this block pops from the stack.
  (consumes nil :type list)
  ;;
  ;; The transitive closure of what this block and all its successors
  ;; consume.  After stack-analysis, that is.
  (total-consumes (make-sset) :type sset)
  ;;
  ;; Set to T whenever the consumes lists of a successor changes and the
  ;; block is queued for re-analysis so we can easily avoid queueing the same
  ;; block several times.
  (already-queued nil :type (member t nil))
  ;;
  ;; The continuations on the stack (in order) when this block starts.
  (start-stack :unknown :type (or (member :unknown) list))
  ;;
  ;; The continuations on the stack (in order) when this block ends.
  (end-stack nil :type list)
  ;;
  ;; List of (nlx-infos . extra-stack-cruft) for each entry point in this
  ;; block.
  (nlx-entries nil :type list)
  ;;
  ;; T if this is an %nlx-entry point, and we shouldn't just assume we know
  ;; what is going to be on the stack.
  (nlx-entry-p nil :type (member t nil)))

(defprinter byte-block-info
  block)

(defstruct (byte-continuation-info
	    (:include sset-element)
	    (:print-function %print-byte-continuation-info)
	    (:constructor make-byte-continuation-info (continuation results)))
  (continuation (required-argument) :type continuation)
  (results (required-argument)
	   :type (or (member :fdefinition :eq-test :unknown) unsigned-byte)))

(defprinter byte-continuation-info
  continuation
  results)



;;;; Annotate the IR1

(defun annotate-continuation (cont results)
  ;; For some reason, do-nodes does the same return node multiple times,
  ;; which causes annotate-continuation to be called multiple times on the
  ;; same continuation.  So we can't assert that we haven't done it.
  #+nil
  (assert (null (continuation-info cont)))
  (setf (continuation-info cont)
	(make-byte-continuation-info cont results))
  (undefined-value))

(defun annotate-set (set)
  ;; Annotate the value for one value.
  (annotate-continuation (set-value set) 1))

(defun annotate-basic-combination-args (call)
  (declare (type basic-combination call))
  (etypecase call
    (combination
     (dolist (arg (combination-args call))
       (when arg
	 (annotate-continuation arg 1))))
    (mv-combination
     ;; Annoate the args.  We allow initial args to supply a fixed number of
     ;; values, but everything after the first :unknown arg must also be
     ;; unknown.  This picks off most of the standard uses (i.e. calls to
     ;; apply), but still is easy to implement.
     (labels
	 ((allow-fixed (remaining)
	    (when remaining
	      (let* ((cont (car remaining))
		     (values (nth-value 1
					(values-types
					 (continuation-derived-type cont)))))
		(cond ((eq values :unknown)
		       (force-to-unknown remaining))
		      (t
		       (annotate-continuation cont values)
		       (allow-fixed (cdr remaining)))))))
	  (force-to-unknown (remaining)
	    (when remaining
	      (let ((cont (car remaining)))
		(when cont
		  (annotate-continuation cont :unknown)))
	      (force-to-unknown (cdr remaining)))))
       (allow-fixed (mv-combination-args call)))))
  (undefined-value))

(defun annotate-local-call (call)
  (annotate-basic-combination-args call)
  (annotate-continuation (basic-combination-fun call) 0))

(defun annotate-full-call (call)
  (let* ((fun (basic-combination-fun call))
	 (name (continuation-function-name fun))
	 (info (find name *inline-functions*
		     :key #'inline-function-info-function)))
    (cond ((and info
		(valid-function-use call (inline-function-info-type info)))
	   (annotate-basic-combination-args call)
	   (setf (node-tail-p call) nil)
	   (setf (basic-combination-info call) info)
	   (annotate-continuation fun 0))
	  ((and (mv-combination-p call) (eq name '%throw))
	   (let ((args (basic-combination-args call)))
	     (assert (= (length args) 2))
	     (annotate-continuation (first args) 1)
	     (annotate-continuation (second args) :unknown))
	   (setf (node-tail-p call) nil)
	   (annotate-continuation fun 0))
	  (t
	   (annotate-basic-combination-args call)
	   (annotate-continuation
	    fun
	    (let ((use (continuation-use fun)))
	      (if (ref-p use)
		  (let ((leaf (ref-leaf use)))
		    (if (and (global-var-p leaf)
			     (eq (global-var-kind leaf) :global-function))
			:fdefinition
			1))
		  1))))))
  (undefined-value))

(defun annotate-known-call (call)
  (annotate-basic-combination-args call)
  (setf (node-tail-p call) nil)
  (annotate-continuation (basic-combination-fun call) 0)
  t)

(defun annotate-basic-combination (call)
  ;; Annotate the function.
  (let ((kind (basic-combination-kind call)))
    (case kind
      (:local
       (annotate-local-call call))
      (:full
       (annotate-full-call call))
      (t
       (unless (and (function-info-byte-compile kind)
		    (funcall (or (function-info-byte-annotate kind)
				 #'annotate-known-call)
			     call))
	 (setf (basic-combination-kind call) :full)
	 (annotate-full-call call)))))

  ;; If this is (still) a tail-call, then blow away the return.
  (when (node-tail-p call)
    (node-ends-block call)
    (let ((block (node-block call)))
      (unlink-blocks block (first (block-succ block)))
      (link-blocks block (component-tail (block-component block)))))
  (undefined-value))

(defun annotate-if (if)
  ;; Annotate the test.
  (let* ((cont (if-test if))
	 (use (continuation-use cont)))
    (annotate-continuation
     cont
     (if (and (combination-p use)
	      (eq (continuation-function-name (combination-fun use)) 'eq)
	      (= (length (combination-args use)) 2))
	 ;; If the test is a call to EQ, then we can use branch-if-eq
	 ;; so don't need to actually funcall the test.
	 :eq-test
	 ;; Otherwise, funcall the test for 1 value.
	 1))))

(defun annotate-return (return)
  (let ((cont (return-result return)))
    (annotate-continuation
     cont
     (nth-value 1 (values-types (continuation-derived-type cont))))))

(defun annotate-exit (exit)
  (let ((cont (exit-value exit)))
    (when cont
      (annotate-continuation cont :unknown))))

(defun annotate-block (block)
  (do-nodes (node cont block)
    (etypecase node
      (bind)
      (ref)
      (cset (annotate-set node))
      (basic-combination (annotate-basic-combination node))
      (cif (annotate-if node))
      (creturn (annotate-return node))
      (entry)
      (exit (annotate-exit node))))
  (undefined-value))

(defun annotate-ir1 (component)
  (do-blocks (block component)
    (when (block-interesting block)
      (annotate-block block)))
  (undefined-value))



;;;; Stack analysis.

(defun compute-produces-and-consumes (block)
  (let ((stack nil)
	(consumes nil)
	(total-consumes (make-sset))
	(nlx-entries nil)
	(nlx-entry-p nil)
	(counter 0))
    (labels ((interesting (cont)
	       (and cont
		    (let ((info (continuation-info cont)))
		      (and info
			   (not (member (byte-continuation-info-results info)
					'(0 :eq-test)))))))
	     (consume (cont)
	       (cond ((not (interesting cont)))
		     (stack
		      (assert (eq (car stack) cont))
		      (pop stack))
		     (t
		      (let ((info (continuation-info cont)))
			(unless (byte-continuation-info-number info)
			  (setf (byte-continuation-info-number info)
				(incf counter)))
			(sset-adjoin info total-consumes))
		      (push cont consumes)))))
      (do-nodes (node cont block)
	(etypecase node
	  (bind)
	  (ref)
	  (cset
	   (consume (set-value node)))
	  (basic-combination
	   (dolist (arg (reverse (basic-combination-args node)))
	     (when arg
	       (consume arg)))
	   (consume (basic-combination-fun node))
	   (when (eq (continuation-function-name
		      (basic-combination-fun node))
		     '%nlx-entry)
	     (let ((nlx-info (continuation-value
			      (first (basic-combination-args node)))))
	       (when (eq (cleanup-kind (nlx-info-cleanup nlx-info)) :block)
		 (push (nlx-info-continuation nlx-info) stack)))
	     (setf nlx-entry-p t)))
	  (cif
	   (consume (if-test node)))
	  (creturn
	   (consume (return-result node)))
	  (entry
	   (let ((nlx-info (cleanup-nlx-info (entry-cleanup node))))
	     (when nlx-info
	       (push (list nlx-info stack (reverse consumes)) nlx-entries))))
	  (exit
	   (when (exit-value node)
	     (consume (exit-value node)))))
	(when (and (not (exit-p node)) (interesting cont))
	  (push cont stack))))
      (setf (block-info block)
	    (make-byte-block-info block
				  :produces stack
				  :produces-sset
				  (let ((result (make-sset)))
				    (dolist (product stack)
				      (sset-adjoin (continuation-info product)
						   result))
				    result)
				  :consumes (reverse consumes)
				  :total-consumes total-consumes
				  :nlx-entries nlx-entries
				  :nlx-entry-p nlx-entry-p)))
  (undefined-value))

(defun walk-successors (block stack)
  (let ((tail (component-tail (block-component block))))
    (dolist (succ (block-succ block))
      (unless (or (eq succ tail)
		  (not (block-interesting succ))
		  (byte-block-info-nlx-entry-p (block-info succ)))
	(walk-block succ block stack)))))

(defun walk-nlx-entry (nlx-infos stack produce consume)
  (dolist (cont consume)
    (assert (eq (car stack) cont))
    (pop stack))
  (setf stack (append produce stack))
  (dolist (nlx-info nlx-infos)
    (walk-block (nlx-info-target nlx-info) nil stack)))

(defun walk-block (block pred stack)
  ;; Pop everything off of stack that isn't live.
  (let* ((info (block-info block))
	 (live (byte-block-info-total-consumes info)))
    (collect ((pops))
      (let ((fixed 0))
	(flet ((flush-fixed ()
		 (unless (zerop fixed)
		   (pops `(%byte-pop-stack ,fixed))
		   (setf fixed 0))))
	  (loop
	    (unless stack
	      (return))
	    (let* ((cont (car stack))
		   (info (continuation-info cont)))
	      (when (sset-member info live)
		(return))
	      (pop stack)
	      (let ((results (byte-continuation-info-results info)))
		(case results
		  (:unknown
		   (flush-fixed)
		   (pops `(%byte-pop-stack 0)))
		  (:fdefinition
		   (incf fixed))
		  (t
		   (incf fixed results))))))
	  (flush-fixed)))
      (when (pops)
	(assert pred)
	(let ((cleanup-block
	       (insert-cleanup-code pred block
				    (continuation-next (block-start block))
				    `(progn ,@(pops)))))
	  (annotate-block cleanup-block))))
    (cond ((eq (byte-block-info-start-stack info) :unknown)
	   ;; Record what the stack looked like at the start of this block.
	   (setf (byte-block-info-start-stack info) stack)
	   ;; Process any nlx entries that build off of our stack.
	   (dolist (stuff (byte-block-info-nlx-entries info))
	     (walk-nlx-entry (first stuff) stack (second stuff) (third stuff)))
	   ;; Remove whatever we consume.
	   (dolist (cont (byte-block-info-consumes info))
	     (assert (eq (car stack) cont))
	     (pop stack))
	   ;; Add whatever we produce.
	   (setf stack (append (byte-block-info-produces info) stack))
	   (setf (byte-block-info-end-stack info) stack)
	   ;; Pass that on to all our successors.
	   (walk-successors block stack))
	  (t
	   ;; We have already processed the successors of this block.  Just
	   ;; make sure we thing the stack is the same now as before.
	   (assert (equal (byte-block-info-start-stack info) stack)))))
  (undefined-value))

(defun byte-stack-analyze (component)
  (let ((head nil))
    (do-blocks (block component)
      (when (block-interesting block)
	(compute-produces-and-consumes block)
	(push block head)
	(setf (byte-block-info-already-queued (block-info block)) t)))
    (let ((tail (last head)))
      (labels ((maybe-enqueue (block)
		 (when (block-interesting block)
		   (let ((info (block-info block)))
		     (unless (byte-block-info-already-queued info)
		       (setf (byte-block-info-already-queued info) t)
		       (let ((new (list block)))
			 (if head
			     (setf (cdr tail) new)
			     (setf head new))
			 (setf tail new))))))
	       (maybe-enqueue-predecessors (block)
		 (dolist (pred (block-pred block))
		   (unless (eq pred (component-head (block-component block)))
		     (maybe-enqueue pred)))))
	(loop
	  (unless head
	    (return))
	  (let* ((block (pop head))
		 (info (block-info block))
		 (total-consumes (byte-block-info-total-consumes info))
		 (did-anything nil))
	    (setf (byte-block-info-already-queued info) nil)
	    (dolist (succ (block-succ block))
	      (unless (eq succ (component-tail component))
		(let ((succ-info (block-info succ)))
		  (when (sset-union-of-difference
			 total-consumes
			 (byte-block-info-total-consumes succ-info)
			 (byte-block-info-produces-sset succ-info))
		    (setf did-anything t)))))
	    (when did-anything
	      (maybe-enqueue-predecessors block)))))))
  (walk-successors (component-head component) nil)
  (undefined-value))



;;;; Actually generate the byte-code

(defvar *byte-component-info*)

(defconstant byte-push-local		#b00000000)
(defconstant byte-push-arg		#b00010000)
(defconstant byte-push-constant		#b00100000)
(defconstant byte-push-system-constant	#b00110000)
(defconstant byte-push-int		#b01000000)
(defconstant byte-push-neg-int		#b01010000)
(defconstant byte-pop-local		#b01100000)
(defconstant byte-pop-n			#b01110000)
(defconstant byte-call			#b10000000)
(defconstant byte-tail-call		#b10010000)
(defconstant byte-multiple-call		#b10100000)
(defconstant byte-named			#b00001000)
(defconstant byte-local-call		#b10110000)
(defconstant byte-local-tail-call	#b10111000)
(defconstant byte-local-multiple-call	#b11000000)
(defconstant byte-return		#b11001000)
(defconstant byte-branch-always		#b11010000)
(defconstant byte-branch-if-true	#b11010010)
(defconstant byte-branch-if-false	#b11010100)
(defconstant byte-branch-if-eq		#b11010110)
(defconstant byte-xop			#b11011000)
(defconstant byte-inline-function	#b11100000)


(defun output-push-int (segment int)
  (declare (type new-assem:segment segment)
	   (type (integer #.(- (ash 1 32)) #.(1- (ash 1 32)))))
  (if (minusp int)
      (output-byte-with-operand segment byte-push-neg-int (- (1+ int)))
      (output-byte-with-operand segment byte-push-int int)))

(defun output-push-constant-leaf (segment constant)
  (declare (type new-assem:segment segment)
	   (type constant constant))
  (let ((info (constant-info constant)))
    (if info
	(output-byte-with-operand segment
				  (ecase (car info)
				    (:system-constant
				     byte-push-system-constant)
				    (:local-constant
				     byte-push-constant))
				  (cdr info))
	(let ((const (constant-value constant)))
	  (if (and (integerp const) (< (- (ash 1 32)) const (ash 1 32)))
	      ;; It can be represented as an immediate.
	      (output-push-int segment const)
	      ;; We need to store it in the constants pool.
	      (let* ((posn (position const *system-constants*))
		     (new-info (if posn
				   (cons :system-constant posn)
				   (cons :local-constant
					 (vector-push-extend
					  constant
					  (byte-component-info-constants
					   *byte-component-info*))))))
		(setf (constant-info constant) new-info)
		(output-push-constant-leaf segment constant)))))))

(defun output-push-constant (segment value)
  (if (and (integerp value)
	   (< (- (ash 1 32)) value (ash 1 32)))
      (output-push-int segment value)
      (output-push-constant-leaf segment (find-constant value))))

(defun output-push-load-time-constant (segment kind datum)
  (let* ((constants (byte-component-info-constants *byte-component-info*))
	 (index (or (position-if #'(lambda (x)
				     (and (consp x)
					  (eq (car x) kind)
					  (eq (cdr x) datum)))
				 constants)
		    (vector-push-extend (cons kind datum) constants))))
    (output-byte-with-operand segment byte-push-constant index))
  (undefined-value))

(defun output-do-inline-function (segment function)
  ;; Note: we don't annotate this as a call site, because it is used for
  ;; internal stuff.  Random functions that get inlined have code locations
  ;; added byte generate-byte-code-for-full-call below.
  (output-byte segment
	       (logior byte-inline-function
		       (inline-function-number-or-lose function))))

(defun output-do-xop (segment xop)
  (let ((index (xop-index-or-lose xop)))
    (cond ((< index 7)
	   (output-byte segment (logior byte-xop index)))
	  (t
	   (output-byte segment (logior byte-xop 7))
	   (output-byte segment index)))))

(defun output-ref-lambda-var (segment var env
				     &optional (indirect-value-cells t))
  (declare (type new-assem:segment segment)
	   (type lambda-var var)
	   (type environment env))
  (if (eq (lambda-environment (lambda-var-home var)) env)
      (let ((info (leaf-info var)))
	(output-byte-with-operand segment
				  (if (byte-lambda-var-info-argp info)
				      byte-push-arg
				      byte-push-local)
				  (byte-lambda-var-info-offset info)))
      (output-byte-with-operand segment
				byte-push-arg
				(position var (environment-closure env))))
  (when (and indirect-value-cells (lambda-var-indirect var))
    (output-do-inline-function segment 'value-cell-ref)))

(defun output-ref-nlx-info (segment info env)
  (if (eq (node-environment (cleanup-mess-up (nlx-info-cleanup info))) env)
      (output-byte-with-operand segment
				byte-push-local
				(byte-nlx-info-stack-slot
				 (nlx-info-info info)))
      (output-byte-with-operand segment
				byte-push-arg
				(position info (environment-closure env)))))

(defun output-set-lambda-var (segment var env &optional make-value-cells)
  (declare (type new-assem:segment segment)
	   (type lambda-var var)
	   (type environment env))
  (let ((indirect (lambda-var-indirect var)))
    (cond ((not (eq (lambda-environment (lambda-var-home var)) env))
	   ;; This is not this guys home environment.  So we need to get it
	   ;; the value cell out of the closure, and fill it in.
	   (assert indirect)
	   (assert (not make-value-cells))
	   (output-byte-with-operand segment byte-push-arg
				     (position var (environment-closure env)))
	   (output-do-inline-function segment 'value-cell-setf))
	  (t
	   (let* ((pushp (and indirect (not make-value-cells)))
		  (byte-code (if pushp byte-push-local byte-pop-local))
		  (info (leaf-info var)))
	     (assert (not (byte-lambda-var-info-argp info)))
	     (when (and indirect make-value-cells)
	       ;; Replace the stack top with a value cell holding the
	       ;; stack top.
	       (output-do-inline-function segment 'make-value-cell))
	     (output-byte-with-operand segment byte-code
				       (byte-lambda-var-info-offset info))
	     (when pushp
	       (output-do-inline-function segment 'value-cell-setf)))))))

;;; CANONICALIZE-VALUES -- internal.
;;;
;;; Output whatever noise is necessary to canonicalize the values on the
;;; top of the stack.  Desired is the number we want, and supplied is the
;;; number we have.  Either push NIL or pop-n to make them balanced.
;;; Note: either desired or supplied can be :unknown, in which case it means
;;; use the ``unknown-values'' convention (which is the stack values followed
;;; by the number of values).
;;; 
(defun canonicalize-values (segment desired supplied)
  (declare (type new-assem:segment segment)
	   (type (or (member :unknown) index) desired supplied))
  (cond ((eq desired :unknown)
	 (unless (eq supplied :unknown)
	   (output-byte-with-operand segment byte-push-int supplied)))
	((eq supplied :unknown)
	 (unless (eq desired :unknown)
	   (output-push-int segment desired)
	   (output-do-xop segment 'default-unknown-values)))
	((< supplied desired)
	 (dotimes (i (- desired supplied))
	   (output-push-constant segment nil)))
	((> supplied desired)
	 (output-byte-with-operand segment byte-pop-n (- supplied desired))))
  (undefined-value))

(defun generate-byte-code-for-bind (segment bind cont)
  (declare (type new-assem:segment segment) (type bind bind)
	   (ignore cont))
  (let ((lambda (bind-lambda bind))
	(env (node-environment bind)))
    (ecase (lambda-kind lambda)
      ((nil :top-level :escape :cleanup :optional)
       (let* ((info (lambda-info lambda))
	      (frame-size (byte-lambda-info-stack-size info)))
	 (cond ((< frame-size (* 255 2))
		(output-byte segment (ceiling frame-size 2)))
	       (t
		(output-byte segment 255)
		(output-byte segment (ldb (byte 8 16) frame-size))
		(output-byte segment (ldb (byte 8 8) frame-size))
		(output-byte segment (ldb (byte 8 0) frame-size))))
	 (loop
	   for argnum upfrom 0
	   for var in (lambda-vars lambda)
	   for info = (lambda-var-info var)
	   do (unless (or (null info) (byte-lambda-var-info-argp info))
		(output-byte-with-operand segment byte-push-arg argnum)
		(output-set-lambda-var segment var env t)))))
      ((:let :mv-let :assignment)
       ;; Everything has been taken care of in the combination node.
       )))
  (undefined-value))

(defun generate-byte-code-for-ref (segment ref cont)
  (declare (type new-assem:segment segment) (type ref ref)
	   (type continuation cont))
  (let ((values (byte-continuation-info-results (continuation-info cont)))
	(leaf (ref-leaf ref)))
    (cond
     ((eq values :fdefinition)
      (assert (and (global-var-p leaf)
		   (eq (global-var-kind leaf)
		       :global-function)))
      (output-push-load-time-constant segment
				      :fdefinition
				      (global-var-name leaf)))
     ((eql values 0)
      ;; Real easy!
      nil)
     (t
      (etypecase leaf
	(constant
	 (output-push-constant-leaf segment leaf))
	(clambda
	 (let* ((refered-env (lambda-environment leaf))
		(closure (environment-closure refered-env)))
	   (if (null closure)
	       (output-push-load-time-constant segment :entry leaf)
	       (let ((my-env (node-environment ref)))
		 (output-push-load-time-constant segment :xep leaf)
		 (dolist (thing closure)
		   (etypecase thing
		     (lambda-var
		      (output-ref-lambda-var segment thing my-env nil))
		     (nlx-info
		      (output-ref-nlx-info segment thing my-env))))
		 (output-push-int segment (length closure))
		 (output-do-xop segment 'make-closure)))))
	(functional
	 (output-push-load-time-constant segment :entry leaf))
	(lambda-var
	 (output-ref-lambda-var segment leaf (node-environment ref)))
	(global-var
	 (ecase (global-var-kind leaf)
	   ((:special :global :constant)
	    (output-push-constant segment (global-var-name leaf))
	    (output-do-inline-function segment 'symbol-value))
	   (:global-function
	    (output-push-load-time-constant segment
					    :fdefinition
					    (global-var-name leaf))
	    (output-do-xop segment 'fdefn-function-or-lose)))))
      (canonicalize-values segment values 1))))
  (undefined-value))

(defun generate-byte-code-for-set (segment set cont)
  (declare (type new-assem:segment segment) (type cset set)
	   (type continuation cont))
  (let* ((leaf (set-var set))
	 (info (continuation-info cont))
	 (values (if info
		     (byte-continuation-info-results info)
		     0)))
    (unless (eql values 0)
      ;; Someone wants the value, so copy it.
      (output-do-xop segment 'dup))
    (etypecase leaf
      (global-var
       (ecase (global-var-kind leaf)
	 ((:special :global)
	  (output-push-constant segment (global-var-name leaf))
	  (output-do-inline-function segment 'setf-symbol-value))))
      (lambda-var
       (output-set-lambda-var segment leaf (node-environment set))))
    (unless (eql values 0)
      (canonicalize-values segment values 1)))
  (undefined-value))

(defun generate-byte-code-for-local-call (segment call cont num-args)
  (let* ((lambda (combination-lambda call))
	 (env (lambda-environment lambda)))
    (ecase (functional-kind lambda)
      ((:let :mv-let :assignment)
       (let ((vars (lambda-vars lambda)))
	 (dolist (var (reverse vars))
	   (when (lambda-var-refs var)
	     (output-set-lambda-var segment var env t)))))
      ((nil :optional :cleanup)
       ;; We got us a local call.  
       (assert (not (eq num-args :unknown)))
       ;; First push closure vars.
       (let ((closure (environment-closure env)))
	 (when closure
	   (let ((my-env (node-environment call)))
	     (dolist (thing (reverse closure))
	       (etypecase thing
		 (lambda-var
		  (output-ref-lambda-var segment thing my-env nil))
		 (nlx-info
		  (output-ref-nlx-info segment thing my-env)))))
	   (incf num-args (length closure))))
       (let ((results
	      (let ((info (continuation-info cont)))
		(if info
		    (byte-continuation-info-results info)
		    0))))
	 ;; Emit the op for whatever flavor of call we are using.
	 (let ((operand
		(cond ((> num-args 6)
		       (output-push-int segment num-args)
		       7)
		      (t
		       num-args))))
	   ;; ### :call-site
	   (output-byte segment
			(logior (cond ((node-tail-p call)
				       byte-local-tail-call)
				      ((member results '(0 1))
				       byte-local-call)
				      (t
				       byte-local-multiple-call))
				operand)))
	 ;; Emit a reference to the label.
	 (output-reference segment
			   (byte-lambda-info-label (lambda-info lambda)))
	 ;; ### :unknown-return
	 ;; Fix up the results.
	 (case results
	   ((:unknown 1)
	    ;; Don't need to do anything.
	    )
	   (0
	    ;; Get rid of the one value.
	    (output-byte-with-operand segment byte-pop-n 1))
	   (t
	    ;; Something strange.
	    (canonicalize-values segment results :unknown)))))))
  (undefined-value))

(defun generate-byte-code-for-full-call (segment call cont num-args)
  (let ((info (basic-combination-info call))
	(results
	 (let ((info (continuation-info cont)))
	   (if info
	       (byte-continuation-info-results info)
	       0))))
    (cond
     (info
      ;; It's an inline function.
      (assert (not (node-tail-p call)))
      (let* ((type (inline-function-info-type info))
	     (desired-args (function-type-nargs type))
	     (supplied-results
	      (nth-value 1
			 (values-types (function-type-returns type)))))
	(canonicalize-values segment desired-args num-args)
	;; ### :call-site
	(output-byte segment (logior byte-inline-function
				    (inline-function-info-number info)))
	;; ### :known-return
	(canonicalize-values segment results supplied-results)))
     (t
      (let ((operand
	     (cond ((eq num-args :unknown)
		    7)
		   ((> num-args 6)
		    (output-push-int segment num-args)
		    7)
		   (t
		    num-args))))
	(when (eq (byte-continuation-info-results
		   (continuation-info
		    (basic-combination-fun call)))
		  :fdefinition)
	  (setf operand (logior operand byte-named)))
	;; ### :call-site
	(if (node-tail-p call)
	    (output-byte segment (logior byte-tail-call operand))
	    (case results
	      (:unknown
	       (output-byte segment (logior byte-multiple-call operand))
	       ;; ### :unknown-return
	       )
	      (0
	       (output-byte segment (logior byte-call operand))
	       ;; ### :unknown-return
	       (output-byte-with-operand segment byte-pop-n 1))
	      (1
	       (output-byte segment (logior byte-call operand))
	       ;; ### :unknown-return
	       )
	      (t
	       (output-byte segment (logior byte-multiple-call operand))
	       ;; ### :unknown-return
	       (canonicalize-values segment results :unknown)))))))))

(defun generate-byte-code-for-known-call (segment call cont num-args)
  (block nil
    (catch 'give-up
      (funcall (function-info-byte-compile (basic-combination-kind call)) call
	       (let ((info (continuation-info cont)))
		 (if info
		     (byte-continuation-info-results info)
		     0))
	       num-args segment)
      (return))
    (assert (member (byte-continuation-info-results
		     (continuation-info
		      (basic-combination-fun call)))
		    '(1 :fdefinition)))
    (generate-byte-code-for-full-call segment call cont num-args))
  (undefined-value))

(defun generate-byte-code-for-generic-combination (segment call cont)
  (declare (type new-assem:segment segment) (type basic-combination call)
	   (type continuation cont))
  (let ((num-args
	 (labels
	     ((examine (args num-fixed)
		(cond
		 ((null args)
		  ;; None of the arugments supply :unknown values, so
		  ;; we know exactly how many there are.
		  num-fixed)
		 ((null (car args))
		  ;; This arg has been deleted.  Ignore it.
		  (examine (cdr args) num-fixed))
		 (t
		  (let* ((vals
			  (byte-continuation-info-results
			   (continuation-info (car args)))))
		    (cond
		     ((eq vals :unknown)
		      (unless (null (cdr args))
			;; There are (length args) :unknown value blocks on
			;; the top of the stack.  We need to combine them.
			(output-push-int segment (length args))
			(output-do-xop segment 'merge-unknown-values))
		      (unless (zerop num-fixed)
			;; There are num-fixed fixed args above the unknown
			;; values block that want in on the action also.
			;; So add num-fixed to the count.
			(output-push-int segment num-fixed)
			(output-do-inline-function segment '+))
		      :unknown)
		     (t
		      (examine (cdr args) (+ num-fixed vals)))))))))
	   (examine (basic-combination-args call) 0))))
    (case (basic-combination-kind call)
      (:local
       (generate-byte-code-for-local-call segment call cont num-args))
      (:full
       (generate-byte-code-for-full-call segment call cont num-args))
      (t
       (generate-byte-code-for-known-call segment call cont num-args)))))

(defun generate-byte-code-for-basic-combination (segment call cont)
  (cond ((and (mv-combination-p call)
	      (eq (continuation-function-name (basic-combination-fun call))
		  '%throw))
	 ;; ### :internal-error
	 (output-do-xop segment 'throw)
	 (canonicalize-values segment
			      (byte-continuation-info-results
			       (continuation-info cont))
			      0))
	(t
	 (generate-byte-code-for-generic-combination segment call cont))))

(defun generate-byte-code-for-if (segment if cont)
  (declare (type new-assem:segment segment) (type cif if)
	   (ignore cont))
  (let* ((next-info (byte-block-info-next (block-info (node-block if))))
	 (consequent-info (block-info (if-consequent if)))
	 (alternate-info (block-info (if-alternative if))))
    (cond ((eq (byte-continuation-info-results
		(continuation-info (if-test if)))
	       :eq-test)
	   (output-branch segment
			  byte-branch-if-eq
			  (byte-block-info-label consequent-info))
	   (unless (eq next-info alternate-info)
	     (output-branch segment
			    byte-branch-always
			    (byte-block-info-label alternate-info))))
	  ((eq next-info consequent-info)
	   (output-branch segment
			  byte-branch-if-false
			  (byte-block-info-label alternate-info)))
	  (t
	   (output-branch segment
			  byte-branch-if-true
			  (byte-block-info-label consequent-info))
	   (unless (eq next-info alternate-info)
	     (output-branch segment
			    byte-branch-always
			    (byte-block-info-label alternate-info)))))))

(defun generate-byte-code-for-return (segment return cont)
  (declare (type new-assem:segment segment) (type creturn return)
	   (ignore cont))
  (let* ((result (return-result return))
	 (info (continuation-info result))
	 (results (byte-continuation-info-results info)))
    (cond ((eq results :unknown)
	   (setf results 7))
	  ((> results 6)
	   (output-byte-with-operand segment byte-push-int results)
	   (setf results 7)))
    (output-byte segment (logior byte-return results)))
  (undefined-value))

(defun generate-byte-code-for-entry (segment entry cont)
  (declare (type new-assem:segment segment) (type entry entry)
	   (ignore cont))
  (dolist (exit (entry-exits entry))
    (let ((nlx-info (find-nlx-info entry (node-cont exit))))
      (when nlx-info
	(let ((kind (cleanup-kind (nlx-info-cleanup nlx-info))))
	  (when (member kind '(:block :tagbody))
	    ;; Generate a unique tag.
	    (output-do-inline-function segment 'cons-unique-tag)
	    ;; Save it so people can close over it.
	    (output-do-xop segment 'dup)
	    (output-byte-with-operand segment
				      byte-pop-local
				      (byte-nlx-info-stack-slot
				       (nlx-info-info nlx-info)))
	    ;; Now do the actual XOP.
	    (ecase kind
	      (:block
	       (output-do-xop segment 'catch)
	       (output-reference segment
				 (byte-nlx-info-label
				  (nlx-info-info nlx-info))))
	      (:tagbody
	       (output-do-xop segment 'tagbody)))
	    (return))))))
  (undefined-value))

(defun generate-byte-code-for-exit (segment exit cont)
  (declare (ignore cont))
  (let ((nlx-info (find-nlx-info (exit-entry exit) (node-cont exit))))
    (output-byte-with-operand segment
			      byte-push-arg
			      (position nlx-info
					(environment-closure
					 (node-environment exit))))
    (ecase (cleanup-kind (nlx-info-cleanup nlx-info))
      (:block
       ;; ### :internal-error
       (output-do-xop segment 'return-from))
      (:tagbody
       ;; ### :internal-error
       (output-do-xop segment 'go)
       (output-reference segment
			 (byte-nlx-info-label (nlx-info-info nlx-info)))))))

(defun generate-byte-code (segment component)
  (let ((*byte-component-info* (component-info component)))
    (do* ((info (byte-block-info-next (block-info (component-head component)))
		next)
	  (block (byte-block-info-block info) (byte-block-info-block info))
	  (next (byte-block-info-next info) (byte-block-info-next info)))
	 ((eq block (component-tail component)))
      (when (block-interesting block)
	(output-label segment (byte-block-info-label info))
	(do-nodes (node cont block)
		  (etypecase node
		    (bind (generate-byte-code-for-bind segment node cont))
		    (ref (generate-byte-code-for-ref segment node cont))
		    (cset (generate-byte-code-for-set segment node cont))
		    (basic-combination
		     (generate-byte-code-for-basic-combination
		      segment node cont))
		    (cif (generate-byte-code-for-if segment node cont))
		    (creturn (generate-byte-code-for-return segment node cont))
		    (entry (generate-byte-code-for-entry segment node cont))
		    (exit
		     (when (exit-entry node)
		       (generate-byte-code-for-exit segment node cont)))))
	(let* ((succ (block-succ block))
	       (first-succ (car succ)))
	  (unless (or (cdr succ)
		      (eq (byte-block-info-block next) first-succ)
		      (eq (component-tail component) first-succ))
	    (output-branch segment
			   byte-branch-always
			   (byte-block-info-label
			    (block-info first-succ))))))))
  (undefined-value))


;;;; Special purpose annotate/compile optimizers.

(defoptimizer (eq byte-annotate) ((this that) node)
  (declare (ignore this that))
  (when (if-p (continuation-dest (node-cont node)))
    (annotate-known-call node)
    t))

(defoptimizer (eq byte-compile) ((this that) call results num-args segment)
  (progn segment) ; ignorable.
  ;; We don't have to do anything, because everything is handled by the
  ;; IF byte-generator.
  (assert (eq results :eq-test))
  (assert (eql num-args 2))
  (undefined-value))


(defoptimizer (values byte-compile)
	      ((&rest values) node results num-args segment)
  (canonicalize-values segment results num-args))


(defknown %byte-pop-stack (index) (values))

(defoptimizer (%byte-pop-stack byte-annotate) ((count) node)
  (assert (constant-continuation-p count))
  (annotate-continuation count 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%byte-pop-stack byte-compile)
	      ((count) node results num-args segment)
  (assert (eql num-args 0))
  (output-byte-with-operand segment byte-pop-n (continuation-value count))
  (canonicalize-values segment results 0))


(defoptimizer (%special-bind byte-annotate) ((var value) node)
  (annotate-continuation var 0)
  (annotate-continuation value 1)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%special-bind byte-compile)
	      ((var value) node results num-args segment)
  (assert (eql num-args 1))
  (output-push-constant segment (leaf-name (continuation-value var)))
  (output-do-inline-function segment '%byte-special-bind)
  (canonicalize-values segment results 0))

(defoptimizer (%special-unbind byte-annotate) ((var) node)
  (annotate-continuation var 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)
  
(defoptimizer (%special-unbind byte-compile)
	      ((var) node results num-args segment)
  (assert (eql num-args 0))
  (output-do-inline-function segment '%byte-special-unbind)
  (canonicalize-values segment results 0))

(defoptimizer (%catch byte-annotate) ((nlx-info tag) node)
  (annotate-continuation nlx-info 0)
  (annotate-continuation tag 1)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%catch byte-compile)
	      ((nlx-info tag) node results num-args segment)
  (progn node) ; ignore
  (assert (= num-args 1))
  (output-do-xop segment 'catch)
  (let ((info (nlx-info-info (continuation-value nlx-info))))
    (output-reference segment (byte-nlx-info-label info)))
  (canonicalize-values segment results 0))

(defoptimizer (%cleanup-point byte-compile) (() node results num-args segment)
  (progn node) ; ignore
  (assert (eql num-args 0))
  (canonicalize-values segment results 0))


(defoptimizer (%catch-breakup byte-compile) (() node results num-args segment)
  (progn node) ; ignore
  (assert (eql num-args 0))
  (output-do-xop segment 'breakup)
  (canonicalize-values segment results 0))


(defoptimizer (%lexical-exit-breakup byte-annotate) ((nlx-info) node)
  (annotate-continuation nlx-info 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%lexical-exit-breakup byte-compile)
	      ((nlx-info) node results num-args segment)
  (assert (eql num-args 0))
  (let ((nlx-info (continuation-value nlx-info)))
    (when (ecase (cleanup-kind (nlx-info-cleanup nlx-info))
	    (:catch t)
	    (:block
	     ;; We only want to do this for the fall-though case.
	     (not (eq (car (block-pred (node-block node)))
		      (nlx-info-target nlx-info))))
	    (:tagbody
	     ;; Only want to do it once per tagbody.
	     (not (byte-nlx-info-duplicate (nlx-info-info nlx-info)))))
      (output-do-xop segment 'breakup)
      (canonicalize-values segment results 0))))


(defoptimizer (%nlx-entry byte-annotate) ((nlx-info) node)
  (annotate-continuation nlx-info 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%nlx-entry byte-compile)
	      ((nlx-info) node results num-args segment)
  (progn node results) ; ignore
  (assert (eql num-args 0))
  (let* ((info (continuation-value nlx-info))
	 (byte-info (nlx-info-info info)))
    (output-label segment (byte-nlx-info-label byte-info))
    ;; ### :non-local-entry
    (ecase (cleanup-kind (nlx-info-cleanup info))
      ((:catch :block)
       (canonicalize-values segment
			    (byte-continuation-info-results
			     (continuation-info
			      (nlx-info-continuation info)))
			    :unknown))
      ((:tagbody :unwind-protect)))))


(defoptimizer (%unwind-protect byte-annotate)
	      ((nlx-info cleanup-fun) node)
  (annotate-continuation nlx-info 0)
  (annotate-continuation cleanup-fun 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)
  
(defoptimizer (%unwind-protect byte-compile)
	      ((nlx-info cleanup-fun) node results num-args segment)
  (assert (eql num-args 0))
  (output-do-xop segment 'unwind-protect)
  (output-reference segment
		    (byte-nlx-info-label
		     (nlx-info-info
		      (continuation-value nlx-info))))
  (canonicalize-values segment results 0))

(defoptimizer (%unwind-protect-breakup byte-compile)
	      (() node results num-args segment)
  (progn node) ; ignore
  (assert (eql num-args 0))
  (output-do-xop segment 'breakup)
  (canonicalize-values segment results 0))

(defoptimizer (%continue-unwind byte-annotate) ((a b c) node)
  (annotate-continuation a 0)
  (annotate-continuation b 0)
  (annotate-continuation c 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)
  
(defoptimizer (%continue-unwind byte-compile)
	      ((a b c) node results num-args segment)
  (progn node) ; ignore
  (assert (member results '(0 nil)))
  (assert (eql num-args 0))
  (output-do-xop segment 'breakup))


;;;; XEP descriptions.

(defstruct (byte-xep
	    (:print-function %print-byte-xep))
  ;;
  ;; The component that this XEP is an entry point into.  NIL until
  ;; LOAD or MAKE-CORE-BYTE-COMPONENT fills it in.  We also allow
  ;; cons so that the dumper can stick in a unique cons cell to
  ;; identify what component it is trying to dump.  What a hack.
  (component nil :type (or null kernel:code-component cons))
  ;;
  ;; The minimum and maximum number of args, ignoring &rest and &key.
  (min-args 0 :type (integer 0 #.call-arguments-limit))
  (max-args 0 :type (integer 0 #.call-arguments-limit))
  ;;
  ;; List of the entry points for min-args, min-args+1, ... max-args.
  (entry-points nil :type list)
  ;;
  ;; The entry point to use when there are more than max-args.  Only filled
  ;; in where okay.  In other words, only when &rest or &key is specified.
  (more-args-entry-point nil :type (or null (unsigned-byte 24)))
  ;;
  ;; The number of ``more-arg'' args.
  (num-more-args 0 :type (integer 0 #.call-arguments-limit))
  ;;
  ;; True if there is a rest-arg.
  (rest-arg-p nil :type (member t nil))
  ;;
  ;; True if there are keywords.  Note: keywords might still be NIL because
  ;; having &key with no keywords is valid and should result in
  ;; allow-other-keys processing.  If :allow-others, then allow other keys.
  (keywords-p nil :type (member t nil :allow-others))
  ;;
  ;; List of keyword arguments.  Each element is a list of:
  ;;   key, default, supplied-p.
  (keywords nil :type list))

(defprinter byte-xep)


;;; MAKE-XEP-FOR -- internal
;;;
;;; Make a byte-xep for LAMBDA.
;;; 
(defun make-xep-for (lambda)
  (flet ((entry-point-for (entry)
	   (let ((info (lambda-info entry)))
	     (assert (byte-lambda-info-interesting info))
	     (new-assem:label-position (byte-lambda-info-label info)))))
    (let ((entry (lambda-entry-function lambda)))
      (etypecase entry
	(optional-dispatch
	 (let ((rest-arg-p nil))
	   (collect ((keywords))
	     (dolist (var (nthcdr (optional-dispatch-max-args entry)
				  (optional-dispatch-arglist entry)))
	       (let ((arg-info (lambda-var-arg-info var)))
		 (assert arg-info)
		 (ecase (arg-info-kind arg-info)
		   (:rest
		    (assert (not rest-arg-p))
		    (setf rest-arg-p t))
		   (:keyword
		    (keywords (list (arg-info-keyword arg-info)
				    (arg-info-default arg-info)
				    (if (arg-info-supplied-p arg-info) t)))))))
	     (make-byte-xep
	      :min-args (optional-dispatch-min-args entry)
	      :max-args (optional-dispatch-max-args entry)
	      :entry-points
	      (mapcar #'entry-point-for
		      (optional-dispatch-entry-points entry))
	      :more-args-entry-point
	      (entry-point-for (optional-dispatch-main-entry entry))
	      :rest-arg-p rest-arg-p
	      :keywords-p
	      (if (optional-dispatch-keyp entry)
		  (if (optional-dispatch-allowp entry)
		      :allow-others t))
	      :keywords (keywords)))))
	(clambda
	 (let ((args (length (lambda-vars entry))))
	   (make-byte-xep
	    :min-args args
	    :max-args args
	    :entry-points (list (entry-point-for entry)))))))))

(defun generate-xeps (component)
  (let ((xeps nil))
    (dolist (lambda (component-lambdas component))
      (when (member (lambda-kind lambda) '(:external :top-level))
	(push (cons lambda (make-xep-for lambda)) xeps)))
    xeps))



;;;; Noise to actually do the compile.

(defun assign-locals (component)
  ;;
  ;; Process all of the lambdas in component, and assign stack frame
  ;; locations for all the locals.
  (dolist (lambda (component-lambdas component))
    ;; We don't generate any code for :external lambdas, so we don't need
    ;; to allocate stack space.  Also, we don't use the ``more'' entry,
    ;; so we don't need code for it.
    (cond
     ((or (eq (lambda-kind lambda) :external)
	  (and (eq (lambda-kind lambda) :optional)
	       (eq (optional-dispatch-more-entry
		    (lambda-optional-dispatch lambda))
		   lambda)))
      (setf (lambda-info lambda)
	    (make-byte-lambda-info :interesting nil)))
     (t
      (let ((num-locals 0))
	(let* ((vars (lambda-vars lambda))
	       (arg-num (+ (length vars)
			   (length (environment-closure
				    (lambda-environment lambda))))))
	  (dolist (var vars)
	    (decf arg-num)
	    (cond ((or (lambda-var-sets var) (lambda-var-indirect var))
		   (setf (leaf-info var)
			 (make-byte-lambda-var-info :offset num-locals))
		   (incf num-locals))
		  ((leaf-refs var)
		   (setf (leaf-info var) 
			 (make-byte-lambda-var-info :argp t
						    :offset arg-num))))))
	(dolist (let (lambda-lets lambda))
	  (dolist (var (lambda-vars let))
	    (setf (leaf-info var)
		  (make-byte-lambda-var-info :offset num-locals))
	    (incf num-locals)))
	(let ((entry-nodes-already-done nil))
	  (dolist (nlx-info (environment-nlx-info (lambda-environment lambda)))
	    (ecase (cleanup-kind (nlx-info-cleanup nlx-info))
	      (:block
	       (setf (nlx-info-info nlx-info)
		     (make-byte-nlx-info :stack-slot num-locals))
	       (incf num-locals))
	      (:tagbody
	       (let* ((entry (cleanup-mess-up (nlx-info-cleanup nlx-info)))
		      (cruft (assoc entry entry-nodes-already-done)))
		 (cond (cruft
			(setf (nlx-info-info nlx-info)
			      (make-byte-nlx-info :stack-slot (cdr cruft)
						  :duplicate t)))
		       (t
			(push (cons entry num-locals) entry-nodes-already-done)
			(setf (nlx-info-info nlx-info)
			      (make-byte-nlx-info :stack-slot num-locals))
			(incf num-locals)))))
	      ((:catch :unwind-protect)
	       (setf (nlx-info-info nlx-info) (make-byte-nlx-info))))))
	(setf (lambda-info lambda)
	      (make-byte-lambda-info :stack-size num-locals))))))

  (undefined-value))


;;; BYTE-COMPILE-COMPONENT -- internal interface.
;;; 
(defun byte-compile-component (component)
  (setf (component-info component) (make-byte-component-info))

  ;; Assign offsets for all the locals, and figure out which args can
  ;; stay in the argument area and which need to be moved into locals.
  (assign-locals component)

  ;; Annotate every continuation with information about how we want the
  ;; values.
  (annotate-ir1 component)

  ;; Determine what stack values are dead, and emit cleanup code to pop
  ;; them.
  (byte-stack-analyze component)

  ;; Assign an ordering of the blocks.
  (control-analyze component #'make-byte-block-info)

  ;; Find the start labels for the lambdas.
  (dolist (lambda (component-lambdas component))
    (let ((info (lambda-info lambda)))
      (when (byte-lambda-info-interesting info)
	(setf (byte-lambda-info-label info)
	      (byte-block-info-label
	       (block-info (node-block (lambda-bind lambda))))))))

  ;; Delete any blocks that we are not going to emit from the emit order.
  (do-blocks (block component)
    (unless (block-interesting block)
      (let* ((info (block-info block))
	     (prev (byte-block-info-prev info))
	     (next (byte-block-info-next info)))
	(setf (byte-block-info-next prev) next)
	(setf (byte-block-info-prev next) prev))))

  (let ((segment nil))
    (unwind-protect
	(progn
	  (setf segment (new-assem:make-segment :name "Byte Output"))
	  (generate-byte-code segment component)
	  (let ((xeps (generate-xeps component))
		(constants (byte-component-info-constants
			    (component-info component))))
	    (when *compiler-trace-output*
	      (describe-component component *compiler-trace-output*))
	    (etypecase *compile-object*
	      (fasl-file
	       (maybe-mumble "FASL")
	       (fasl-dump-byte-component segment constants xeps
					 *compile-object*))
	      (core-object
	       (maybe-mumble "Core")
	       (make-core-byte-component segment constants xeps
					 *compile-object*))
	      (null))))
      (when segment
	(new-assem:release-segment segment))))
  (undefined-value))



;;;; Extra stuff for debugging.

(defun dump-stack-info (component)
  (do-blocks (block component)
     (when (block-interesting block)
       (print-nodes block)
       (let ((info (block-info block)))
	 (cond
	  (info
	   (format t
	   "start-stack ~S~%consume ~S~%produce ~S~%end-stack ~S~%~
	    total-consume ~S~%~@[nlx-entries ~S~%~]~@[nlx-entry-p ~S~%~]"
	   (byte-block-info-start-stack info)
	   (byte-block-info-consumes info)
	   (byte-block-info-produces info)
	   (byte-block-info-end-stack info)
	   (byte-block-info-total-consumes info)
	   (byte-block-info-nlx-entries info)
	   (byte-block-info-nlx-entry-p info)))
	  (t
	   (format t "no info~%")))))))



;;;; Hacks until we want to recompile everything.

#+nil
(defun block-label (block)
  (declare (type cblock block))
  (let ((info (block-info block)))
    (etypecase info
      (ir2-block
       (or (ir2-block-%label info)
	   (setf (ir2-block-%label info) (gen-label))))
      (byte-block-info
       (byte-block-info-label info)))))
