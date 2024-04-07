;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/assembly/assemfile.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the extra code necessary to feed an entire file of
;;; assembly code to the assembler.
;;;
(in-package "C")

(export '(define-assembly-routine))


(defvar *do-assembly* nil
  "If non-NIL, emit assembly code.  If NIL, emit VOP templates.")

(defvar *lap-output-file* nil
  "The FASL file currently being output to.")

(defvar *assembler-routines* nil
  "A List of (name . label) for every entry point.")

(defun assemble-file (name &key
			   (output-file
			    (make-pathname :defaults name
					   :type "assem"))
			   trace-file)
  (let* ((*do-assembly* t)
	 (name (pathname name))
	 (*lap-output-file* (open-fasl-file (pathname output-file) name))
	 (*assembler-routines* nil)
	 (*load-verbose* nil)
	 (won nil)
	 (*code-segment* nil)
	 (*elsewhere* nil)
	 (*assembly-optimize* nil)
	 (*compiler-trace-output* nil)
	 (*fixups* nil)
	 (*coalesce-constants* t))
    (unwind-protect
	(let ((*features* (cons :assembler (backend-features *backend*))))
	  (when trace-file
	    (setf *compiler-trace-output*
		  (open (if (eq trace-file t)
			    (make-pathname :defaults name
					   :type "trace")
			    trace-file)
			:direction :output
			:if-exists :supersede)))
	  (init-assembler)
	  (load (merge-pathnames name (make-pathname :type "lisp")))
	  (fasl-dump-cold-load-form `(in-package ,(package-name *package*))
				    *lap-output-file*)
	  (new-assem:append-segment *code-segment* *elsewhere*)
	  (setf *elsewhere* nil)
	  (let ((length (new-assem:finalize-segment *code-segment*)))
	    (dump-assembler-routines *code-segment*
				     length
				     *fixups*
				     *assembler-routines*
				     *lap-output-file*))
	  (setq won t))
      (new-assem:release-segment *code-segment*)
      (when *elsewhere*
	(new-assem:release-segment *elsewhere*))
      (when *compiler-trace-output*
	(close *compiler-trace-output*))
      (close-fasl-file *lap-output-file* (not won)))
    won))



(defstruct (reg-spec
	    (:print-function %print-reg-spec))
  (kind :temp :type (member :arg :temp :res))
  (name nil :type symbol)
  (temp nil :type symbol)
  (scs nil :type (or list symbol))
  (offset nil))

(defun %print-reg-spec (spec stream depth)
  (declare (ignore depth))
  (format stream
	  "#<reg ~S ~S scs=~S offset=~S>"
	  (reg-spec-kind spec)
	  (reg-spec-name spec)
	  (reg-spec-scs spec)
	  (reg-spec-offset spec)))

(defun reg-spec-sc (spec)
  (if (atom (reg-spec-scs spec))
      (reg-spec-scs spec)
      (car (reg-spec-scs spec))))

(defun parse-reg-spec (kind name sc offset)
  (let ((reg (make-reg-spec :kind kind :name name :scs sc :offset offset)))
    (ecase kind
      (:temp)
      ((:arg :res)
       (setf (reg-spec-temp reg) (make-symbol (symbol-name name)))))
    reg))


(defun emit-assemble (name options regs code)
  (collect ((decls))
    (loop
      (if (and (consp code) (consp (car code)) (eq (caar code) 'declare))
	  (decls (pop code))
	  (return)))
    `(let (,@(mapcar
	      #'(lambda (reg)
		  `(,(reg-spec-name reg)
		    (make-random-tn
		     :kind :normal
		     :sc (sc-or-lose ',(reg-spec-sc reg))
		     :offset ,(reg-spec-offset reg))))
	      regs))
       ,@(decls)
       (new-assem:assemble (*code-segment* ',name)
	 ,name
	 (push (cons ',name ,name) *assembler-routines*)
	 ,@code
	 ,@(generate-return-sequence
	    (or (cadr (assoc :return-style options)) :raw)))
       (when *compile-print*
	 (format *error-output* "; ~S assembled~%" ',name)))))

(defun arg-or-res-spec (reg)
  `(,(reg-spec-name reg)
    :scs ,(if (atom (reg-spec-scs reg))
	      (list (reg-spec-scs reg))
	      (reg-spec-scs reg))
    ,@(unless (eq (reg-spec-kind reg) :res)
	`(:target ,(reg-spec-temp reg)))))

(defun emit-vop (name options vars)
  (let* ((args (remove :arg vars :key #'reg-spec-kind :test-not #'eq))
	 (temps (remove :temp vars :key #'reg-spec-kind :test-not #'eq))
	 (results (remove :res vars :key #'reg-spec-kind :test-not #'eq))
	 (return-style (or (cadr (assoc :return-style options)) :raw))
	 (cost (or (cadr (assoc :cost options)) 247))
	 (vop (make-symbol "VOP")))
    (unless (member return-style '(:raw :full-call :none))
      (error "Unknown return-style for ~S: ~S" name return-style))
    (multiple-value-bind
	(call-sequence call-temps)
	(let ((*backend* *target-backend*))
	  (generate-call-sequence name return-style vop))
      `(define-vop ,(if (atom name) (list name) name)
	 (:args ,@(mapcar #'arg-or-res-spec args))
	 ,@(let ((index -1))
	     (mapcar #'(lambda (arg)
			 `(:temporary (:sc ,(reg-spec-sc arg)
					   :offset ,(reg-spec-offset arg)
					   :from (:argument ,(incf index))
					   :to (:eval 2))
				      ,(reg-spec-temp arg)))
		     args))
	 ,@(mapcar #'(lambda (temp)
		       `(:temporary (:sc ,(reg-spec-sc temp)
					 :offset ,(reg-spec-offset temp)
					 :from (:eval 1)
					 :to (:eval 3))
				    ,(reg-spec-name temp)))
		   temps)
	 ,@call-temps
	 (:vop-var ,vop)
	 ,@(let ((index -1))
	     (mapcar #'(lambda (res)
			 `(:temporary (:sc ,(reg-spec-sc res)
					   :offset ,(reg-spec-offset res)
					   :from (:eval 2)
					   :to (:result ,(incf index))
					   :target ,(reg-spec-name res))
				      ,(reg-spec-temp res)))
		     results))
	 (:results ,@(mapcar #'arg-or-res-spec results))
	 (:ignore ,@(mapcar #'reg-spec-name temps)
		  ,@(apply #'append
			   (mapcar #'cdr
				   (remove :ignore call-temps
					   :test-not #'eq :key #'car))))
	 ,@(remove-if #'(lambda (x)
			  (member x '(:return-style :cost)))
		      options
		      :key #'car)
	 (:generator ,cost
	   ,@(mapcar #'(lambda (arg)
			 (if (or (target-featurep :hppa)
				 (target-featurep :alpha))
			     `(move ,(reg-spec-name arg)
				    ,(reg-spec-temp arg))
			     `(move ,(reg-spec-temp arg)
				    ,(reg-spec-name arg))))
		     args)
	   ,@call-sequence
	   ,@(mapcar #'(lambda (res)
			 (if (or (target-featurep :hppa)
				 (target-featurep :alpha))
			     `(move ,(reg-spec-temp res)
				    ,(reg-spec-name res))
			     `(move ,(reg-spec-name res)
				    ,(reg-spec-temp res))))
		     results))))))

;;; Define-Assembly-Routine -- Public
;;;
;;;   Parse the code to produce an assembly routine and create a VOP
;;;   that calls the assembly routine.
(defmacro define-assembly-routine (name&options vars &rest code)
  "Define-Assembly-Routine (Name&Options Vars Code*)
  Define a Lisp assembly routine, and a VOP to that calls the assembly
  routine, if enabled.  (A VOP is not created if the reader
  conditional #+assembler precedes the definition of the assembly
  routine.)

  Name&Options
    A list giving the name of the assembly routine and options
    describing the assembly routine options and VOP options.  The
    format is (Name ({Key Value})*) where Name is the name of the
    assembly routine.  Options is a list of options:

    Options

      :Cost Cost
        The cost of the VOP.  This is used in the generated VOP.

      :Policy {:Small | :Fast | :Safe | :Fast-Safe}
        The policy for the VOP.

      :Translate Name
        The translation for the VOP.

      :Arg-Types arg-types
      :Result-Types result-types
        The template restrictions for the arguments of the VOP and the
        results of the VOP.

      :Return-Style {:Raw :Full-Call :None}
 
    Vars is a list of the arguments and returned results and
    temporaries used by the assembly routine.

      :Arg Arg-Name (SC*) SC-Offset
        Input argument for the assembly routine with the name
        Arg-Name.  The argument must be one of the Sc types.  The register
        assigned to this argument is given by SC-Offset which must be
        the offset for the register holding this argument.

      :Res Res-Name SC SC-Offset
        Result of the assembly routine with the name Res-Name.  The
        result must be a register of the specified storage class SC.  The
        Sc-offset is the register used for the result.

      :Temp Temp-Name SC SC-Offset
        Like :Res, except this names a temporary register that the
        assembly routine can use.

  Code
    The code for the assembly routine."
  (multiple-value-bind (name options)
		       (if (atom name&options)
			   (values name&options nil)
			   (values (car name&options)
				   (cdr name&options)))
    (let* ((regs (mapcar #'(lambda (var) (apply #'parse-reg-spec var)) vars)))
      (if *do-assembly*
	  (emit-assemble name options regs code)
	  (emit-vop name options regs)))))
