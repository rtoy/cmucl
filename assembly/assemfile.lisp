;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/assemfile.lisp,v 1.20 1990/10/28 05:59:54 wlott Exp $
;;;
;;; This file contains the extra code necessary to feed an entire file of
;;; assembly code to the assembler.
;;;
(in-package "C")


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
	 (won nil))
    (unwind-protect
	(let (*code-segment*
	      *elsewhere*
	      #-new-compiler (lisp::*in-compilation-unit* nil))
	  (pushnew :assembler *features*)
	  (init-assembler)
	  (load (merge-pathnames name (make-pathname :type "lisp")))
	  (fasl-dump-cold-load-form `(in-package ,(package-name *package*))
				    *lap-output-file*)
	  (assemble (*code-segment* nil)
	    (insert-segment *elsewhere*))
	  (let ((length (finalize-segment *code-segment*)))
	    (dump-assembler-routines *code-segment*
				     length
				     *assembler-routines*
				     *lap-output-file*))
	  (when trace-file
	    (with-open-file (file (if (eq trace-file t)
				      (make-pathname :defaults name
						     :type "trace")
				      trace-file)
				  :direction :output
				  :if-exists :supersede)
	      (format file "Assembly listing for ~A:~3%" (namestring name))
	      (dump-segment *code-segment* file)
	      (fresh-line file)))
	  (setq won t))
      (deletef :assembler *features*)
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
  (let* ((labels nil)
	 (insts (mapcar #'(lambda (inst)
			    (cond ((symbolp inst)
				   (push inst labels)
				   `(emit-label ,inst))
				  (t
				   inst)))
			code))
	 (return-style (or (cadr (assoc :return-style options)) :raw)))
    `(let ((,name (gen-label))
	   ,@(mapcar #'(lambda (label)
			 `(,label (gen-label)))
		     labels))
       (push (cons ',name ,name) *assembler-routines*)
       (assemble (*code-segment* ',name)
	 (emit-label ,name)
	 (let (,@(mapcar
		  #'(lambda (reg)
		      `(,(reg-spec-name reg)
			(make-random-tn
			 :kind :normal
			 :sc (sc-or-lose ',(reg-spec-sc reg))
			 :offset ,(reg-spec-offset reg))))
		  regs))
	   ,@insts
	   ,@(ecase vm:target-fasl-file-implementation
	       (#.vm:pmax-fasl-file-implementation
		(ecase return-style
		  (:raw
		   `((inst j lip-tn)
		     (inst nop)))
		  (:full-call
		   `((lisp-return (make-random-tn :kind :normal
						  :sc (sc-or-lose
						       'descriptor-reg)
						  :offset lra-offset)
				  lip-tn :offset 2)))
		  (:none))))))
       (format *error-output* "~S assembled~%" ',name))))

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
	 (lip (make-symbol "LIP"))
	 (lra (make-symbol "LRA"))
	 (temp (make-symbol "TEMP"))
	 (vop (make-symbol "VOP"))
	 (nfp-save (make-symbol "NFP-SAVE")))
    (unless (member return-style '(:raw :full-call :none))
      (error "Unknown return-style for ~S: ~S" name return-style))
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
       (:temporary (:scs (interior-reg) :type interior
			 :from (:eval 0) :to (:eval 1))
		   ,lip)
       ,@(when (eq return-style :full-call)
	   `((:temporary (:sc descriptor-reg :offset lra-offset
			      :from (:eval 0) :to (:eval 1))
			 ,lra)
	     (:temporary (:scs (non-descriptor-reg)
			       :from (:eval 0) :to (:eval 1))
			 ,temp)
	     (:temporary (:sc control-stack :offset nfp-save-offset) ,nfp-save)
	     (:vop-var ,vop)))
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
       (:ignore ,lip ,@(mapcar #'reg-spec-name temps))
       ,@(remove-if #'(lambda (x)
			(member x '(:return-style :cost)))
		    options
		    :key #'car)
       (:generator ,cost
	 ,@(mapcar #'(lambda (arg)
		       `(move ,(reg-spec-temp arg)
			      ,(reg-spec-name arg)))
		   args)
	 ,@(ecase vm:target-fasl-file-implementation
	     (#.vm:pmax-fasl-file-implementation
	      (ecase return-style
		(:raw
		 `((inst jal (make-fixup ',name :assembly-routine))
		   (inst nop)))
		(:full-call
		 (when (> (length results) 1)
		   (error "Can't use :full-call in an assembly-routine for ~
		           more than one return value."))
		 `((let ((lra-label (gen-label))
			 (cur-nfp (current-nfp-tn ,vop)))
		     (when cur-nfp
		       (store-stack-tn ,nfp-save cur-nfp))
		     (inst compute-lra-from-code ,lra code-tn lra-label ,temp)
		     (inst j (make-fixup ',name :assembly-routine))
		     (inst nop)
		     (emit-return-pc lra-label)
		     (note-this-location ,vop :unknown-return)
		     (move csp-tn old-fp-tn)
		     (inst nop)
		     (inst compute-code-from-lra code-tn code-tn
			   lra-label ,temp)
		     (when cur-nfp
		       (load-stack-tn cur-nfp ,nfp-save)))))
		(:none
		 `((inst j (make-fixup ',name :assembly-routine))
		   (inst nop))))))
	 ,@(mapcar #'(lambda (res)
		       `(move ,(reg-spec-name res)
			      ,(reg-spec-temp res)))
		   results)))))

(defmacro define-assembly-routine (name&options vars &rest code)
  (multiple-value-bind (name options)
		       (if (atom name&options)
			   (values name&options nil)
			   (values (car name&options)
				   (cdr name&options)))
    (let* ((regs (mapcar #'(lambda (var) (apply #'parse-reg-spec var)) vars)))
      (if *do-assembly*
	  (emit-assemble name options regs code)
	  (emit-vop name options regs)))))
