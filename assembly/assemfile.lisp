;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/assemfile.lisp,v 1.28 1992/05/22 15:33:27 wlott Exp $
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
	 (*fixups* nil))
    (unwind-protect
	(progn
	  (pushnew :assembler *features*)
	  (init-assembler)
	  (load (merge-pathnames name (make-pathname :type "lisp")))
	  (fasl-dump-cold-load-form `(in-package ,(package-name *package*))
				    *lap-output-file*)
	  (let ((length
		 (cond ((target-featurep :new-assembler)
			(new-assem:append-segment *code-segment* *elsewhere*)
			(setf *elsewhere* nil)
			(new-assem:finalize-segment *code-segment*))
		       (t
			(assem:assemble (*code-segment* nil)
			  (assem:insert-segment *elsewhere*))
			(assem:expand-pseudo-instructions *code-segment*)
			(assem:finalize-segment *code-segment*)))))
	    (dump-assembler-routines *code-segment*
				     length
				     *assembler-routines*
				     *fixups*
				     *lap-output-file*))
	  (when (and trace-file (not (target-featurep :new-assembler)))
	    (with-open-file (file (if (eq trace-file t)
				      (make-pathname :defaults name
						     :type "trace")
				      trace-file)
				  :direction :output
				  :if-exists :supersede)
	      (format file "Assembly listing for ~A:~3%" (namestring name))
	      (assem:dump-segment *code-segment* file)
	      (fresh-line file)))
	  (setq won t))
      (deletef :assembler *features*)
      (when (target-featurep :new-assembler)
	(new-assem:release-segment *code-segment*)
	(when *elsewhere*
	  (new-assem:release-segment *elsewhere*)))
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
  (if (backend-featurep :new-assembler)
      `(let (,@(mapcar
		#'(lambda (reg)
		    `(,(reg-spec-name reg)
		      (make-random-tn
		       :kind :normal
		       :sc (sc-or-lose ',(reg-spec-sc reg))
		       :offset ,(reg-spec-offset reg))))
		regs))
	 (new-assem:assemble (*code-segment* ',name)
	   ,name
	   (push (cons ',name ,name) *assembler-routines*)
	   ,@code
	   ,@(generate-return-sequence
	      (or (cadr (assoc :return-style options)) :raw)))
	 (when *compile-print*
	   (format *error-output* "~S assembled~%" ',name)))
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
	   (assem:assemble (*code-segment* ',name)
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
	       ,@(generate-return-sequence return-style)))
	   (when *compile-print*
	     (format *error-output* "~S assembled~%" ',name))))))

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
	 (:ignore ,@(mapcar #'reg-spec-name temps))
	 ,@(remove-if #'(lambda (x)
			  (member x '(:return-style :cost)))
		      options
		      :key #'car)
	 (:generator ,cost
	   ,@(mapcar #'(lambda (arg)
			 `(move ,(reg-spec-temp arg)
				,(reg-spec-name arg)))
		     args)
	   ,@call-sequence
	   ,@(mapcar #'(lambda (res)
			 `(move ,(reg-spec-name res)
				,(reg-spec-temp res)))
		     results))))))

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
