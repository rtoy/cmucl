;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/assemfile.lisp,v 1.18 1990/10/20 01:36:42 wlott Exp $
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
					   :type "mips-fasl"))
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


(defun emit-assemble (name regs code)
  (let* ((labels nil)
	 (insts (mapcar #'(lambda (inst)
			    (cond ((symbolp inst)
				   (push inst labels)
				   `(emit-label ,inst))
				  (t
				   inst)))
			code)))
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
	   (inst addu lip-tn lra-tn (- vm:word-bytes vm:other-pointer-type))
	   (inst j lip-tn)
	   (inst nop)))
       (format t "~S assembled~%" ',name))))

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
	 (return-pc-label (make-symbol "RETURN-PC-LABEL"))
	 (return-pc (make-symbol "RETURN-PC"))
	 (lip (make-symbol "LIP"))
	 (ndescr (make-symbol "NDESCR")))
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
       (:temporary (:sc any-reg :offset lra-offset
			:from (:eval 0) :to (:eval 1))
		   ,return-pc)
       (:temporary (:scs (non-descriptor-reg) :type random
			 :from (:eval 0) :to (:eval 1))
		   ,ndescr)
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
       ,@options
       (:generator 247
	 (let ((,return-pc-label (gen-label)))
	   ,@(mapcar #'(lambda (arg)
			 `(move ,(reg-spec-temp arg)
				,(reg-spec-name arg)))
		     args)
	   (inst compute-lra-from-code
		 ,return-pc code-tn ,return-pc-label ,ndescr)
	   (inst j (make-fixup ',name :assembly-routine))
	   (inst nop)
	   (emit-return-pc ,return-pc-label)
	   
	   ,@(mapcar #'(lambda (res)
			 `(move ,(reg-spec-name res)
				,(reg-spec-temp res)))
		     results))))))

(defmacro define-assembly-routine ((name options &rest vars) &rest code)
  (let* ((regs (mapcar #'(lambda (var) (apply #'parse-reg-spec var)) vars)))
    (if *do-assembly*
	(emit-assemble (if (atom name) name (car name)) regs code)
	(emit-vop name options regs))))
