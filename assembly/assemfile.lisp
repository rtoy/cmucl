;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/assemfile.lisp,v 1.9 1990/04/27 19:21:01 wlott Exp $
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

(defun assemble-file (name &key (output-file
				 (make-pathname :defaults name
						:type "mips-fasl")))
  (let* ((*do-assembly* t)
	 (name (pathname name))
	 (*lap-output-file* (open-fasl-file (pathname output-file) name))
	 (*assembler-routines* nil)
	 (won nil))
    (unwind-protect
	(let (*code-segment* *elsewhere*)
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
	  (setq won t))
      (close-fasl-file *lap-output-file* (not won)))
    won))



(defstruct (reg-spec
	    (:print-function %print-reg-spec))
  (kind :temp :type '(member :arg :temp :result))
  (name nil :type 'symbol)
  (temp nil :type '(or null reg-spec))
  (sc nil :type '(or null sc))
  (offset nil :type '(or null fixnum))
  (type nil :type '(or null symbol))
  (from '(:eval 1))
  (to '(:eval 2))
  (keys :type 'list))

(defun %print-reg-spec (spec stream depth)
  (declare (ignore depth))
  (macrolet ((maybe (fn arg)
	       (let ((var (gensym)))
		 `(let ((,var ,arg))
		    (when ,var (,fn ,var))))))
    (format stream
	    "#<reg ~S ~S~@[[~S]~]~@[ sc=~S~]~@[ offset=~S~]~@[ type=~S~]>"
	    (reg-spec-kind spec)
	    (reg-spec-name spec)
	    (reg-spec-temp spec)
	    (maybe sc-name (reg-spec-sc spec))
	    (reg-spec-offset spec)
	    (reg-spec-type spec))))

(defun parse-reg-spec (kind name &rest keys &key sc offset type
			    (from '(:eval 1)) (to '(:eval 2))
			    &allow-other-keys)
  (make-reg-spec :kind kind
		 :name name
		 :sc (if sc (sc-or-lose sc))
		 :offset (eval offset)
		 :type type
		 :from from
		 :to to
		 :keys keys))


(defvar *allocated-regs* nil)
(defvar *available-storage-bases* nil)
(defvar *available-storage-classes* nil)

(defun allocate (reg offset)
  (let* ((sc (reg-spec-sc reg))
	 (sb (sc-sb sc))
	 (avail (find sb *available-storage-bases* :key #'car)))
    (unless (member offset avail)
      (let ((conflict (find-if #'(lambda (other)
				   (and (eq (sc-sb (reg-spec-sc other)) sb)
					(= (reg-spec-offset other) offset)))
			       *allocated-regs*)))
	(if conflict
	    (error "Attempt to allocate ~S on top of ~S." reg conflict)
	    (error "Attempted to allocate ~S in ~S at ~S, but ~:*~S is an ~
	            invalid offset."
		   reg sc offset))))
    ;; Note: we will never try to delete the first element 'cause it is
    ;; the storage base itself.
    (delete offset avail)
    (setf (reg-spec-offset reg) offset)
    (push reg *allocated-regs*)))


(defun make-arg-temp (name sc offset)
  (parse-reg-spec :temp (make-symbol (symbol-name name))
		  :sc sc
		  :offset offset
		  :from '(:eval 0)
		  :to '(:eval 2)))

(defun allocate-regs (regs)
  (let ((*allocated-regs* nil)
	(*available-storage-bases* nil)
	(*available-storage-classes* (list (sc-or-lose 'descriptor-reg))))
    ;; First, find all the storage classes that are refered to.
    (dolist (reg regs)
      (when (reg-spec-sc reg)
	(pushnew (reg-spec-sc reg) *available-storage-classes*)))
    ;; Second, find all the storage bases that are used by them and
    ;; record what locations are available.
    (dolist (sc *available-storage-classes*)
      (let ((sb (sc-sb sc)))
	(unless (find sb *available-storage-bases*
		      :key #'car)
	  (let ((list (list sb)))
	    (dotimes (i (sb-size sb))
	      (push i list))
	    (push (nreverse list)
		  *available-storage-bases*)))))
    ;; Third, allocate the wired regs.
    #+nil
    (dolist (reg regs)
      (when (reg-spec-offset reg)
	(allocate reg (reg-spec-offset reg))))
    ;; Fourth, make wired temp regs for the args and results.
    (let* ((arg-temps (mapcar #'(lambda (name offset)
				  (make-arg-temp name 'descriptor-reg offset))
			      register-arg-names register-arg-offsets))
	   (res-temps (copy-list arg-temps))
	   (arg-index 0)
	   (res-index 0))
      (dolist (reg regs)
	(macrolet ((next (list)
		     `(if (reg-spec-offset reg)
			  (make-arg-temp (reg-spec-name reg)
					 (sc-name (reg-spec-sc reg))
					 (reg-spec-offset reg))
			  (pop ,list))))
	  (case (reg-spec-kind reg)
	    (:arg
	     (let ((temp (next arg-temps)))
	       (setf (reg-spec-temp reg) temp)
	       (unless (member temp *allocated-regs*)
		 (allocate temp (reg-spec-offset temp)))
	       (setf (reg-spec-from temp)
		     `(:argument ,arg-index)))
	     (incf arg-index))
	    (:res
	     (let ((temp (next res-temps)))
	       (setf (reg-spec-temp reg) temp)
	       (unless (member temp *allocated-regs*)
		 (allocate temp (reg-spec-offset temp)))
	       (setf (reg-spec-to temp)
		     `(:result ,res-index)))
	     (incf res-index))
	    (:temp
	     (when (reg-spec-offset reg)
	       (allocate reg (reg-spec-offset reg))))))))
    ;; Fifth, remove regs we have already delt with.
    (setf regs
	  (remove-if #'(lambda (reg)
			 (case (reg-spec-kind reg)
			   ((:arg :res) t)
			   (t (reg-spec-offset reg))))
		     regs))
    ;; Sixth, order the remaining regs based on the number of possibilities.
    (assert (every #'reg-spec-sc regs) ()
	    "All :TEMP regs must have a SC speced: ~S." regs)
    (setf regs
	  (sort (copy-list regs) #'<
		:key #'(lambda (reg)
			 (length (sc-locations (reg-spec-sc reg))))))
    ;; Seventh, start picking.
    (dolist (reg regs)
      (let* ((sb (sc-sb (reg-spec-sc reg)))
	     (possible (intersection (find sb *available-storage-bases*
					   :key #'car)
				     (sc-locations (reg-spec-sc reg))))
	     (offset (car possible)))
	(unless possible
	  (error "Ran out of locations before allocating ~S" reg))
	(allocate reg offset)))))


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
		      (let ((temp (or (reg-spec-temp reg) reg)))
			`(,(reg-spec-name reg)
			  (make-random-tn
			   :sc (sc-or-lose ',(sc-name (reg-spec-sc temp)))
			   :offset ,(reg-spec-offset temp)))))
		  regs))
	   ,@insts
	   (inst addu lip-tn lra-tn (- vm:word-bytes vm:other-pointer-type))
	   (inst j lip-tn)
	   (inst nop)))
       (format t "~S assembled~%" ',name))))

(defun arg-or-res-spec (reg)
  `(,(reg-spec-name reg)
    :scs ,(if (reg-spec-sc reg)
	      (list (sc-name (reg-spec-sc reg)))
	      '(any-reg descriptor-reg))
    ,@(unless (eq (reg-spec-kind reg) :res)
	`(:target ,(reg-spec-name (reg-spec-temp reg))))
    ,@(do ((keys (reg-spec-keys reg) (cddr keys))
	   (results nil (let ((key (car keys)))
			  (if (member key '(:sc :offset))
			      results
			      (list* (cadr keys) key results)))))
	  ((null keys) (nreverse results)))))

(defun emit-vop (name vars)
  (let* ((args (remove :arg vars :key #'reg-spec-kind :test-not #'eq))
	 (orig-temps (remove :temp vars :key #'reg-spec-kind :test-not #'eq))
	 (temps orig-temps)
	 (results (remove :res vars :key #'reg-spec-kind :test-not #'eq))
	 (return-pc-label (make-symbol "RETURN-PC-LABEL"))
	 (return-pc (make-symbol "RETURN-PC"))
	 (lip (make-symbol "LIP"))
	 (ndescr (make-symbol "NDESCR")))
    (dolist (arg args)
      (pushnew (reg-spec-temp arg) temps))
    (dolist (res results)
      (pushnew (reg-spec-temp res) temps))
    `(define-vop (,name)
       (:args ,@(mapcar #'arg-or-res-spec args))
       ,@(mapcar #'(lambda (temp)
		     `(:temporary (:sc ,(sc-name (reg-spec-sc temp))
				   :offset ,(reg-spec-offset temp)
				   ,@(when (reg-spec-type temp)
				       `(:type
					 ,(reg-spec-type temp)))
				   ,@(when (reg-spec-from temp)
				       `(:from ,(reg-spec-from temp)))
				   ,@(when (reg-spec-to temp)
				       `(:to ,(reg-spec-to temp))))
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
       (:results ,@(mapcar #'arg-or-res-spec results))
       (:ignore ,lip ,@(mapcar #'reg-spec-name orig-temps))
       (:generator 247
	 ,@(mapcar #'(lambda (arg)
		       `(move ,(reg-spec-name (reg-spec-temp arg))
			      ,(reg-spec-name arg)))
		   args)
	 (let ((,return-pc-label (gen-label)))
	   (inst compute-lra-from-code ,return-pc code-tn ,return-pc-label)
	   (inst li ,ndescr (make-fixup ',name :assembly-routine))
	   (inst j ,ndescr)
	   (inst nop)
	   (emit-return-pc ,return-pc-label))

	 ,@(mapcar #'(lambda (res)
		       `(move ,(reg-spec-name res)
			      ,(reg-spec-name (reg-spec-temp res))))
		   results)))))

(defmacro define-assembly-routine ((name &rest vars) &rest code)
  (let* ((regs (mapcar #'(lambda (var) (apply #'parse-reg-spec var)) vars)))
    (allocate-regs regs)
    (if *do-assembly*
	(emit-assemble name regs code)
	(emit-vop name regs))))
