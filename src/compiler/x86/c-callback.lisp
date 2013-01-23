;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: src/compiler/x86/c-callback.lisp $")

(in-package :x86)
(use-package :alien)
(use-package :alien-internals)
(intl:textdomain "cmucl-x86-vm")

;;; Support for callbacks to Lisp.
(export '(make-callback-trampoline callback-accessor-form
	  compatible-function-types-p))

(defun callback-accessor-form (type sp offset)
  `(alien:deref (sap-alien 
		 (sys:sap+ ,sp ,offset)
		 (* ,type))))

(defun compatible-function-types-p (type1 type2)
  (flet ((machine-rep (type)
	   (etypecase type
	     (alien::integer-64$ :dword)
	     ((or alien::integer$ alien::pointer$ alien::sap$) :word)
	     (alien::single$ :single)
	     (alien::double$ :double)
	     (alien::void$ :void))))
    (let ((type1 (alien-function-type-result-type type1))
	  (type2 (alien-function-type-result-type type2)))
      (eq (machine-rep type1) (machine-rep type2)))))

(defun make-callback-trampoline (index fn-type)
  "Cons up a piece of code which calls call-callback with INDEX and a
pointer to the arguments."
  (let* ((return-type (alien-function-type-result-type fn-type))
	 (segment (make-segment))
	 (eax x86::eax-tn)
	 (edx x86::edx-tn)
	 (ebp x86::ebp-tn)
	 (esp x86::esp-tn)
	 ([ebp-8] (x86::make-ea :dword :base ebp :disp -8))
	 ([ebp-4] (x86::make-ea :dword :base ebp :disp -4)))
    (assemble (segment)
	      (inst push ebp)			    ; save old frame pointer
	      (inst mov  ebp esp)		    ; establish new frame
	      (inst mov  eax esp)		    ; 
	      (inst sub  eax 8)		            ; place for result 
	      (inst push eax)			    ; arg2
	      (inst add  eax 16)		    ; arguments  
	      (inst push eax)			    ; arg1
	      (inst push (ash index 2))		    ; arg0
	      (inst push (alien::address-of-call-callback))     ; function
	      (inst mov  eax (alien::address-of-funcall3))
	      (inst call eax)
	      ;; now put the result into the right register
	      (etypecase return-type
		(alien::integer-64$
		 (inst mov eax [ebp-8])
		 (inst mov edx [ebp-4]))
		((or alien::integer$ alien::pointer$ alien::sap$)
		 (inst mov eax [ebp-8]))
		(alien::single$
		 (inst fld  [ebp-8]))
		(alien::double$
		 (inst fldd [ebp-8]))
		(alien::void$ ))
	      (inst mov esp ebp)		   ; discard frame
	      (inst pop ebp)			   ; restore frame pointer
	      (inst ret))
    (let* ((length (finalize-segment segment)))
      (prog1 (alien::segment-to-trampoline segment length)
	(release-segment segment)))))


