;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/macros.lisp,v 1.2 1990/02/03 19:33:02 wlott Exp $
;;;
;;;    This file contains various useful macros for generating MIPS code.
;;;
;;; Written by William Lott.
;;; 

(in-package "C")



(defmacro nop ()
  "Emit a nop."
  '(inst or zero-tn zero-tn zero-tn))


(defmacro move (dst src)
  "Move SRC into DST (unless they are already the same)."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst or ,n-dst ,n-src zero-tn))))


(defmacro def-mem-op (op inst shift load)
  `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
     `(progn
	(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag))
	,,@(when load
	     '('(nop))))))


(def-mem-op loadw lw word-shift t)
(def-mem-op storew sw word-shift nil)



(defmacro loadi (reg const)
  (once-only ((n-reg reg)
	      (n-const const))
    `(cond ((<= #x-8000 ,n-const #x7fff)
	    (inst addi ,n-reg zero-tn ,n-const))
	   ((<= #x8000 ,n-const #xffff)
	    (inst ori ,n-reg zero-tn ,n-const))
	   ((<= #x-80000000 ,n-const #xffffffff)
	    (inst lui ,n-reg (ldb (byte 16 16) ,n-const))
	    (let ((low (ldb (byte 16 0) ,n-const)))
	      (unless (zerop low)
		(inst ori ,n-reg ,n-reg low))))
	   (t
	    (error "Constant ~D cannot be loaded." ,n-const)))))


;;; Load-Stack-TN, Store-Stack-TN  --  Interface
;;;
;;;    Move a stack TN to a register and vice-versa.
;;;
(defmacro load-stack-tn (reg stack)
  (once-only ((n-reg reg)
	      (n-stack stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg string-char-reg sap-reg)
	(sc-case ,n-stack
	  ((control-stack number-stack string-char-stack sap-stack)
	   (loadw ,n-reg cont-tn (tn-offset ,n-stack))))))))


(defmacro store-stack-tn (stack reg)
  (once-only ((n-stack stack)
	      (n-reg reg))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg string-char-reg sap-reg)
	(sc-case ,n-stack
	  ((control-stack number-stack string-char-stack sap-stack)
	   (storew ,n-reg cont-tn (tn-offset ,n-stack))))))))

;;; Error stuff.
;;;

(defmacro error-call (error-code &rest values)
  (once-only ((n-error-code error-code))
    `(progn
       ,@(collect ((args))
	   (do ((vals values (cdr vals))
		(regs register-argument-tns (cdr regs)))
	       ((or (null vals) (null regs))
		(if (null vals)
		    (args)
		    (error "Can't use ERROR-CALL with ~D values"
			   (length values))))
	     (args `(move ,(car regs) ,(car vals)))))
       (inst break ,error-code))))

(defmacro generate-error-code (node error-code &rest values)
  "Generate-Error-Code Node Error-code Value*
  Emit code for an error with the specified Error-Code and context Values.
  Node is used for source context."
  `(unassemble
     (assemble-elsewhere ,node
       (let ((start-lab (gen-label)))
	 (emit-label start-lab)
	 (error-call ,error-code ,@values)
	 start-lab))))
