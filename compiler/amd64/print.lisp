;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/amd64/print.lisp,v 1.1 2004/05/24 22:35:00 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the print VOP, which is used while booting the kernel
;;; core to keep the user entertained.
;;;
;;; Written by William Lott.
;;; Enhancements/debugging by Douglas T. Crosher 1996.
;;;
(in-package :amd64)

(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg :offset rax-offset :target result
		   :from :eval :to :result) rax)
  (:temporary (:sc unsigned-reg :offset rdi-offset) rdi) ; from/to?
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:generator 100
    (move rdi object) ;; C arg 1
    ;; LEA can't be used because it can only do sign-extended 32 bit argument.
    ;; We need to specify immediate here because we don't want to be indirected
    (inst mov-imm rax (make-fixup (extern-alien-name "debug_print") :foreign))
    (inst call (make-fixup (extern-alien-name "call_into_c") :foreign))
    (move result rax)))
