;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/x86/Attic/kludges.lisp,v 1.1 1997/01/21 00:30:28 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Kludges to make up for the fact that we don't have an assembler.  
;;; replace this file with real native assembly source later.
;;;
;;; by jrd
;;;

(in-package :x86)


#+assembler
;;;
;;; a kludge to tranpoline through when we're calling into lisp.
;;; this cf the sparc one
;;;
;;; int call_into_lisp_kludge(control_stack_pointer, function, args, nargs);
;;;
(define-assembly-routine (call-into-lisp-kludge)
			 ((:temp eax dword-reg eax-offset)
			  (:temp ebx dword-reg ebx-offset)
			  (:temp ecx dword-reg ecx-offset)
			  (:temp edx dword-reg edx-offset)
			  (:temp esi dword-reg esi-offset)
			  (:temp edi dword-reg edi-offset)
			  (:temp esp dword-reg esp-offset)
			  (:temp ebp dword-reg ebp-offset))
  ;; get the lisp stack pointer into ebx, so we can push things on it
  (inst mov ebp (make-ea :dword :base esp :disp 4))

  ;; make room on the lisp stack
  (inst sub ebp 8)

  ;; pull the args off the stack
;  (inst mov eax (make-ea :dword :base esp :disp 16))	;nargs.  zzz is this right?
;  (inst mov (make-ea :dword :base ebp :disp 4) eax)
;  (inst mov eax (make-ea :dword :base esp :disp 12))	; args
;  (inst mov (make-ea :dword :base ebp  :disp 0) eax)

  ;; get the function 
  (inst mov eax (make-ea :dword :base esp :disp 8))
  (inst add eax 23)				; function-code-offset
  (inst mov edi eax)				; move it to reg-code

  ;; clear the descriptor regs
  (inst mov eax 0)
  (inst mov ebx 0)
  (inst mov ecx 0)
  (inst mov edx 0)

  ;; zzz what else?

  ;; jump there
  (inst call edi))

