;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/struct.lisp,v 1.16 1993/04/04 10:00:34 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains structure definitions that need to be compiled early
;;; for bootstrapping reasons.
;;;
(in-package "LISP")

;;;; The stream structure:

(defconstant in-buffer-length 512 "The size of a stream in-buffer.")
(deftype in-buffer-type ()
  `(simple-array (unsigned-byte 8) (,in-buffer-length)))

;;; Change the kind of stream to :instance so that the defstruct doesn't flame
;;; out.
;;; 
(eval-when (compile eval)
  (setf (info type kind 'stream) :instance))

(defstruct (stream (:predicate streamp) (:print-function %print-stream))
  ;;
  ;; Buffered input.
  (in-buffer nil :type (or in-buffer-type null))
  (in-index in-buffer-length :type index)	; Index into in-buffer
  (in #'ill-in :type function)			; Read-Char function
  (bin #'ill-bin :type function)		; Byte input function
  (n-bin #'ill-bin :type function)		; N-Byte input function
  (out #'ill-out :type function)		; Write-Char function
  (bout #'ill-bout :type function)		; Byte output function
  (sout #'ill-out :type function)		; String output function
  (misc #'do-nothing :type function))		; Less used methods


;;; Condition structures:

(in-package "CONDITIONS")

(defstruct (condition (:constructor |constructor for condition|)
                      (:predicate nil)
                      (:print-function condition-print))
  )
