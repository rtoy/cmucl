;
;************************************************************************
;
;	VPS2 -- Interpreter for OPS5
;
;
;
; This Common Lisp version of OPS5 is in the public domain.  It is based
; in part on based on a Franz Lisp implementation done by Charles L. Forgy
; at Carnegie-Mellon University, which was placed in the public domain by
; the author in accordance with CMU policies.  This version has been
; modified by George Wood, Dario Giuse, Skef Wholey, Michael Parzen,
; and Dan Kuokka.
;
; This code is made available is, and without warranty of any kind by the
; authors or by Carnegie-Mellon University.
;

;;;; This file handles the loading of all files composing the OPS interpreter.
;;;; It also performs the necessary initialization.


(setf (search-list "ops:") '("/afs/cs/project/clisp/library/ops/"))

(in-package "OPS")

(defun ops-init ()
  ; Allows ^ , { , and } operators to be right next to another symbol.
  (set-macro-character #\{ #'(lambda (s c)
			       (declare (ignore s c))
			       '\{))
  (set-macro-character #\} #'(lambda (s c)
			       (declare (ignore s c))
			       '\}))
  (set-macro-character #\^ #'(lambda (s c)
			       (declare (ignore s c))
			       '\^))
  (backup-init)
  (compile-init)
  (main-init)
  (match-init)
  (io-init)
  (rhs-init))

(load "ops:ops-util")
(load "ops:ops-backup")
(load "ops:ops-compile")
(load "ops:ops-main")
(load "ops:ops-match")
(load "ops:ops-io")
(load "ops:ops-rhs")
(ops-init)
