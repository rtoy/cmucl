;;; xref-macro-caller-fixture.lisp -- fixture for the
;;; xref.macro-caller test.  Compiling this file should record a
;;; WHO-CALLS entry for HELPER whose caller context is (:MACRO MAC-A),
;;; because HELPER is invoked from inside MAC-A's expander body.

(defpackage :xref-macro-caller-fixture
  (:use :cl))

(in-package :xref-macro-caller-fixture)

;; HELPER must be defined in the compiling Lisp's image at the time
;; COMPILE-FILE reaches the use of MAC-A below, because macroexpanding
;; (MAC-A ...) runs MAC-A's body, which calls HELPER.  EVAL-WHEN gets
;; HELPER defined at compile-time as well as load-time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun helper ()
    'hello))

;; MAC-A's expander body calls HELPER.  That is the call whose xref
;; context we want to inspect.
(defmacro mac-a (&body body)
  (helper)
  `(progn ,@body))

;; A use of MAC-A so the compiler actually processes its expander.
;; This use itself is irrelevant to the test.
(defun use-site ()
  (mac-a 1 2 3))
