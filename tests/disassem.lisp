;; Test disassembler

(defpackage :disassem-tests
  (:use :cl :lisp-unit))

(in-package :disassem-tests)

#+x86
(define-test issue.187.imul-imm8-disassembly
  (:tag :issues)
  ;; (* x -7) compiles to "imul reg, r/m, imm8".  The prefilter
  ;; function for that printer used to be shared with the one built
  ;; for the arith instructions' imm8 form, which keeps its immediate
  ;; in a different filtered-value slot.  The immediate was read but
  ;; never stored where the printer looked for it, so it printed as 0.
  (let ((text (with-output-to-string (s)
		(disassem:disassemble
		 (compile nil
			  `(lambda (x)
			     (declare (type (signed-byte 16) x))
			     (* x -7)))))))
    (assert-true (search ", #x-7" text) text)))
