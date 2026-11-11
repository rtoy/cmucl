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
			     (* x -7)))
		 :stream s))))
    (assert-true (search ", #x-7" text) text)))

#+x86
(define-test issue.187.prefilter-slot-sharing
  (:tag :issues)
  ;; A cached prefilter has to fill exactly the slots its printer
  ;; reads.  BIT-TEST-REG/MEM, EXT-REG-REG/MEM-SHIFT, COND-MOVE and
  ;; COND-SET each have one prefiltered arg named REG/MEM, with the
  ;; same fields and the same prefilter, but the first two keep it in
  ;; slot 2 and the last two in slot 3.  All four used to share one
  ;; function, so CMOV and SET read a slot nothing had written; it
  ;; held 0, and 0 is a valid register encoding, so they printed EAX
  ;; and AL whatever the ModRM byte said.
  (flet ((dis (&rest octets)
	   ;; None of these formats has a WIDTH field, so their
	   ;; register operands print at whatever width the preceding
	   ;; instruction left in the dstate.  Prime it with "mov eax,
	   ;; eax" (8b c0) to establish :dword, then drop that line and
	   ;; return just the instruction under test.
	   (let* ((octets (list* #x8b #xc0 octets))
		  (v (make-array (length octets)
				 :element-type '(unsigned-byte 8)
				 :initial-contents octets))
		  (text (sys:without-gcing
			 (with-output-to-string (s)
			   (disassem:disassemble-memory
			    (sys:vector-sap v) (length v) :stream s))))
		  (nl (position #\Newline text)))
	     (assert-true nl text)
	     (assert-true (search "mov" text :end2 nl) text)
	     (assert-true (search "eax, eax" text :end2 nl) text)
	     (subseq text (1+ nl)))))
    ;; 0f a3 d8 -- bt eax, ebx.  Owns the shared prefilter; always worked.
    (assert-true (search "eax, ebx" (dis #x0f #xa3 #xd8)))
    ;; 0f 44 d3 -- cmove edx, ebx.  Used to print "cmove edx, eax".
    (assert-true (search "edx, ebx" (dis #x0f #x44 #xd3)))
    ;; 0f 95 c3 -- setne bl.  Used to print "setne al".
    (let ((text (dis #x0f #x95 #xc3)))
      (assert-true (search "setne" text) text)
      (assert-true (search "bl" text) text))))

