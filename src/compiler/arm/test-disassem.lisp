;; Simple tests for disassembly of instructions.  The inst-space is
;; dumped to /tmp/disassem.  You can then enter various calls to
;; TEST-PATTERN to test the disassembly on certain patterns of
;; instructions.

;; Currently, this file can be loaded during cross-compile, after
;; insts.lisp has been compiled.  If this is done from the debugger,
;; enter FLUSH so that errors in the debugger are not flushed.
;;
;; For this to work, we need to load insts.lisp to define the
;; instructions, if we haven't loaded insts.lisp yet.  This might
;; cause an error about a structure being redefined.  Select the
;; clobber-it restart.
;;(load "target:compiler/arm/insts")

(defvar *mem* (make-array 1 :element-type '(unsigned-byte 32)))

(defun print-bits (word)
  (format t "~8,'0b|" (ldb (byte 8 24) word))
  (format t "~8,'0b|" (ldb (byte 8 16) word))
  (format t "~8,'0b|" (ldb (byte 8 8) word))
  (format t "~8,'0b" (ldb (byte 8 0) word)))
  

;; Main test routine.  NAME is just a name for the test.  PATTERN is a
;; string containing the pattern of bits for the instruction that we
;; want to test.  It must consist of a a sequence of 0's, 1's, and
;; x's.  All other characters are ignored.  The 0's and 1's indicate
;; that that bit of the instruction has that value.  An x will be
;; randomly replaced by a zero or one.
;;
;; However, the condition field for the instruction MUST not be
;; included in the pattern.
;;
;; The output will be the instruction bits followed by the disassembly
;; of that instruction.  All x's in the pattern will be replaced with
;; all possible values and the corresponding instruction is
;; disassembled.
(defun test-pattern (name pattern)
  (let* ((xcount (count #\x pattern))
	 (len (+ xcount
		 (count #\0 pattern)
		 (count #\1 pattern))))
    (assert (<= len 28))
    (dotimes (n (ash 1 xcount))
      (dolist (c '(14 1))
	(let ((word (ash c 28))
	      (bit-posn 0)
	      (posn (1- len)))
	  (declare (type (unsigned-byte 32) word)
		   (type (integer -1 32) bit-posn posn))
	  (map nil
	       #'(lambda (c)
		   (cond ((char-equal c #\0)
			  (setf word (dpb 0 (byte 1 posn) word)))
			 ((char-equal c #\1)
			  (setf word (dpb 1 (byte 1 posn) word)))
			 ((char-equal c #\x)
			  (setf word (dpb (ldb (byte 1 bit-posn) n)
					  (byte 1 posn)
					  word))
			  (incf bit-posn))
			 (t (incf posn)))
		   (decf posn))
	       pattern)
	  (setf (aref *mem* 0) word)
	  (sys:without-gcing
	      (format t "~15A: " name)
	    (print-bits word)
	    (format t ": ")
	    (let ((dis
		    (with-output-to-string (s)
		      (let ((*standard-output* s)
			    (vm::*use-lisp-register-names* nil)
			    (*print-case* :downcase))
			(disassem:disassemble-memory (sys:vector-sap *mem*)
						     4
						     :backend c::*target-backend*)))))
	      ;; Remove the address
	      (format t "~A" (subseq dis 15)))))))))

;; Save the inst space
(with-open-file (s "/tmp/disassem" :direction :output :if-exists :supersede)
  (let ((*standard-output* s))
    (disassem::print-backend-inst-space)))

;; Reads the inst space file produced above and generates a
;; TEST-PATTERN for each of the instructions listed in the inst space.

(defun snarf-disassem (path)
  (with-open-file (s path :direction :input :external-format :latin1)
    (let (line)
      (loop while (setf line (read-line s nil nil)) do
	(setf line (string-left-trim '(#\space) line))
	(when (member (aref line 0) '(#\[ #\:))
	  (let ((name-end (position #\space line))
		(pattern-begin (position #\space line :from-end t)))
	    (format t "(test-pattern ~S ~S)~%"
		    (subseq line 1 name-end)
		    (subseq line (+ 1 4 pattern-begin) (1- (length line))))))))))

(in-package "VM")

;; A set of random insts to test that the assembler can assembly the
;; given instructions.
(defun test-assem ()
  (let ((segment (make-segment))
	(n (c:make-random-tn :kind :normal
			     :sc (c:sc-or-lose 'vm::descriptor-reg)
			     :offset vm::null-offset))
	(na (c:make-random-tn :kind :normal
			      :sc (c:sc-or-lose 'vm::descriptor-reg)
			      :offset vm::nargs-offset))
	(sp (c:make-random-tn :kind :normal
			      :sc (c:sc-or-lose 'vm::any-reg)
			      :offset csp-offset))
	(fd-0 (c:make-random-tn :kind :normal
				:sc (c:sc-or-lose 'vm::double-reg)
				:offset 0))
	(fd-1 (c:make-random-tn :kind :normal
				:sc (c:sc-or-lose 'vm::double-reg)
				:offset 2)))
    (assemble (segment)
      (inst mov n na)
      (inst add n na (make-shift na :lsl 2))
      (inst vadd fd-0 fd-0 fd-1)
      ;; Tests for issue #21
      (inst ldrh na n 4)		; ldrh na, [n, #4]
      (inst ldrh na n -8)		; ldrh na, [n, #-8]
      (inst ldrh na (pre-index n) 7)	; ldrh na, [n, #7]!
      (inst ldrh na (pre-index n) -7)	; ldrh na, [n, #-7]!
      (inst ldrh na (post-index n) 7)	; ldrh na, [n], #7
      (inst ldrh na (post-index n) -7)	; ldrh na, [n], #7
      (inst ldrh na n n)		; ldrh na, [n, n]
      (inst ldrh na n (make-op2 n))	; ldrh na, [n, n]
      (inst ldrh na (pre-index n) n)	; ldrh na, [n, n]!
      (inst ldrh na (pre-index n) (make-op2 n :add nil))   ; ldrh na, [n, -n]!
      (inst ldrh na n (make-op2 n :add nil))               ; ldrh na, [n, n]
      (inst ldrh na (post-index n) (make-op2 n))           ; ldrh na, [n], n
      (inst ldrh na (post-index n) (make-op2 n :add nil))  ; ldrh na, [n], -n
      (inst ldm sp (list na n))		; ldm sp, {na, n}
      (inst ldm sp (list na n) :eq)		; ldmeq sp, {na, n}
      (inst ldm (pre-index sp) (list na n))	; ldm sp!, {na, n}
      (inst stmdb sp (list na n))		; stmfd sp, {na, n}
      (inst stmdb (pre-index sp) (list na n) :eq)  ; stmfdeq sp!, {na, n}
      (inst word #x4152462d) 		; cmp nl2, sp lsr #12
      )
    segment))

;; Disassemble the result of TEST-ASSEM.  Intended to verify that we
;; can disassemble the instructions in TEST-ASSEM and that the
;; disassemnbly has the expected from.
(defun disassem-test-assem ()
  ;; Hack. Don't know why disassem::disassemble-assem-segment won't
  ;; disassemble the result of test-assem.  Hence we do it here
  ;; ourselves by looking at the block of memory into which the
  ;; instructions were assembled.  This is fragile, but good enough
  ;; for simple testing for now.  Currently assumes everything is in
  ;; the first element returned by NEW-ASSEM::SEGMENT-OUTPUT-BLOCKS.
  (let* ((seg (test-assem))
	 (start (new-assem::segment-output-blocks seg)))
    (disassem::disassemble-memory (aref start 0)
				  (new-assem::segment-current-index seg)
				  :backend c::*target-backend*)))

(in-package "CL-USER")
#||
;; These patterns can be obtained from
;; (disassem::print-backend-inst-space)
;;
;; [MOV(FORMAT-1-IMMED)                    xxxx0011|101x0000|xxxxxxxx|xxxxxxxx]
;;
;; Since the last 12 bits are the immediate shift type and amount,
;; it's not really interesting to test all of those.  Just test a few
;; of them.

;; [MOV(FORMAT-1-IMMED)                    xxxx0011|10100000|xxxxxxxx|xxxxxxxx]
;; [MOVS(FORMAT-1-IMMED)                   xxxx0011|10110000|xxxxxxxx|xxxxxxxx]
;; MOV[s][ne]  [r8,r9], [#129, #1073741856]
(test-pattern "MOV(FORMAT-1-IMMED)" "0011|101x0000|100x000x|10000001")

;; [MVN(FORMAT-1-IMMED)                    xxxx0011|111x0000|xxxxxxxx|xxxxxxxx
;; [MVNS(FORMAT-1-IMMED)                   xxxx0011|11110000|xxxxxxxx|xxxxxxxx]
;; MVN[s][ne]  [r0, r4], [#129, #1073741856]
(test-pattern "MVN(FORMAT-1-IMMED)" "0011|111x0000|0x00000x|10000001")

;; [BIC(FORMAT-1-IMMED)                    xxxx0011|110xxxxx|xxxxxxxx|xxxxxxxx]
;; [BICS(FORMAT-1-IMMED)                   xxxx0011|1101xxxx|xxxxxxxx|xxxxxxxx]
;; BIC[s][ne] [r0,r1], [r2,r3], [#129, #8454144]
(test-pattern "BIC(FORMAT-1-IMMED)" "0011|110x001x|000xx000|10000001")

;; [BIC(FORMAT-0-REG)                  xxxx0001|110xxxxx|xxxxxxxx|xxx0xxxx]
;; [BICS(FORMAT-0-REG)                 xxxx0001|1101xxxx|xxxxxxxx|xxx0xxxx]
;;BIC[s][ne] [r0,r1], [r2,r3], [csp, cfp] ["", lsl, asr, lsr, ror] [#1]
(test-pattern "BIC(FORMAT-0-REG)" "0001|110x001x|000x0000|xxx0101x")

;; [ADC(FORMAT-0-REG-SHIFTED)      xxxx0000|101xxxxx|xxxxxxxx|0xx1xxxx]
;; [ADCS(FORMAT-0-REG-SHIFTED)     xxxx0000|1011xxxx|xxxxxxxx|0xx1xxxx]
;; ADC[s][ne] [r0,r1], [r2,r3], [r0,r4] [shift-type] [r0,r8]
(test-pattern "ADC(FORMAT-0-REG-SHIFTED)" "0000|101x001x|000xx000|0xx10x00")

;; [BIC(FORMAT-0-REG-SHIFTED)      xxxx0001|110xxxxx|xxxxxxxx|0xx1xxxx[
;; [BIC(FORMAT-0-REG-SHIFTED)      xxxx0001|1100xxxx|xxxxxxxx|0xx1xxxx]
;; BIC[s][ne] [r0,r1], [r2,r3], [r0,r4] [shift-type] [r0,r8]
(test-pattern "BIC(FORMAT-0-REG-SHIFTED)" "0001|110x001x|000xx000|0xx10x00")

;; [CMN(FORMAT-1-IMMED)                    xxxx0011|0111xxxx|0000xxxx|xxxxxxxx]
;; CMN[ne] [r0, r1], [#129, #8454144]
(test-pattern "CMN(FORMAT-1-IMMED)" "0011|0111000x|0000x000|10000001")

;; [ORR(FORMAT-1-IMMED)                    xxxx0011|100xxxxx|xxxxxxxx|xxxxxxxx]
;; [ORRS(FORMAT-1-IMMED)                   xxxx0011|1001xxxx|xxxxxxxx|xxxxxxxx]
;; ORR[s][ne] [r0, r1], [r2,r3], [#129, #8454144]
(test-pattern "ORR(FORMAT-1-IMMED)" "0011|100x001x|000xx000|10000001")

;; MOVT(FORMAT-MOV16)                 xxxx0011|0100xxxx|xxxxxxxx|xxxxxxxx
;; The low 16 bits are an immediate which isn't that interesting.
;; MOVT[ne] [r0,r4], [#2730, #2731]
(test-pattern "MOVT(FORMAT-MOV16)" "0011|01001010|0x001010|1010101x")

;; [ROR(FORMAT-0-REG)              xxxx0001|101x0000|xxxxxxxx|x110xxxx
;; :RRX                            xxxx0001|101x0000|xxxx0000|0110xxxx]
;; [RORS(FORMAT-0-REG)             xxxx0001|10110000|xxxxxxxx|x110xxxx
;; :RRXS                           xxxx0001|10110000|xxxx0000|0110xxxx]
;; ROR[s][ne] [r0,r1], [r0,r2], [r8,r9]
(test-pattern "ROR(FORMAT-0-REG)" "0001|101x0000|000x1000|x11000x0")
;; RRX[s][ne] [r0,r1], [r0,r2]
(test-pattern ":RRX" "0001|101x0000|000x0000|011000x0")

;; [STRB(FORMAT-2-IMMED)                   xxxx010x|x1x0xxxx|xxxxxxxx|xxxxxxxx]
;;(test-pattern "010x|x1x0xxxx|xxxxxxxx|xxxxxxxx")
;; STRB[ne] r1, [r12], #3200
;; STRB[ne] r1, [r12, #3200]!
;; STRB[ne] r1, [r12], #3200
;; STRB[ne] r1, [r12], #-3200
;; STRB[ne] r1, [r12, #-3200]!
;; STRB[ne] r1, [r12], #-3200
(test-pattern "STRB(FORMAT-2-IMMED)" "010x|x1x01100|00011100|1000000x")

;; LDRB(FORMAT-2-IMMED)                   xxxx010x|x1x1xxxx|xxxxxxxx|xxxxxxxx
;; ldrb[ne] r1, [r12], #nnn
;; ldrb[ne] r1, [r12, #nnn]
(test-pattern "LDRB(FORMAT-2-IMMED)" "010x|x1x11100|00011100|1000000x")

;; VMSR(FORMAT-VFP-FPSCR)             xxxx1110|11100001|xxxx1010|00010000
;; VMSR fpscr, [r0,r4]
(test-pattern "VMSR(FORMAT-VFP-FPSCR)" "1110|11100001|0x001010|00010000")

;; VADD(FORMAT-VFP-3)                 xxxx1110|0x11xxxx|xxxx1010|x0x0xxxx
;; vadd[ne].f64 [d1,d17], [d0,d2], [d16,d17]
(test-pattern "VADD(FORMAT-VFP-3)" "1110|0x1100x0|00011011|0010000x")
;; vadd[ne].f32 [s2,s3], [s0,s4], [s1,s3]
(test-pattern "VADD(FORMAT-VFP-3)" "1110|0x1100x0|00011010|0010000x")

;; VABS.F32(FORMAT-VFP-2-ARG)     xxxx1110|1x110000|xxxx1010|11x0xxxx
;; This test verifies that the encoded register is what we expected.
;; The intended instruction is VABS.F32 S17, S9
(test-pattern "VABS.F32(FORMAT-VFP-2-ARG)" "1110|11110000|10001010|11100100")
;; General test
(test-pattern "VABS.F32(FORMAT-VFP-2-ARG)" "1110|1x110000|xxxx1010|11x0xxxx")

;; VABS.F64(FORMAT-VFP-2-ARG)     xxxx1110|1x110000|xxxx1011|11x0xxxx
;; VABS.F64 D17, D9
(test-pattern "VABS.F64(FORMAT-VFP-2-ARG)" "1110|11110000|00011011|11001001")

;; VMOV(FORMAT-7-VFP-VMOV-CORE)       xxxx1110|0001xxxx|xxxx1010|x0010000
;; VMOV(FORMAT-7-VFP-VMOV-CORE)       xxxx1110|0000xxxx|xxxx1010|x0010000
;; vmov[ne] r15, s17
(test-pattern "VMOV(FORMAT-7-VFP-VMOV-CORE)" "1110|00011000|11111010|10010000")
;; vmov[ne] s17, r15
(test-pattern "VMOV(FORMAT-7-VFP-VMOV-CORE)" "1110|00001000|11111010|10010000")

;; VMOV(FORMAT-6-VFP-VMOV-CORE-DOUBLE)    xxxx1100|0101xxxx|xxxx1011|00x0xxxx
;; vmov[ne] r8, r3, [d0, d1, d16, d17]
(test-pattern "VMOV(FORMAT-6-VFP-VMOV-CORE-DOUBLE)" "1100|01010011|10001011|00x0000x")
;; VMOV(FORMAT-6-VFP-VMOV-CORE-DOUBLE)    xxxx1100|0100xxxx|xxxx1011|00x0xxxx
;; vmov[ne] [d0, d1, d16, d17], r8, r3
(test-pattern "VMOV(FORMAT-6-VFP-VMOV-CORE-DOUBLE)" "1100|01000011|10001011|00x0000x")

;; VCVT.S32.F32(FORMAT-VFP-2-ARG) xxxx1110|1x111101|xxxx1010|11x0xxxx
;; VCVT.S32.F32  [s17,s19], [s0,s2]
(test-pattern "VCVT.S32.F32(FORMAT-VFP-2-ARG)" "1110|11111101|100x1010|1100000x")

;; VCVT.F32.U32(FORMAT-VFP-2-ARG) xxxx1110|1x111000|xxxx1010|01x0xxxx
;; VCVT.F32.U32 [s17,s19],[s0,s2]
(test-pattern "VCVT.F32.U32(FORMAT-VFP-2-ARG)" "1110|11111000|100x1010|0100000x")

;; VCVT.F32.S32(FORMAT-VFP-2-ARG) xxxx1110|1x111000|xxxx1010|11x0xxxx
;; VCVT.F32.S32 [s17,s19],[s0,s2]
(test-pattern "VCVT.F32.S32(FORMAT-VFP-2-ARG)" "1110|11111000|100x1010|1100000x")

;; VCVTR.U32.F32(FORMAT-VFP-2-ARG) xxxx1110|1x111100|xxxx1010|01x0xxxx
;; VCVTR.U32,F32 [s17,s19],[s0,s2]
(test-pattern "VCVTR.U32.F32(FORMAT-VFP-2-ARG)" "1110|11111100|100x1010|0100000x")

;; VCVTR.S32.F32(FORMAT-VFP-2-ARG) xxxx1110|1x111101|xxxx1010|01x0xxxx
;; VCVTR.S32.F32 [s17,s19], [s0,s2]
(test-pattern "VCVTR.S32.F32(FORMAT-VFP-2-ARG)" "1110|11111101|100x1010|0100000x")

;; VCVT.U32.F32(FORMAT-VFP-2-ARG) xxxx1110|1x111100|xxxx1010|11x0xxxx
;; VCVT.U32.F32  [s17,s19], [s0,s2]
(test-pattern "VCVT.U32.F32(FORMAT-VFP-2-ARG)" "1110|11111100|100x1010|1100000x")

;; VCVT.S32.F32(FORMAT-VFP-2-ARG) xxxx1110|1x111101|xxxx1010|11x0xxxx
(test-pattern "VCVT.S32.F32(FORMAT-VFP-2-ARG)" "1110|11111101|100x1010|1100000x")

;; VCVT.F64.F32(FORMAT-VFP-2-ARG) xxxx1110|1x110111|xxxx1010|11x0xxxx
;; VCVT.F64.F32 [d24,d25], [s2,s4]
(test-pattern "VCVT.F64.F32(FORMAT-VFP-2-ARG)" "1110|11110111|100x1010|1100000x")

;; VCVT.F32.F64(FORMAT-VFP-2-ARG) xxxx1110|1x110111|xxxx1011|11x0xxxx
;; VCVT.F32.F64 [s17,s19], [d0,d1]
(test-pattern "VCVT.F32.F64(FORMAT-VFP-2-ARG)" "1110|11110111|100x1011|1100000x")

;; VCMP.F32(FORMAT-VFP-2-ARG)     xxxx1110|1x110101|xxxx1010|01000000
;; VCMP.F32 s17, #0.0
;; VCMP.F32 s19, #0.0
(test-pattern "VCMP.F32(FORMAT-VFP-2-ARG)" "1110|11110101|100x1010|01000000")

;; VCMP.F64(FORMAT-VFP-2-ARG)     xxxx1110|1x110100|xxxx1011|01x0xxxx
;; VCNP.F64 [d16,d17], [d0,d1]
(test-pattern "VCMP.F64(FORMAT-VFP-2-ARG)" "1110|11110100|000x1011|0100000x")

;; MRS(FORMAT-0-MRS)              xxxx0001|00001111|xxxx0000|00000000
(test-pattern "MRS(FORMAT-0-MRS)" "0001|00001111|xxxx0000|00000000")

;; MSR(FORMAT-0-MSR)              xxxx0001|0010xx00|11110000|0000xxxx
;; msr apsr-nzcvq, [r0,r1,r8,r9]
(test-pattern "MSR(FORMAT-0-MSR)" "0001|00101x00|11110000|0000x00x")
;; msr [apsr-g, apsr-nzcvqg], [r0,r1,r8,r9]
(test-pattern "MSR(FORMAT-0-MSR)" "0001|0010x100|11110000|0000x00x")

;; CLZ(FORMAT-0-CLZ)              xxxx0001|01101111|xxxx1111|0000xxxx
(test-pattern "CLZ(FORMAT-0-CLZ)" "0001|01101111|000x1111|00000x00")

;; B(BRANCH-IMM)                          xxxx1010|xxxxxxxx|xxxxxxxx|xxxxxxxx
(test-pattern "B(BRANCH-IMM)" "1010|00000000|00000000|0000000x")

;; BL(BRANCH-IMM)                         xxxx1011|xxxxxxxx|xxxxxxxx|xxxxxxxx
(test-pattern "BL(BRANCH-IMM)" "1011|00000000|00000000|0000000x")

;; BLX(BRANCH-REG)        xxxx0001|00101111|11111111|0011xxxx
(test-pattern "BLX(BRANCH-REG)" "0001|00101111|11111111|0011010x")

;; BKPT(FORMAT-0-BKPT)    xxxx0001|0010xxxx|xxxxxxxx|0111xxxx
(test-pattern "BKPT(FORMAT-0-BKPT)" "0001|00100000|00000000|01110x00")

;; LSR(FORMAT-0-REG)              xxxx0001|101x0000|xxxxxxxx|x010xxxx
;; LSR[s][ne] [r0,r8], [r2,r3], [#0,#2]
(test-pattern "LSR(FORMAT-0-REG)" "0001|101x0000|0x00000x|0010001x")

;; ASR(FORMAT-0-REG)              xxxx0001|101x0000|xxxxxxxx|x100xxxx
;; ASR[s][ne] [r0, r1], [r0, r2], [#0, #16]
(test-pattern "ASR(FORMAT-0-REG)" "0001|101x0000|000xx000|010000x0")

;; ASR(FORMAT-0-REG-SHIFTED)  xxxx0001|101x0000|xxxxxxxx|0101xxxx
;; ASR[s][ne] [r0,r1], [r4,r5], [r0,r2]
(test-pattern "ASR(FORMAT-0-REG-SHIFTED)" "0001|101x0000|000x00x0|0101010x")

;; [MUL(FORMAT-0-MUL)          xxxx0000|000xxxxx|0000xxxx|1001xxxx]
;; mul[s][ne] [r0,r1], [r8, r9], [r0, r2]
(test-pattern "MUL(FORMAT-0-MUL)" "0000|000x000x|000000x0|1001100x")

;; [UMULL(FORMAT-0-MUL)        xxxx0000|100xxxxx|xxxxxxxx|1001xxxx]
;; umull[s][ne] [r0, r1], [r8, r9], [r2, r3], [r4, r5]
(test-pattern "UMULL(FORMAT-0-MUL)" "0000|100x100x|000x010x|1001001x")

;; [SMULL(FORMAT-0-MUL)        xxxx0000|110xxxxx|xxxxxxxx|1001xxxx]
;; smull[s][ne] [r0, r1], [r8, r9], [r2, r3], [r4, r5]
(test-pattern "SMULL(FORMAT-0-MUL)" "0000|110x100x|000x010x|1001001x")

;; [MLA(FORMAT-0-MUL)          xxxx0000|001xxxxx|xxxxxxxx|1001xxxx]
;; mla[s][ne] [r0, r1], [r2, r3], [r4, r5], [r10, r11]
(test-pattern "MLA(FORMAT-0-MUL)" "0000|001x000x|101x010x|1001001x")

;; [MLS(FORMAT-0-MUL)          xxxx0000|0110xxxx|xxxxxxxx|1001xxxx]
;; mls[ne] [r0, r1], [r8, r9], [r2, r3], [r4, r5]
(test-pattern "[MLS(FORMAT-0-MUL)" "0000|0110000x|101x010x|1001001x")

;; [UDIV(FORMAT-DIV)                   xxxx0111|0011xxxx|1111xxxx|0001xxxx]
;; udiv[ne] [r0, r1], [r4, r5], [r2, r3]
(test-pattern "[UDIV(FORMAT-DIV)" "0111|0011000x|1111001x|0001010x")

;; [NOP(FORMAT-1-IMMED)                    xxxx0011|00100000|11110000|00000000]
;; nop[ne]
(test-pattern "[NOP(FORMAT-1-IMMED)" "0011|00100000|11110000|00000000")

;; [CLZ(FORMAT-0-CLZ)                  xxxx0001|01101111|xxxx1111|0000xxxx]
;; clz[ne] [r0, r1], [r10, r11]
(test-pattern "[CLZ(FORMAT-0-CLZ)" "0001|01101111|000x1111|0000101x")

;; [VMOV.F64(FORMAT-VFP-VMOV-IMMED)    xxxx1110|1x11xxxx|xxxx1011|0000xxxx]
;; vmov[ne] [d1, d17], <float-immed>
(test-pattern "VMOV.F64(FORMAT-VFP-VMOV-IMMED)" "1110|1x11xxxx|00011011|0000xxxx")

;; [VMOV.F32(FORMAT-VFP-VMOV-REG)  xxxx1110|1x110000|xxxx1010|01x0xxxx]
;; vmov[ne].f32 [s0,s1], [s0,s2]
(test-pattern "VMOV.F32(FORMAT-VFP-VMOV-REG)" "1110|1x110000|00001010|0100000x")

;; [VLDR(FORMAT-6-VFP-LOAD/STORE)          xxxx1101|xx01xxxx|xxxx1011|xxxxxxxx]
;; [VLDR(FORMAT-6-VFP-LOAD/STORE)          xxxx1101|xx01xxxx|xxxx1010|xxxxxxxx]
;; vldr.64 [d0], [r1, #+7]
;; vldr.64 [d8], [r1, #+7]
(test-pattern "VLDR(FORMAT-6-VFP-LOAD/STORE)" "1101|10010001|x0001011|00000111")
;; vldr.32 [s0], [r1, #-7]
;; vldr.32 [s16], [r1, #-7]
(test-pattern "VLDR(FORMAT-6-VFP-LOAD/STORE)" "1101|00010001|x0001010|00000111")

;; [VSTR(FORMAT-6-VFP-LOAD/STORE)          xxxx1101|xx00xxxx|xxxx1011|xxxxxxxx]
;; [VSTR(FORMAT-6-VFP-LOAD/STORE)          xxxx1101|xx00xxxx|xxxx1010|xxxxxxxx]
;; vstr.64 [d0], [r1, #+7]
;; vstr.64 [d8], [r1, #+7]
(test-pattern "VSTR(FORMAT-6-VFP-LOAD/STORE)" "1101|10000001|x0001011|00000111")

;; [VSTR(FORMAT-6-VFP-LOAD/STORE)          xxxx1101|xx00xxxx|xxxx1010|xxxxxxxx]
;; vstr.32 [s0], [r1, #-7]
;; vstr.32 [s16], [r1, #-7]
(test-pattern "VSTR(FORMAT-6-VFP-LOAD/STORE)" "1101|00000001|x0001010|00000111")
||#

