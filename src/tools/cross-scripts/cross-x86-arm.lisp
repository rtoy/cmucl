;;; Cross-compile script to build an ARM core using x86 as the
;;; compiling system.  This is based on the x86 to sparc script.
;;;
;;; This needs work!

(in-package :cl-user)

;;; Rename the X86 package and backend so that new-backend does the
;;; right thing.
(rename-package "X86" "OLD-X86" '("OLD-VM"))
(setf (c:backend-name c:*native-backend*) "OLD-X86")

(c::new-backend "ARM"
   ;; Features to add here
   '(:arm
     :vfpv3				; 32 double-float regs
     :linkage-table
     ;;:gencgc				; Generational GC
     :relative-package-names		; Relative package names from Allegro
     :conservative-float-type
     :hash-new
     :random-mt19937			; MT-19937 generator
     :cmu				; Announce this is CMUCL
     :cmu20 :cmu20d			; Current version identifier
     :modular-arith			; Modular arithmetic
     :double-double			; Double-double float support
     :unicode

     :linux
     :unix
     )
   ;; Features to remove from current *features* here
   '(
     ;; Other architectures we aren't using.  Particularly important
     ;; to get rid of sse2 and x87 so we don't accidentally try to
     ;; compile the x87/sse2 float support on sparc, which won't work.
     :x86 :x86-bootstrap :sse2 :x87 :i486
     :alpha :osf1 :mips
     ;; Really old stuff that should have been removed long ago.
     :propagate-fun-type :propagate-float-type :constrain-float-type
     ;; Other OSes were not using
     :openbsd :freebsd :glibc2 :linux :mach-o :darwin :bsd

     :gencgc
     :heap-overflow-check
     :stack-checking
     :pentium
     :long-float
     :new-random
     :small
     :mp))

;;; Changes needed to bootstrap cross-compiling from x86 to arm

;; Set up the linkage space stuff appropriately for arm.
(setf (c::backend-foreign-linkage-space-start c::*target-backend*)
      #x0f000000
      (c::backend-foreign-linkage-entry-size c::*target-backend*)
      ;; FIXME!
      16)

;; Get new fops so we can process fasls with big-endian unicode
;; strings on our little-endian compiling system.
;;#+unicode
;;(load "target:tools/cross-scripts/cross-unicode-big-endian.lisp")

;;; End changes needed to bootstrap cross-compiling from x86 to arm

;;; Extern-alien-name for the new backend.
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  ;;(format t "extern-alien-name: ~S~%" name)
  ;;(lisp::maybe-swap-string 'extern-alien-name (copy-seq name))
  name)
(defconstant c::arm-fasl-file-implementation 13)


;; FIXME: Enable this with the correct code when we have more of the
;; ARM kernel running.
#+(or)
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (unless (zerop (rem offset vm::word-bytes))
    (error (intl:gettext "Unaligned instruction?  offset=#x~X.") offset))
  (system:without-gcing
   (let ((sap (truly-the system-area-pointer
			 (%primitive c::code-instructions code))))
     (ecase kind
       (:call
	(error (intl:gettext "Can't deal with CALL fixups, yet.")))
       (:sethi
	(setf (ldb (byte 22 0) (sap-ref-32 sap offset))
	      (ldb (byte 22 10) fixup)))
       (:add
	(setf (ldb (byte 10 0) (sap-ref-32 sap offset))
	      (ldb (byte 10 0) fixup)))))))
(export 'fixup-code-object)

;; FIXME: implement this correctly
#+(or)
(defun sanctify-for-execution (component)
  (without-gcing
    (alien-funcall (extern-alien "os_flush_icache"
				 (function void
					   system-area-pointer
					   unsigned-long))
		   (code-instructions component)
		   (* (code-header-ref component code-code-size-slot)
		      word-bytes)))
  nil)
(export 'sanctify-for-execution)

;; Export all external VM symbols.  (Stolen from exports.lisp.)
;; FIXME: Can we do this in a better way?  This was required before,
;; but I (rtoy) think this happened when the compiler magic for EXPORT
;; was removed to make EXPORT a regular function without compiler
;; magic.
(export
 (mapcar #'(lambda (s)
	     (intern s "VM"))
	 '("*ASSEMBLY-UNIT-LENGTH*" "*PRIMITIVE-OBJECTS*"
	   "AFTER-BREAKPOINT-TRAP"
	   "ANY-REG-SC-NUMBER" "ARRAY-DATA-SLOT" "ARRAY-DIMENSIONS-OFFSET"
	   "ARRAY-DISPLACED-P-SLOT" "ARRAY-DISPLACEMENT-SLOT"
	   "ARRAY-ELEMENTS-SLOT" "ARRAY-FILL-POINTER-P-SLOT"
	   "ARRAY-FILL-POINTER-SLOT" "ATOMIC-FLAG" "BASE-CHAR-REG-SC-NUMBER"
	   "BASE-CHAR-STACK-SC-NUMBER" "BASE-CHAR-TYPE"
	   "BIGNUM-DIGITS-OFFSET" "BIGNUM-TYPE" "BINDING-SIZE"
	   "BINDING-SYMBOL-SLOT" "BINDING-VALUE-SLOT" "BREAKPOINT-TRAP"
	   "BYTE-CODE-CLOSURE-TYPE" "BYTE-CODE-FUNCTION-TYPE"
	   "BYTE-BITS" "BYTE-REG-SC-NUMBER"
	   "CATCH-BLOCK-CURRENT-CODE-SLOT"
	   "CATCH-BLOCK-CURRENT-CONT-SLOT" "CATCH-BLOCK-CURRENT-UWP-SLOT"
	   "CATCH-BLOCK-ENTRY-PC-SLOT" "CATCH-BLOCK-PREVIOUS-CATCH-SLOT"
	   "CATCH-BLOCK-SC-NUMBER" "CATCH-BLOCK-SIZE" "CATCH-BLOCK-SIZE-SLOT"
	   "CATCH-BLOCK-TAG-SLOT" "CERROR-TRAP"
	   "CLOSURE-FUNCTION-HEADER-TYPE" "CLOSURE-FUNCTION-SLOT"
	   "CLOSURE-HEADER-TYPE" "CLOSURE-INFO-OFFSET" "CODE-BREAKDOWN"
	   "CODE-CODE-SIZE-SLOT" "CODE-CONSTANTS-OFFSET"
	   "CODE-DEBUG-INFO-SLOT" "CODE-ENTRY-POINTS-SLOT" "CODE-HEADER-TYPE"
	   "CODE-TRACE-TABLE-OFFSET-SLOT" "COMPLEX-ARRAY-TYPE"
	   "COMPLEX-BIT-VECTOR-TYPE" "COMPLEX-DOUBLE-FLOAT-FILLER-SLOT"
	   "COMPLEX-DOUBLE-FLOAT-IMAG-SLOT" "COMPLEX-DOUBLE-FLOAT-REAL-SLOT"
	   "COMPLEX-DOUBLE-FLOAT-SIZE" "COMPLEX-DOUBLE-FLOAT-TYPE"
	   "COMPLEX-DOUBLE-REG-SC-NUMBER" "COMPLEX-DOUBLE-STACK-SC-NUMBER" 
	   "COMPLEX-IMAG-SLOT" "COMPLEX-REAL-SLOT"
	   "COMPLEX-LONG-FLOAT-IMAG-SLOT" "COMPLEX-LONG-FLOAT-REAL-SLOT"
	   "COMPLEX-LONG-FLOAT-SIZE" "COMPLEX-LONG-FLOAT-TYPE"
	   "COMPLEX-LONG-REG-SC-NUMBER" "COMPLEX-LONG-STACK-SC-NUMBER" 
	   "COMPLEX-SINGLE-FLOAT-IMAG-SLOT" "COMPLEX-SINGLE-FLOAT-REAL-SLOT"
	   "COMPLEX-SINGLE-FLOAT-SIZE" "COMPLEX-SINGLE-FLOAT-TYPE"
	   "COMPLEX-SINGLE-REG-SC-NUMBER" "COMPLEX-SINGLE-STACK-SC-NUMBER"
	   "COMPLEX-SIZE" "COMPLEX-STRING-TYPE" "COMPLEX-TYPE"
	   "COMPLEX-VECTOR-TYPE" "CONS-CAR-SLOT" "CONS-CDR-SLOT" "CONS-SIZE"
	   "CONSTANT-SC-NUMBER" "CONTROL-STACK-FORK" "CONTROL-STACK-RESUME"
	   "CONTROL-STACK-RETURN" "CONTROL-STACK-SC-NUMBER" "COUNT-NO-OPS"
	   "CURRENT-FLOAT-TRAP" "DEFINE-FOR-EACH-PRIMITIVE-OBJECT"
	   "DESCRIPTOR-REG-SC-NUMBER" "DESCRIPTOR-VS-NON-DESCRIPTOR-STORAGE"
	   "DOUBLE-FLOAT-EXPONENT-BYTE" "DOUBLE-FLOAT-BIAS"
	   "DOUBLE-FLOAT-DIGITS" "DOUBLE-FLOAT-EXPONENT-BYTE"
	   "DOUBLE-FLOAT-FILLER-SLOT" "DOUBLE-FLOAT-HIDDEN-BIT"
	   "DOUBLE-FLOAT-NORMAL-EXPONENT-MAX"
	   "DOUBLE-FLOAT-NORMAL-EXPONENT-MIN" "DOUBLE-FLOAT-SIGNIFICAND-BYTE"
	   "DOUBLE-FLOAT-SIZE" "DOUBLE-FLOAT-TRAPPING-NAN-BIT"
	   "DOUBLE-FLOAT-TYPE" "DOUBLE-FLOAT-VALUE-SLOT"
	   "DOUBLE-INT-CARG-REG-SC-NUMBER" "DOUBLE-REG-SC-NUMBER"
	   "DOUBLE-STACK-SC-NUMBER" "DYLAN-FUNCTION-HEADER-TYPE"
	   "ERROR-TRAP" "EVEN-FIXNUM-TYPE"
	   "EXPORTED-STATIC-SYMBOLS" "EXTERN-ALIEN-NAME"
	   "FDEFN-FUNCTION-SLOT" "FDEFN-NAME-SLOT" "FDEFN-RAW-ADDR-SLOT"
	   "FDEFN-SIZE" "FDEFN-TYPE" "FIND-HOLES" "FIXNUM"
	   "FIXUP-CODE-OBJECT" "FLOAT-DENORMAL-TRAP-BIT"
	   "FLOAT-DIVIDE-BY-ZERO-TRAP-BIT"
	   "FLOAT-IMPRECISE-TRAP-BIT" "FLOAT-INVALID-TRAP-BIT"
	   "FLOAT-OVERFLOW-TRAP-BIT" "FLOAT-SIGN-SHIFT"
	   "FLOAT-UNDERFLOW-TRAP-BIT" "FLOATING-POINT-MODES"
	   "FORWARDING-POINTER-TYPE"
	   "FP-CONSTANT-SC-NUMBER"
	   "FP-DOUBLE-ZERO-SC-NUMBER" "FP-SINGLE-ZERO-SC-NUMBER"
	   "FUNCALLABLE-INSTANCE-FUNCTION-SLOT"
	   "FUNCALLABLE-INSTANCE-HEADER-TYPE" 
	   "FUNCALLABLE-INSTANCE-INFO-OFFSET"
	   "FUNCTION-ARGLIST-SLOT" "FUNCTION-CODE-OFFSET"
	   "FUNCTION-END-BREAKPOINT-TRAP" "FUNCTION-HEADER-TYPE"
	   "FUNCTION-NAME-SLOT" "FUNCTION-NEXT-SLOT" "FUNCTION-POINTER-TYPE"
	   "FUNCTION-SELF-SLOT" "FUNCTION-TYPE-SLOT"
	   "FUNCALLABLE-INSTANCE-LAYOUT-SLOT"
	   "FUNCALLABLE-INSTANCE-LEXENV-SLOT"
	   "GENESIS" "HALT-TRAP" "IGNORE-ME-SC-NUMBER"
	   "IMMEDIATE-BASE-CHAR-SC-NUMBER" "IMMEDIATE-SAP-SC-NUMBER"
	   "IMMEDIATE-SC-NUMBER"
	   "INSTANCE-HEADER-TYPE" "INSTANCE-POINTER-TYPE"
	   "INSTANCE-SLOTS-OFFSET" "INSTANCE-USAGE"
	   "INTERIOR-REG-SC-NUMBER" "INTERNAL-ERROR-ARGUMENTS"
	   "INTERRUPTED-FLAG" "LIST-ALLOCATED-OBJECTS" "LIST-POINTER-TYPE"
	   "LONG-FLOAT-BIAS" "LONG-FLOAT-DIGITS" "LONG-FLOAT-EXPONENT-BYTE"
	   "LONG-FLOAT-HIDDEN-BIT" "LONG-FLOAT-NORMAL-EXPONENT-MAX"
	   "LONG-FLOAT-NORMAL-EXPONENT-MIN" "LONG-FLOAT-SIGNIFICAND-BYTE"
	   "LONG-FLOAT-SIZE" "LONG-FLOAT-TRAPPING-NAN-BIT" "LONG-FLOAT-TYPE"
	   "LONG-FLOAT-VALUE-SLOT" "LONG-REG-SC-NUMBER" "LONG-STACK-SC-NUMBER"
	   "LOWTAG-BITS" "LOWTAG-LIMIT" "LOWTAG-MASK"
	   "MEMORY-USAGE" "MOST-POSITIVE-COST"
	   "NEGATIVE-IMMEDIATE-SC-NUMBER" "NON-DESCRIPTOR-REG-SC-NUMBER"
	   "NULL-SC-NUMBER" "OBJECT-NOT-LIST-TRAP" "OBJECT-NOT-INSTANCE-TRAP"
	   "ODD-FIXNUM-TYPE" "OFFSET-STATIC-SYMBOL" "OTHER-IMMEDIATE-0-TYPE"
	   "OTHER-IMMEDIATE-1-TYPE" "OTHER-POINTER-TYPE"
	   "PAD-DATA-BLOCK" "PENDING-INTERRUPT-TRAP"
	   "PRIMITIVE-OBJECT" "PRIMITIVE-OBJECT-HEADER"
	   "PRIMITIVE-OBJECT-LOWTAG" "PRIMITIVE-OBJECT-NAME"
	   "PRIMITIVE-OBJECT-OPTIONS" "PRIMITIVE-OBJECT-P"
	   "PRIMITIVE-OBJECT-SIZE" "PRIMITIVE-OBJECT-SLOTS"
	   "PRIMITIVE-OBJECT-VARIABLE-LENGTH" "PRINT-ALLOCATED-OBJECTS"
	   "RANDOM-IMMEDIATE-SC-NUMBER" "RATIO-DENOMINATOR-SLOT"
	   "RATIO-NUMERATOR-SLOT" "RATIO-SIZE" "RATIO-TYPE"
	   "REGISTER-SAVE-PENALTY" "RETURN-PC-HEADER-TYPE"
	   "RETURN-PC-RETURN-POINT-OFFSET" "SANCTIFY-FOR-EXECUTION"
	   "SAP-POINTER-SLOT" "SAP-REG-SC-NUMBER" "SAP-SIZE"
	   "SAP-STACK-SC-NUMBER" "SAP-TYPE"
	   "SCAVENGER-HOOK-FUNCTION-SLOT" "SCAVENGER-HOOK-SIZE"
	   "SCAVENGER-HOOK-TYPE" "SCAVENGER-HOOK-VALUE-SLOT"
	   "SCAVENGER-HOOK-NEXT-SLOT"
	   "SIGCONTEXT-FLOATING-POINT-MODES" "SIGCONTEXT-FLOAT-REGISTER"
	   "SIGCONTEXT-PROGRAM-COUNTER" "SIGCONTEXT-REGISTER"
	   "SIGFPE-HANDLER" "SIGNED-REG-SC-NUMBER" "SIGNED-STACK-SC-NUMBER"
	   "SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-TYPE"
	   "SIMPLE-ARRAY-COMPLEX-LONG-FLOAT-TYPE"
	   "SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-TYPE"
	   "SIMPLE-ARRAY-DOUBLE-FLOAT-TYPE"
	   "SIMPLE-ARRAY-LONG-FLOAT-TYPE"
	   "SIMPLE-ARRAY-SINGLE-FLOAT-TYPE"
	   "SIMPLE-ARRAY-TYPE" "SIMPLE-ARRAY-UNSIGNED-BYTE-16-TYPE"
	   "SIMPLE-ARRAY-UNSIGNED-BYTE-2-TYPE"
	   "SIMPLE-ARRAY-UNSIGNED-BYTE-32-TYPE"
	   "SIMPLE-ARRAY-UNSIGNED-BYTE-4-TYPE"
	   "SIMPLE-ARRAY-UNSIGNED-BYTE-8-TYPE"
	   "SIMPLE-ARRAY-SIGNED-BYTE-16-TYPE"
	   "SIMPLE-ARRAY-SIGNED-BYTE-30-TYPE"
	   "SIMPLE-ARRAY-SIGNED-BYTE-32-TYPE"
	   "SIMPLE-ARRAY-SIGNED-BYTE-8-TYPE"
	   "SIMPLE-BIT-VECTOR-TYPE"
	   "SIMPLE-STRING-TYPE" "SIMPLE-VECTOR-TYPE" "SINGLE-FLOAT-BIAS"
	   "SINGLE-FLOAT-DIGITS" "SINGLE-FLOAT-EXPONENT-BYTE"
	   "SINGLE-FLOAT-HIDDEN-BIT" "SINGLE-FLOAT-NORMAL-EXPONENT-MAX"
	   "SINGLE-FLOAT-NORMAL-EXPONENT-MIN" "SINGLE-FLOAT-SIGNIFICAND-BYTE"
	   "SINGLE-FLOAT-SIZE" "SINGLE-FLOAT-TRAPPING-NAN-BIT"
	   "SINGLE-FLOAT-TYPE" "SINGLE-FLOAT-VALUE-SLOT"
	   "SINGLE-INT-CARG-REG-SC-NUMBER"
	   "SINGLE-REG-SC-NUMBER" "SINGLE-STACK-SC-NUMBER"
	   "SINGLE-STEP-BREAKPOINT-TRAP"
	   "SINGLE-VALUE-RETURN-BYTE-OFFSET" "SLOT-DOCS"
	   "SLOT-LENGTH" "SLOT-NAME" "SLOT-OFFSET" "SLOT-OPTIONS"
	   "SLOT-REST-P" "STATIC-FUNCTIONS" "STATIC-FUNCTION-OFFSET"
	   "STATIC-SYMBOL-OFFSET" "STATIC-SYMBOL-P" "STATIC-SYMBOLS"
	   "STRUCTURE-USAGE" "SYMBOL-FUNCTION-SLOT"
	   "SYMBOL-HASH-SLOT" "SYMBOL-HEADER-TYPE" "SYMBOL-NAME-SLOT"
	   "SYMBOL-PACKAGE-SLOT" "SYMBOL-PLIST-SLOT"
	   "SYMBOL-RAW-FUNCTION-ADDR-SLOT" "SYMBOL-SETF-FUNCTION-SLOT"
	   "SYMBOL-SIZE" "SYMBOL-UNUSED-SLOT" "SYMBOL-VALUE-SLOT"
	   "TARGET-BINDING-STACK-START" "TARGET-BYTE-ORDER"
	   "TARGET-CONTROL-STACK-START" "TARGET-DYNAMIC-SPACE-START"
	   "TARGET-FASL-CODE-FORMAT" "TARGET-FASL-FILE-TYPE"
	   "TARGET-FOREIGN-LINKAGE-ENTRY-SIZE"
	   "TARGET-FOREIGN-LINKAGE-SPACE-START"
	   "TARGET-HEAP-ADDRESS-SPACE" "TARGET-MOST-NEGATIVE-FIXNUM"
	   "TARGET-MOST-POSITIVE-FIXNUM" "TARGET-READ-ONLY-SPACE-START"
	   "TARGET-STATIC-SPACE-START" "TRACE-TABLE-CALL-SITE"
	   "TRACE-TABLE-FUNCTION-EPILOGUE" "TRACE-TABLE-FUNCTION-PROLOGUE"
	   "TRACE-TABLE-NORMAL" "TYPE-BITS" "TYPE-MASK" "UNBOUND-MARKER-TYPE"
	   "UNINTERNED-SYMBOL-COUNT" "UNSIGNED-IMMEDIATE-SC-NUMBER"
	   "UNSIGNED-REG-SC-NUMBER" "UNSIGNED-STACK-SC-NUMBER"
	   "UNWIND-BLOCK-CURRENT-CODE-SLOT" "UNWIND-BLOCK-CURRENT-CONT-SLOT"
	   "UNWIND-BLOCK-CURRENT-UWP-SLOT" "UNWIND-BLOCK-ENTRY-PC-SLOT"
	   "UNWIND-BLOCK-SIZE" "VALUE-CELL-HEADER-TYPE" "VALUE-CELL-SIZE"
	   "VALUE-CELL-VALUE-SLOT" "VECTOR-DATA-OFFSET" "VECTOR-LENGTH-SLOT"
	   "VECTOR-MUST-REHASH-SUBTYPE" "VECTOR-NORMAL-SUBTYPE"
	   "VECTOR-VALID-HASHING-SUBTYPE"
	   "WEAK-POINTER-BROKEN-SLOT" "WEAK-POINTER-NEXT-SLOT"
	   "WEAK-POINTER-SIZE" "WEAK-POINTER-TYPE" "WEAK-POINTER-VALUE-SLOT"
	   "WORD-BITS" "WORD-BYTES" "WORD-REG-SC-NUMBER" "WORD-SHIFT"
	   "ZERO-SC-NUMBER"
	   "CALLBACK-ACCESSOR-FORM" "MAKE-CALLBACK-TRAMPOLINE"
	   "FIXNUMIZE"
	   "WEAK-POINTER-MARK-BIT-SLOT"
	   "CHAR-BITS" "CHAR-BYTES"
	   ))
 "VM")

(export
 (mapcar #'(lambda (s)
	     (intern s "VM"))
	 '("ALLOCATION-TRAP"
	   "POSITIVE-FIXNUM-BITS"
	   "FIXNUM-TAG-BITS"
	   "FIXNUM-TAG-MASK"
	   "PSEUDO-ATOMIC-TRAP"
	   "GET-FP-OPERANDS"
	   "PSEUDO-ATOMIC-VALUE"
	   "PSEUDO-ATOMIC-INTERRUPTED-VALUE"
	   "COMPATIBLE-FUNCTION-TYPES-P"
	   "SINGLE-FLOAT-BYTES"
	   "DOUBLE-FLOAT-BYTES"))
 "VM")

(export
 (mapcar #'(lambda (s)
	     (intern s "VM"))
	 '("DOUBLE-DOUBLE-FLOAT"
	   "DOUBLE-DOUBLE-FLOAT-TYPE"
	   "DOUBLE-DOUBLE-FLOAT-HI-SLOT"
	   "DOUBLE-DOUBLE-FLOAT-LO-SLOT"
	   "DOUBLE-DOUBLE-FLOAT-FILLER-SLOT"
	   "DOUBLE-DOUBLE-FLOAT-SIZE"
	   "DOUBLE-DOUBLE-REG-SC-NUMBER"
	   "DOUBLE-DOUBLE-STACK-SC-NUMBER"
	   "DOUBLE-DOUBLE-FLOAT-DIGITS"
	   "SIMPLE-ARRAY-DOUBLE-DOUBLE-FLOAT-TYPE"

	   "COMPLEX-DOUBLE-DOUBLE-FLOAT-TYPE"
	   "COMPLEX-DOUBLE-DOUBLE-FLOAT-FILLER-SLOT"
	   "COMPLEX-DOUBLE-DOUBLE-FLOAT-REAL-HI-SLOT"
	   "COMPLEX-DOUBLE-DOUBLE-FLOAT-REAL-LO-SLOT"
	   "COMPLEX-DOUBLE-DOUBLE-FLOAT-IMAG-HI-SLOT"
	   "COMPLEX-DOUBLE-DOUBLE-FLOAT-IMAG-LO-SLOT"
	   "COMPLEX-DOUBLE-DOUBLE-FLOAT-SIZE"
	   "COMPLEX-DOUBLE-DOUBLE-REG-SC-NUMBER"
	   "COMPLEX-DOUBLE-DOUBLE-STACK-SC-NUMBER"

	   "SIMPLE-ARRAY-COMPLEX-DOUBLE-DOUBLE-FLOAT-TYPE"
	   ))
 "VM")

;;; Compile the new backend.
(pushnew :bootstrap *features*)
(pushnew :building-cross-compiler *features*)
(load "target:tools/comcom")

;;; Load the new backend.
(setf (search-list "c:")
      '("target:compiler/"))
(setf (search-list "vm:")
      '("c:arm/" "c:generic/"))
(setf (search-list "assem:")
      '("target:assembly/" "target:assembly/arm/"))

;; Load the backend of the compiler.

(in-package "C")

(load "vm:vm-macs")
(load "vm:parms")
(load "vm:objdef")
(load "vm:interr")
(load "assem:support")

(load "target:compiler/srctran")
(load "vm:vm-typetran")
(load "target:compiler/float-tran")
(load "target:compiler/saptran")

(load "vm:macros")
(load "vm:utils")

(load "vm:vm")
(load "vm:insts")
(load "vm:primtype")
(load "vm:move")
(load "vm:sap")
(load "vm:system")
(load "vm:char")
(load "vm:float")

(load "vm:memory")
(load "vm:static-fn")
(load "vm:arith")
(load "vm:cell")
(load "vm:subprim")
(load "vm:debug")
(load "vm:c-call")
(load "vm:print")
(load "vm:alloc")
(load "vm:call")
(load "vm:nlx")
(load "vm:values")
(load "vm:array")
(load "vm:pred")
(load "vm:type-vops")

(load "assem:assem-rtns")

(load "assem:array")
(load "assem:arith")
(load "assem:alloc")

(load "c:pseudo-vops")

(check-move-function-consistency)

(load "vm:new-genesis")

;;; OK, the cross compiler backend is loaded.

(setf *features* (remove :building-cross-compiler *features*))

;;; Info environment hacks.
(macrolet ((frob (&rest syms)
	     `(progn ,@(mapcar #'(lambda (sym)
				   `(defconstant ,sym
				      (symbol-value
				       (find-symbol ,(symbol-name sym)
						    :vm))))
			       syms))))
  (frob OLD-VM:BYTE-BITS OLD-VM:WORD-BITS
	OLD-VM::WORD-BYTES
	OLD-VM:CHAR-BITS
	OLD-VM:LOWTAG-BITS
	#+long-float OLD-VM:SIMPLE-ARRAY-LONG-FLOAT-TYPE 
	OLD-VM:SIMPLE-ARRAY-DOUBLE-FLOAT-TYPE 
	OLD-VM:SIMPLE-ARRAY-SINGLE-FLOAT-TYPE
	#+long-float OLD-VM:SIMPLE-ARRAY-COMPLEX-LONG-FLOAT-TYPE 
	OLD-VM:SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-TYPE 
	OLD-VM:SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-TYPE
	OLD-VM:SIMPLE-ARRAY-UNSIGNED-BYTE-2-TYPE 
	OLD-VM:SIMPLE-ARRAY-UNSIGNED-BYTE-4-TYPE
	OLD-VM:SIMPLE-ARRAY-UNSIGNED-BYTE-8-TYPE 
	OLD-VM:SIMPLE-ARRAY-UNSIGNED-BYTE-16-TYPE 
	OLD-VM:SIMPLE-ARRAY-UNSIGNED-BYTE-32-TYPE 
	OLD-VM:SIMPLE-ARRAY-SIGNED-BYTE-8-TYPE 
	OLD-VM:SIMPLE-ARRAY-SIGNED-BYTE-16-TYPE
	OLD-VM:SIMPLE-ARRAY-SIGNED-BYTE-30-TYPE 
	OLD-VM:SIMPLE-ARRAY-SIGNED-BYTE-32-TYPE
	OLD-VM:SIMPLE-BIT-VECTOR-TYPE
	OLD-VM:SIMPLE-STRING-TYPE OLD-VM:SIMPLE-VECTOR-TYPE 
	OLD-VM:SIMPLE-ARRAY-TYPE OLD-VM:VECTOR-DATA-OFFSET
	OLD-VM:DOUBLE-FLOAT-DIGITS
	old-vm:single-float-digits
	OLD-VM:DOUBLE-FLOAT-EXPONENT-BYTE
	OLD-VM:DOUBLE-FLOAT-NORMAL-EXPONENT-MAX
	OLD-VM:DOUBLE-FLOAT-SIGNIFICAND-BYTE
	OLD-VM:SINGLE-FLOAT-EXPONENT-BYTE
	OLD-VM:SINGLE-FLOAT-NORMAL-EXPONENT-MAX
	OLD-VM:SINGLE-FLOAT-SIGNIFICAND-BYTE
	)
  #+double-double
  (frob OLD-VM:SIMPLE-ARRAY-COMPLEX-DOUBLE-DOUBLE-FLOAT-TYPE
	OLD-VM:SIMPLE-ARRAY-DOUBLE-DOUBLE-FLOAT-TYPE)
  )

(let ((function (symbol-function 'kernel:error-number-or-lose)))
  (let ((*info-environment* (c:backend-info-environment c:*target-backend*)))
    (setf (symbol-function 'kernel:error-number-or-lose) function)
    (setf (info function kind 'kernel:error-number-or-lose) :function)
    (setf (info function where-from 'kernel:error-number-or-lose) :defined)))

(defun fix-class (name)
  (let* ((new-value (find-class name))
	 (new-layout (kernel::%class-layout new-value))
	 (new-cell (kernel::find-class-cell name))
	 (*info-environment* (c:backend-info-environment c:*target-backend*)))
    (remhash name kernel::*forward-referenced-layouts*)
    (kernel::%note-type-defined name)
    (setf (info type kind name) :instance)
    (setf (info type class name) new-cell)
    (setf (info type compiler-layout name) new-layout)
    new-value))
(fix-class 'c::vop-parse)
(fix-class 'c::operand-parse)

#+random-mt19937
(declaim (notinline kernel:random-chunk))

(setf c:*backend* c:*target-backend*)

;;; Extern-alien-name for the new backend.
;;; FIXME: implement this correctly!
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  ;;(format t "extern-alien-name: ~S~%" name)
  ;;(lisp::maybe-swap-string 'extern-alien-name (copy-seq name))
  name)
(export 'extern-alien-name)

;; FIXME: Enable this with the correct code when we have more of the
;; ARM kernel running.
#+(or)
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (unless (zerop (rem offset vm::word-bytes))
    (error (intl:gettext "Unaligned instruction?  offset=#x~X.") offset))
  (system:without-gcing
   (let ((sap (truly-the system-area-pointer
			 (%primitive c::code-instructions code))))
     (ecase kind
       (:call
	(error (intl:gettext "Can't deal with CALL fixups, yet.")))
       (:sethi
	(setf (ldb (byte 22 0) (sap-ref-32 sap offset))
	      (ldb (byte 22 10) fixup)))
       (:add
	(setf (ldb (byte 10 0) (sap-ref-32 sap offset))
	      (ldb (byte 10 0) fixup)))))))
(export 'fixup-code-object)
;; FIXME: implement this correctly
#+(or)
(defun sanctify-for-execution (component)
  (without-gcing
    (alien-funcall (extern-alien "os_flush_icache"
				 (function void
					   system-area-pointer
					   unsigned-long))
		   (code-instructions component)
		   (* (code-header-ref component code-code-size-slot)
		      word-bytes)))
  nil)
(export 'sanctify-for-execution)

(in-package :cl-user)

;;; Don't load compiler parts from the target compilation

(defparameter *load-stuff* nil)

;; hack, hack, hack: Make old-x86::any-reg the same as
;; x86::any-reg as an SC.  Do this by adding old-x86::any-reg
;; to the hash table with the same value as x86::any-reg.
     
(let ((ht (c::backend-sc-names c::*target-backend*)))
  (setf (gethash 'old-vm::any-reg ht)
	(gethash 'vm::any-reg ht)))


;;(pushnew :debug *features*)
