;; Basic cross-compile script for cross-compiling from x86 on darwin
;; (Mac OS X) to x86 on Solaris.  This is a basic x86-to-x86
;; cross-compile, except we tweek the features and misfeatures
;; for Solaris/x86.

(in-package :cl-user)

;;; Rename the X86 package and backend so that new-backend does the
;;; right thing.
(rename-package "X86" "OLD-X86" '("OLD-VM"))
(setf (c:backend-name c:*native-backend*) "OLD-X86")

(c::new-backend "X86"
   ;; Features to add here.  These are just examples.  You may not
   ;; need to list anything here.  We list them here anyway as a
   ;; record of typical features for all x86 ports.
   '(:x86 
     :i486
     :pentium
     :stack-checking			; Catches stack overflow
     :heap-overflow-check		; Catches heap overflows
     :relative-package-names		; relative package names
     :mp				; multiprocessing
     :gencgc				; Generational GC
     :conservative-float-type
     :complex-fp-vops
     :hash-new
     :random-mt19937
     :cmu :cmu21 :cmu21e		; Version features
     :double-double			; double-double float support
     :linkage-table

     :solaris :svr4 :sunos :elf
     ;; The :sse2 and :x87 features will get set by the compiling
     ;; lisp, so don't set it here!
     #+x87 :x87
     #+sse2 :sse2
     )
   ;; Features to remove from current *features* here.  Normally don't
   ;; need to list anything here unless you are trying to remove a
   ;; feature.
   '(:x86-bootstrap
     ;; :alpha :osf1 :mips
     :propagate-fun-type :propagate-float-type :constrain-float-type
     :openbsd :freebsd :glibc2 :linux
     :mach-o :darwin
     :long-float :new-random :small))
;;;
(setf *features* (remove :bsd *features*))
;; Set up the linkage space stuff appropriately for x86-solaris.
(setf (c::backend-foreign-linkage-space-start c::*target-backend*)
      #x30000000
      (c::backend-foreign-linkage-entry-size c::*target-backend*)
      8)

;;;
;;; Compile the new backend.
(pushnew :bootstrap *features*)
(pushnew :building-cross-compiler *features*)

;; Make fixup-code-object and sanctify-for-execution in the VM package
;; be the same as the original.  Needed to get rid of a compiler error
;; in generic/core.lisp.  (This halts cross-compilations if the
;; compiling lisp uses the -batch flag.
(import 'old-vm::fixup-code-object "VM")
(import 'old-vm::sanctify-for-execution "VM")
(export 'vm::fixup-code-object "VM")
(export 'vm::sanctify-for-execution "VM")

(load "target:tools/comcom")

;;; Load the new backend.
(setf (search-list "c:")
      '("target:compiler/"))
(setf (search-list "vm:")
      '("c:x86/" "c:generic/"))
(setf (search-list "assem:")
      '("target:assembly/" "target:assembly/x86/"))

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
(when (target-featurep :sse2)
  (load "vm:sse2-sap"))
(load "vm:system")
(load "vm:char")
(if (target-featurep :sse2)
    (load "vm:float-sse2")
    (load "vm:float"))

(load "vm:memory")
(load "vm:static-fn")
(load "vm:arith")
(load "vm:cell")
(load "vm:subprim")
(load "vm:debug")
(load "vm:c-call")
(if (target-featurep :sse2)
    (load "vm:sse2-c-call")
    (load "vm:x87-c-call"))

(load "vm:print")
(load "vm:alloc")
(load "vm:call")
(load "vm:nlx")
(load "vm:values")
;; These need to be loaded before array because array wants to use
;; some vops as templates.
(load (if (target-featurep :sse2)
	  "vm:sse2-array"
	  "vm:x87-array"))
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
	OLD-VM:CHAR-BITS
	OLD-VM:CHAR-BYTES
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
	OLD-VM:DOUBLE-FLOAT-EXPONENT-BYTE
	OLD-VM:DOUBLE-FLOAT-NORMAL-EXPONENT-MAX 
	OLD-VM:DOUBLE-FLOAT-SIGNIFICAND-BYTE
	OLD-VM:SINGLE-FLOAT-EXPONENT-BYTE
	OLD-VM:SINGLE-FLOAT-NORMAL-EXPONENT-MAX
	OLD-VM:SINGLE-FLOAT-SIGNIFICAND-BYTE
	)
  #+double-double
  (frob OLD-VM:SIMPLE-ARRAY-COMPLEX-DOUBLE-DOUBLE-FLOAT-TYPE
	OLD-VM:SIMPLE-ARRAY-DOUBLE-DOUBLE-FLOAT-TYPE))

;; Modular arith hacks
(setf (fdefinition 'vm::ash-left-mod32) #'old-vm::ash-left-mod32)
(setf (fdefinition 'vm::lognot-mod32) #'old-vm::lognot-mod32)
;; End arith hacks

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
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  name)
(export 'extern-alien-name)
(in-package :cl-user)

;;; Don't load compiler parts from the target compilation

(defparameter *load-stuff* nil)

;; hack, hack, hack: Make old-vm::any-reg the same as
;; x86::any-reg as an SC.  Do this by adding old-vm::any-reg
;; to the hash table with the same value as x86::any-reg.
(let ((ht (c::backend-sc-names c::*target-backend*)))
  (setf (gethash 'old-vm::any-reg ht)
	(gethash 'vm::any-reg ht)))
