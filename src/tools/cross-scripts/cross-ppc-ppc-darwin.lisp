(in-package :cl-user)

;;; Rename the X86 package and backend so that new-backend does the
;;; right thing.
(rename-package "PPC" "OLD-PPC" '("OLD-VM"))
(setf (c:backend-name c:*native-backend*) "OLD-PPC")

(c::new-backend "PPC"
   ;; Features to add here
   '(:ppc
     :new-assembler
     :conservative-float-type
     :hash-new
     :random-mt19937
     :darwin :bsd
     :cmu :cmu20 :cmu20a
     :relative-package-names		; Relative package names from Allegro
     :linkage-table
     :modular-arith			; Modular arithmetic
     :double-double			; Double-double floats
     :gencgc				; Generational GC
     )
   ;; Features to remove from current *features* here
   '(:x86-bootstrap :alpha :osf1 :mips :x86 :i486 :pentium :ppro
     :propagate-fun-type :propagate-float-type :constrain-float-type
     :openbsd :freebsd :glibc2 :linux :pentium :elf :mp
     :stack-checking :heap-overflow-check
     :cgc :long-float :new-random :small))

;;; Extern-alien-name for the new backend.
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  (concatenate 'simple-string "_" name))
;; When compiling the compiler, vm:fixup-code-object and
;; vm:sanctify-for-execution are undefined.  Import these to get rid
;; of that error.
(import 'old-vm::fixup-code-object)
(import 'old-vm::sanctify-for-execution)
(export 'extern-alien-name)
(export 'fixup-code-object)
(export 'sanctify-for-execution)

(in-package :cl-user)

;;; Compile the new backend.
(pushnew :bootstrap *features*)
(pushnew :building-cross-compiler *features*)
(load "target:tools/comcom")

;;; Load the new backend.
(setf (search-list "c:")
      '("target:compiler/"))
(setf (search-list "vm:")
      '("c:ppc/" "c:generic/"))
(setf (search-list "assem:")
      '("target:assembly/" "target:assembly/ppc/"))

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
	OLD-VM:DOUBLE-FLOAT-DIGITS
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

;; Modular arith hacks.  When cross-compiling, the compiler wants to
;; constant-fold some stuff, and it needs the following functions to
;; do so.  This just gets rid of the hundreds of errors that happen.
(setf (fdefinition 'vm::ash-left-mod32) #'old-ppc::ash-left-mod32)
(setf (fdefinition 'vm::lognot-mod32) #'old-ppc::lognot-mod32)
;; End modular arith hacks

;; Fused multiply hack.  Don't know why this is needed for a cross-compile
(setf (fdefinition 'vm::fused-multiply-add) #'old-ppc::fused-multiply-add)
(setf (fdefinition 'vm::fused-multiply-subtract) #'old-ppc::fused-multiply-subtract)
;; end

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
  (concatenate 'simple-string "_" name))
(export 'extern-alien-name)
(export 'fixup-code-object)
(export 'sanctify-for-execution)

(in-package :cl-user)

;;; Don't load compiler parts from the target compilation

(defparameter *load-stuff* nil)

;; hack, hack, hack: Make old-x86::any-reg the same as
;; x86::any-reg as an SC.  Do this by adding old-x86::any-reg
;; to the hash table with the same value as x86::any-reg.
     
(let ((ht (c::backend-sc-names c::*target-backend*)))
  (setf (gethash 'old-ppc::any-reg ht)
	(gethash 'ppc::any-reg ht)))

;; A hack for ppc.  Make sure the vop, move-double-to-int-arg, is
;; available in both the OLD-VM and new PPC package.  (I don't know
;; why this is needed, but it seems to be.)
(let ((ht (c::backend-template-names c::*target-backend*)))
  (dolist (syms '((old-ppc::move-double-to-int-arg
		   ppc::move-double-to-int-arg)
		  (old-ppc::move-single-to-int-arg
		   ppc::move-single-to-int-arg)))
    (destructuring-bind (old new)
	syms
      (setf (gethash old
		     ht)
	    (gethash new ht)))))
