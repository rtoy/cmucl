;;; Cross-compile script to add 16-bit strings for Unicode support.
;;; Use as the cross-compile script for cross-build-world.sh.

(in-package :cl-user)

;;; Rename the X86 package and backend so that new-backend does the
;;; right thing.
(rename-package "X86" "OLD-X86")
(setf (c:backend-name c:*native-backend*) "OLD-X86")

(c::new-backend "X86"
   ;; Features to add here
   '(:x86 :i486 :pentium
     :stack-checking
     :heap-overflow-check
     :relative-package-names
     :mp
     :gencgc
     :conservative-float-type
     :hash-new :random-mt19937
     :cmu :cmu19 :cmu19e
     :double-double
     :unicode
     )
   ;; Features to remove from current *features* here
   '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Things needed to cross-compile unicode changes.

(in-package "C")
(handler-bind ((error #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'kernel::continue))))
(defconstant array-info
  '((base-char #\NULL 16 old-x86:simple-string-type)
    (single-float 0.0f0 32 old-x86:simple-array-single-float-type)
    (double-float 0.0d0 64 old-x86:simple-array-double-float-type)
    #+long-float (long-float 0.0l0 #+x86 96 #+sparc 128
		  old-x86:simple-array-long-float-type)
    #+double-double
    (double-double-float 0w0 128
		  old-x86::simple-array-double-double-float-type)
    (bit 0 1 old-x86:simple-bit-vector-type)
    ((unsigned-byte 2) 0 2 old-x86:simple-array-unsigned-byte-2-type)
    ((unsigned-byte 4) 0 4 old-x86:simple-array-unsigned-byte-4-type)
    ((unsigned-byte 8) 0 8 old-x86:simple-array-unsigned-byte-8-type)
    ((unsigned-byte 16) 0 16 old-x86:simple-array-unsigned-byte-16-type)
    ((unsigned-byte 32) 0 32 old-x86:simple-array-unsigned-byte-32-type)
    ((signed-byte 8) 0 8 old-x86:simple-array-signed-byte-8-type)
    ((signed-byte 16) 0 16 old-x86:simple-array-signed-byte-16-type)
    ((signed-byte 30) 0 32 old-x86:simple-array-signed-byte-30-type)
    ((signed-byte 32) 0 32 old-x86:simple-array-signed-byte-32-type)
    ((complex single-float) #C(0.0f0 0.0f0) 64
     old-x86:simple-array-complex-single-float-type)
    ((complex double-float) #C(0.0d0 0.0d0) 128
     old-x86:simple-array-complex-double-float-type)
    #+long-float
    ((complex long-float) #C(0.0l0 0.0l0) #+x86 192 #+sparc 256
     old-x86:simple-array-complex-long-float-type)
    #+double-double
    ((complex double-double-float) #C(0.0w0 0.0w0) 256
     old-x86::simple-array-complex-double-double-float-type)
    (t 0 32 old-x86:simple-vector-type)))
)

(load "target:bootfiles/19e/boot-2008-05-cross-unicode-common.lisp")

;; Kill the any deftransforms
(in-package "C")
(dolist (f '(concatenate subseq replace copy-seq))
  (setf (c::function-info-transforms (c::function-info-or-lose f)) nil))

;; End changes for unicode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)

;;; Compile the new backend.
(pushnew :bootstrap *features*)
(pushnew :building-cross-compiler *features*)
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
  (frob OLD-X86:BYTE-BITS OLD-X86:WORD-BITS
	#+long-float OLD-X86:SIMPLE-ARRAY-LONG-FLOAT-TYPE 
	OLD-X86:SIMPLE-ARRAY-DOUBLE-FLOAT-TYPE 
	OLD-X86:SIMPLE-ARRAY-SINGLE-FLOAT-TYPE
	#+long-float OLD-X86:SIMPLE-ARRAY-COMPLEX-LONG-FLOAT-TYPE 
	OLD-X86:SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-TYPE 
	OLD-X86:SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-TYPE
	OLD-X86:SIMPLE-ARRAY-UNSIGNED-BYTE-2-TYPE 
	OLD-X86:SIMPLE-ARRAY-UNSIGNED-BYTE-4-TYPE
	OLD-X86:SIMPLE-ARRAY-UNSIGNED-BYTE-8-TYPE 
	OLD-X86:SIMPLE-ARRAY-UNSIGNED-BYTE-16-TYPE 
	OLD-X86:SIMPLE-ARRAY-UNSIGNED-BYTE-32-TYPE 
	OLD-X86:SIMPLE-ARRAY-SIGNED-BYTE-8-TYPE 
	OLD-X86:SIMPLE-ARRAY-SIGNED-BYTE-16-TYPE
	OLD-X86:SIMPLE-ARRAY-SIGNED-BYTE-30-TYPE 
	OLD-X86:SIMPLE-ARRAY-SIGNED-BYTE-32-TYPE
	OLD-X86:SIMPLE-BIT-VECTOR-TYPE
	OLD-X86:SIMPLE-STRING-TYPE OLD-X86:SIMPLE-VECTOR-TYPE 
	OLD-X86:SIMPLE-ARRAY-TYPE OLD-X86:VECTOR-DATA-OFFSET
	OLD-X86:DOUBLE-FLOAT-EXPONENT-BYTE
	OLD-X86:DOUBLE-FLOAT-NORMAL-EXPONENT-MAX 
	OLD-X86:DOUBLE-FLOAT-SIGNIFICAND-BYTE
	OLD-X86:SINGLE-FLOAT-EXPONENT-BYTE
	OLD-X86:SINGLE-FLOAT-NORMAL-EXPONENT-MAX
	OLD-X86:SINGLE-FLOAT-SIGNIFICAND-BYTE
	)
  #+double-double
  (frob OLD-X86:SIMPLE-ARRAY-COMPLEX-DOUBLE-DOUBLE-FLOAT-TYPE
	OLD-X86:SIMPLE-ARRAY-DOUBLE-DOUBLE-FLOAT-TYPE))

;; Modular arith hacks
(setf (fdefinition 'vm::ash-left-mod32) #'old-x86::ash-left-mod32)
(setf (fdefinition 'vm::lognot-mod32) #'old-x86::lognot-mod32)
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
(export 'fixup-code-object)
(export 'sanctify-for-execution)
(in-package :cl-user)

;;; Don't load compiler parts from the target compilation

(defparameter *load-stuff* nil)

;; hack, hack, hack: Make old-x86::any-reg the same as
;; x86::any-reg as an SC.  Do this by adding old-x86::any-reg
;; to the hash table with the same value as x86::any-reg.
(let ((ht (c::backend-sc-names c::*target-backend*)))
  (setf (gethash 'old-x86::any-reg ht)
	(gethash 'x86::any-reg ht)))
