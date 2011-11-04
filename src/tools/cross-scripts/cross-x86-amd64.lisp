(in-package :c)

;;; Used to record the source-location of definitions.
;;;
(define-info-class source-location)
(define-info-type source-location defvar (or form-numbers null) nil)

;; Boot file for adding *runtime-features*
(in-package :sys)
(defvar *runtime-features* nil)

(in-package "LISP")

(defun c::%%defconstant (name value doc source-location)
  (when doc
    (setf (documentation name 'variable) doc))
  (when (boundp name)
    (unless (equalp (symbol-value name) value)
      (warn "Constant ~S being redefined." name)))
  (setf (symbol-value name) value)
  (setf (info variable kind name) :constant)
  (clear-info variable constant-value name)
  (set-defvar-source-location name source-location)
  name)

(in-package :disassem)

(use-package :extensions)
(defun sap-ref-int (sap offset length byte-order)
  (declare (type system:system-area-pointer sap)
	   (type (unsigned-byte 16) offset)
	   (type (member 1 2 4 8) length)
	   (type (member :little-endian :big-endian) byte-order)
	   (optimize (speed 3) (safety 0)))
  (ecase length
    (1 (system:sap-ref-8 sap offset))
    (2 (if (eq byte-order :big-endian)
	   (+ (ash (system:sap-ref-8 sap offset) 8)
	      (system:sap-ref-8 sap (+ offset 1)))
	   (+ (ash (system:sap-ref-8 sap (+ offset 1)) 8)
	      (system:sap-ref-8 sap offset))))
    (4 (if (eq byte-order :big-endian)
	   (+ (ash (system:sap-ref-8 sap offset) 24)
	      (ash (system:sap-ref-8 sap (+ 1 offset)) 16)
	      (ash (system:sap-ref-8 sap (+ 2 offset)) 8)
	      (system:sap-ref-8 sap (+ 3 offset)))
	   (+ (system:sap-ref-8 sap offset)
	      (ash (system:sap-ref-8 sap (+ 1 offset)) 8)
	      (ash (system:sap-ref-8 sap (+ 2 offset)) 16)
	      (ash (system:sap-ref-8 sap (+ 3 offset)) 24))))
    (8 (if (eq byte-order :big-endian)
	   (+ (ash (system:sap-ref-8 sap offset) 56)
	      (ash (system:sap-ref-8 sap (+ 1 offset)) 48)
	      (ash (system:sap-ref-8 sap (+ 2 offset)) 40)
	      (ash (system:sap-ref-8 sap (+ 3 offset)) 32)
	      (ash (system:sap-ref-8 sap (+ 4 offset)) 24)
	      (ash (system:sap-ref-8 sap (+ 5 offset)) 16)
	      (ash (system:sap-ref-8 sap (+ 6 offset)) 8)
	      (system:sap-ref-8 sap (+ 7 offset)))
	   (+ (system:sap-ref-8 sap offset)
	      (ash (system:sap-ref-8 sap (+ 1 offset)) 8)
	      (ash (system:sap-ref-8 sap (+ 2 offset)) 16)
	      (ash (system:sap-ref-8 sap (+ 3 offset)) 24)
	      (ash (system:sap-ref-8 sap (+ 4 offset)) 32)
	      (ash (system:sap-ref-8 sap (+ 5 offset)) 40)
	      (ash (system:sap-ref-8 sap (+ 6 offset)) 48)
	      (ash (system:sap-ref-8 sap (+ 7 offset)) 56))))))

(defun read-suffix (length dstate)
  (declare (type (member 8 16 32 64) length)
	   (type disassem-state dstate)
	   (optimize (speed 3) (safety 0)))
  (let ((length (ecase length (8 1) (16 2) (32 4) (64 8))))
    (declare (type (unsigned-byte 3) length))
    (prog1
      (sap-ref-int (dstate-segment-sap dstate)
		   (dstate-next-offs dstate)
		   length
		   (dstate-byte-order dstate))
      (incf (dstate-next-offs dstate) length))))

(defun disassemble-segments (segments stream dstate)
  nil)

(in-package "ALIEN")
(defun sign-extend-32-bit (num)
  (if (> num #x7fffffff)
      (- num #x100000000)
      num))

(def-alien-type-method (integer :naturalize-gen) (type alien)
  (if (and (alien-integer-type-signed type)
	   (< (alien-integer-type-bits type) 64))
      `(sign-extend-32-bit ,alien)
      alien))

(in-package :cl-user)

;; need this since we change them a little
(comf "target:compiler/pack" :load t)
(comf "target:compiler/aliencomp" :load t)

;;; Rename the X86 package and backend so that new-backend does the
;;; right thing.
(rename-package "X86" "OLD-X86")
(setf (c:backend-name c:*native-backend*) "OLD-X86")

(c::new-backend "AMD64"
   ;; Features to add here
   '(:amd64
     :stack-checking :gencgc
     :conservative-float-type
     :hash-new :random-mt19937
     :linux :glibc2 :glibc2.1
     :cmu :cmu18 :cmu18d
     )
   ;; Features to remove from current *features* here
   '(:x86 :i486 :pentium :x86-bootstrap :alpha :osf1 :mips
     :propagate-fun-type :propagate-float-type :constrain-float-type
     :openbsd :freebsd :glibc2 :linux :mp :heap-overflow-check
     :long-float :new-random :small))

;;; Compile the new backend.
(pushnew :bootstrap *features*)
(pushnew :building-cross-compiler *features*)

(in-package :cl-user)

(load "target:tools/comcom")

;;; Load the new backend.
(setf (search-list "c:")
      '("target:compiler/"))
(setf (search-list "vm:")
      '("c:amd64/" "c:generic/"))
(setf (search-list "assem:")
      '("target:assembly/" "target:assembly/amd64/"))

;; Load the backend of the compiler.

(in-package "C")

(load "vm:vm-fndb")

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

(load "target:compiler/codegen")
(load "target:compiler/array-tran.lisp")
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
  (frob OLD-X86:BYTE-BITS
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
	))

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
  #+(and bsd (not elf))
  (concatenate 'string "_" name)
  #-(and bsd (not elf))
  name)
(export 'extern-alien-name)
(export 'fixup-code-object)
(export 'sanctify-for-execution)
(in-package :cl-user)

;;; Don't load compiler parts from the target compilation

(defparameter *load-stuff* nil)

;; hack, hack, hack: Make old-x86::any-reg the same as
;; amd64::any-reg as an SC.  Do this by adding old-x86::any-reg
;; to the hash table with the same value as amd64::any-reg.
(let ((ht (c::backend-sc-names c::*target-backend*)))
  (setf (gethash 'old-x86::any-reg ht)
	(gethash 'amd64::any-reg ht)))
