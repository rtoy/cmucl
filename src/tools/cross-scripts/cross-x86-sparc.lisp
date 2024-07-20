;;; Cross-compile script to build a sparc core using x86 as the
;;; compiling system.  This needs work!

(in-package :cl-user)

;;; Rename the X86 package and backend so that new-backend does the
;;; right thing.
(rename-package "X86" "OLD-X86" '("OLD-VM"))
(setf (c:backend-name c:*native-backend*) "OLD-X86")

(c::new-backend "SPARC"
   ;; Features to add here
   '(:sparc
     :sparc-v9				; For Ultrasparc processors
     :complex-fp-vops			; Some slightly faster FP vops on complex numbers
     :linkage-table
     :stack-checking			; Throw error if we run out of stack
     :heap-overflow-check		; Throw error if we run out of
					; heap (This requires gencgc!)
     :gencgc				; Generational GC
     :relative-package-names		; Relative package names from Allegro
     :conservative-float-type
     :hash-new
     :random-mt19937			; MT-19937 generator
     :cmu				; Announce this is CMUCL
     :cmu21 :cmu21e			; Current version identifier
     :modular-arith			; Modular arithmetic
     :double-double			; Double-double float support
     :executable
     
     :solaris
     :svr4
     :sun4
     :sunos
     :unix
     )
   ;; Features to remove from current *features* here
   '(:sparc-v8 :sparc-v7		; Choose only one of :sparc-v7, :sparc-v8, :sparc-v9
     ;; Other architectures we aren't using.  Particularly important
     ;; to get rid of sse2 and x87 so we don't accidentally try to
     ;; compile the x87/sse2 float support on sparc, which won't work.
     :x86 :x86-bootstrap :sse2 :x87 :i486
     :alpha :osf1 :mips
     ;; Really old stuff that should have been removed long ago.
     :propagate-fun-type :propagate-float-type :constrain-float-type
     ;; Other OSes were not using
     :openbsd :freebsd :glibc2 :linux :mach-o :darwin :bsd
     
     :pentium
     :long-float
     :new-random
     :small
     :mp))

;;; Changes needed to bootstrap cross-compiling from x86 to sparc

;; Set up the linkage space stuff appropriately for sparc.
(setf (c::backend-foreign-linkage-space-start c::*target-backend*)
      #x0f800000
      (c::backend-foreign-linkage-entry-size c::*target-backend*)
      16)

;; Get new fops so we can process fasls with big-endian unicode
;; strings on our little-endian compiling system.
#+unicode
(load "target:tools/cross-scripts/cross-unicode-big-endian.lisp")

;;; End changes needed to bootstrap cross-compiling from x86 to sparc

;;; Extern-alien-name for the new backend.
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  ;;(format t "extern-alien-name: ~S~%" name)
  ;;(lisp::maybe-swap-string 'extern-alien-name (copy-seq name))
  name)
(export 'extern-alien-name)
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

;;; Compile the new backend.
(pushnew :bootstrap *features*)
(pushnew :building-cross-compiler *features*)
(load "target:tools/comcom")

;;; Load the new backend.
(setf (search-list "c:")
      '("target:compiler/"))
(setf (search-list "vm:")
      '("c:sparc/" "c:generic/"))
(setf (search-list "assem:")
      '("target:assembly/" "target:assembly/sparc/"))

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
  (frob OLD-VM:BYTE-BITS
	OLD-VM:WORD-BITS
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
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  ;;(format t "extern-alien-name: ~S~%" name)
  ;;(lisp::maybe-swap-string 'extern-alien-name (copy-seq name))
  name)
(export 'extern-alien-name)
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
