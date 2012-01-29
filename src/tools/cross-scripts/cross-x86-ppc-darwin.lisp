(in-package :cl-user)

;;; Rename the X86 package and backend so that new-backend does the
;;; right thing.
(rename-package "X86" "OLD-X86" '("OLD-VM"))
(setf (c:backend-name c:*native-backend*) "OLD-X86")

(c::new-backend "PPC"
   ;; Features to add here
   '(:ppc
     :conservative-float-type
     :hash-new :random-mt19937
     :darwin :bsd
     :cmu :cmu20 :cmu20a
     :gencgc
     :relative-package-names
     :modular-arith
     :double-double
     :linkage-table
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
     
     :pentium
     :long-float
     :new-random
     :small
     :mp
     ;; ppc currently doesn't support these.
     :executable
     :heap-overflow-check
     :stack-checking
     :complex-fp-vops))

;;; Changes needed to bootstrap cross-compiling from x86 to ppc

;; ppc doesn't have these features yet.  Remove them.  It is a bug in
;; cross-compiling that these features leak through to the target.
(setf *features* (remove :executable *features*))
(setf *features* (remove :heap-overflow-check *features*))
(setf *features* (remove :stack-checking *features*))
(setf *features* (remove :complex-fp-vops *features*))

;; Set up the linkage space stuff appropriately for ppc.
#+nil
(setf (c::backend-foreign-linkage-space-start c::*target-backend*)
      #x17000000
      (c::backend-foreign-linkage-entry-size c::*target-backend*)
      32)

(in-package "LISP")
;; We need the the fops because the cross-compiled fasl file is in
;; big-endian order for sparc.  When we read in a string, we need to
;; convert the big-endian string to little-endian for x86 so we can
;; process the symbols and such as expected.

#+unicode
(progn
(defconstant ppc::char-bytes 2)  
(defun maybe-swap-string (f name &optional (len (length name)))
  (declare (ignorable f))
  (unless (eq (c:backend-byte-order c:*backend*)
	      (c:backend-byte-order c:*native-backend*))
    (dotimes (k len)
      (let ((code (char-code (aref name k))))
	(setf (aref name k)
	      (code-char (logior (ash (ldb (byte 8 0) code) 8)
				 (ldb (byte 8 8) code))))))
    ;;(format t "~S: new name = ~S~%" f (subseq name 0 len))
    name))

(macrolet ((frob (name code name-size package)
	     (let ((n-package (gensym "PACKAGE-"))
		   (n-size (gensym "SIZE-"))
		   (n-buffer (gensym "BUFFER-"))
		   (k (gensym "IDX-")))
	       `(define-fop (,name ,code)
		  (prepare-for-fast-read-byte *fasl-file*
		    (let ((,n-package ,package)
			  (,n-size (fast-read-u-integer ,name-size)))
		      (when (> ,n-size *load-symbol-buffer-size*)
			(setq *load-symbol-buffer*
			      (make-string (setq *load-symbol-buffer-size*
						 (* ,n-size vm::char-bytes)))))
		      (done-with-fast-read-byte)
		      (let ((,n-buffer *load-symbol-buffer*))
			(read-n-bytes *fasl-file* ,n-buffer 0
				      (* old-vm:char-bytes ,n-size))
			(maybe-swap-string ',name ,n-buffer ,n-size)
			(push-table (intern* ,n-buffer ,n-size ,n-package)))))))))
  (frob fop-symbol-save 6 4 *package*)
  (frob fop-small-symbol-save 7 1 *package*)
  (frob fop-lisp-symbol-save 75 4 *lisp-package*)
  (frob fop-lisp-small-symbol-save 76 1 *lisp-package*)
  (frob fop-keyword-symbol-save 77 4 *keyword-package*)
  (frob fop-keyword-small-symbol-save 78 1 *keyword-package*)

  (frob fop-symbol-in-package-save 8 4
    (svref *current-fop-table* (fast-read-u-integer 4)))
  (frob fop-small-symbol-in-package-save 9 1
    (svref *current-fop-table* (fast-read-u-integer 4)))
  (frob fop-symbol-in-byte-package-save 10 4
    (svref *current-fop-table* (fast-read-u-integer 1)))
  (frob fop-small-symbol-in-byte-package-save 11 1
    (svref *current-fop-table* (fast-read-u-integer 1))))

(define-fop (fop-package 14)
  (let ((name (pop-stack)))
    ;;(format t "xfop-package: ~{~X~^ ~}~%" (map 'list #'char-code name))
    (or (find-package name)
	(error (intl:gettext "The package ~S does not exist.") name))))

(clone-fop (fop-string 37)
	   (fop-small-string 38)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (read-n-bytes *fasl-file* res 0
		  (* old-vm:char-bytes arg))
    (maybe-swap-string 'fop-string res)
    res))

#+unicode
(defun cold-load-symbol (size package)
  (let ((string (make-string size)))
    (read-n-bytes *fasl-file* string 0 (* 2 size))
    ;;(format t "xpre swap cold-load-symbol: ~S to package ~S~%" string package)
    (maybe-swap-string 'cold-load-symbol string)
    ;;(format t "xpost swap cold-load-symbol: ~S to package ~S~%" string package)
    (cold-intern (intern string package) package)))
)

;;; End changes needed to bootstrap cross-compiling from x86 to ppc


;;; Extern-alien-name for the new backend.
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  (concatenate 'string "_" name))
(export 'extern-alien-name)
(export 'fixup-code-object)
(export 'sanctify-for-execution)

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
	OLD-VM:SINGLE-FLOAT-SIGNIFICAND-BYTE)
  #+double-double
  (frob OLD-VM:SIMPLE-ARRAY-COMPLEX-DOUBLE-DOUBLE-FLOAT-TYPE
	OLD-VM:SIMPLE-ARRAY-DOUBLE-DOUBLE-FLOAT-TYPE))

;; Modular arith hacks
(setf (fdefinition 'vm::ash-left-mod32) #'old-vm::ash-left-mod32)
(setf (fdefinition 'vm::lognot-mod32) #'old-vm::lognot-mod32)
;; End arith hacks

;; Hack to define fused-multiply-add and subtract.  Doesn't need to be
;; correct; just needs to exist.
(defun ppc::fused-multiply-subtract (x y z)
  "Compute x*y-z with only one rounding operation"
  (declare (double-float x y z))
  (- (* x y) z))

(defun ppc::fused-multiply-add (x y z)
  "Compute x*y+z with only one rounding operation"
  (declare (double-float x y z))
  (+ (* x y) z))

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
  (concatenate 'string "_" name))
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
  (setf (gethash 'old-vm::any-reg ht)
	(gethash 'vm::any-reg ht)))
