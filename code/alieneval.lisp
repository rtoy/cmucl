;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/alieneval.lisp,v 1.5 1990/10/03 15:15:55 ram Exp $
;;;
;;;    This file contains any the part of the Alien implementation that
;;; is not part of the compiler.
;;;
(in-package 'lisp)
(in-package 'system)
(export '(*alien-eval-when* make-alien alien-type alien-size alien-address
			    copy-alien dispose-alien defalien alien-value
			    alien-bind defoperator alien-index alien-indirect
			    bits bytes words long-words port perq-string
			    boolean defenumeration enumeration
			    system-area-pointer pointer alien alien-access
			    alien-assign alien-sap define-alien-stack
			    with-stack-alien null-terminated-string c-procedure
			    unstructured))
(in-package 'lisp)

(defun concat-pnames* (name1 name2)
  (if name1
      (make-symbol (concatenate 'simple-string (symbol-name name1)
				(symbol-name name2)))
      name2))

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* :both))

;;; The number of bits corresponding to a change of 1 in the value of a SAP.
;;;
(defconstant alien-address-unit vm:byte-bits)
(defconstant alien-address-shift (1- (integer-length alien-address-unit)))

;;; The address pointed to by the SAP in an alien is always a multiple of this
;;; number of bits.
;;;
(defconstant alien-alignment vm:word-bits)


(defvar *alien-eval-when* '(compile load eval)
  "This is a list of the times to eval Alien compiler info.")

(defun %print-alien-value (s stream d)
  (declare (ignore d))
  (let ((offset (alien-value-offset s)))
    (format
     stream
     "#<Alien value, Address = #x~X~:[+~D/~D~;~2*~], Size = ~D, Type = ~S>"
     (sap-int (alien-value-sap s)) (zerop offset) offset alien-address-unit
     (alien-value-size s) (alien-value-type s))))

(defun %print-alien-info (s stream d)
  (declare (ignore s d))
  (write-string "#<Alien-Info>" stream))


;;;; Interpreter stubs for SAP functions:

#+new-compiler (progn

(defun pointer< (x y)
  "Return T iff the SAP X points to a smaller address then the SAP Y."
  (declare (type system-area-pointer x y))
  (pointer< x y))

(defun pointer> (x y)
  "Return T iff the SAP X points to a larger address then the SAP Y."
  (declare (type system-area-pointer x y))
  (pointer> x y))

(defun sap+ (sap offset)
  "Return a new sap OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap+ sap offset))

(defun sap- (sap1 sap2)
  "Return the byte offset between SAP1 and SAP2."
  (declare (type system-area-pointer sap1 sap2))
  (sap- sap1 sap2))

(defun sap-int (sap)
  "Converts a System Area Pointer into an integer."
  (declare (type system-area-pointer sap))
  (sap-int sap))

(defun int-sap (int)
  "Converts an integer into a System Area Pointer."
  (declare (type (unsigned-byte #.vm:word-bits) int))
  (int-sap int))

(defun sap-ref-8 (sap offset)
  "Returns the 8-bit byte at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (sap-ref-8 sap offset))

(defun sap-ref-16 (sap offset)
  "Returns the 16-bit word at OFFSET half-words from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (sap-ref-16 sap offset))

(defun sap-ref-32 (sap offset)
  "Returns the 32-bit dualword at OFFSET words from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (sap-ref-32 sap offset))

(defun sap-ref-sap (sap offset)
  "Returns the 32-bit system-area-pointer at OFFSET words from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (sap-ref-sap sap offset))

(defun sap-ref-single (sap offset)
  "Returns the 32-bit single-float at OFFSET words from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (sap-ref-single sap offset))

(defun sap-ref-double (sap offset)
  "Returns the 64-bit double-float at OFFSET words from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (sap-ref-double sap offset))

(defun signed-sap-ref-8 (sap offset)
  "Returns the signed 8-bit byte at Offset bytes from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (signed-sap-ref-8 sap offset))

(defun signed-sap-ref-16 (sap offset)
  "Returns the signed 16-bit word at Offset words from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (signed-sap-ref-16 sap offset))

(defun signed-sap-ref-32 (sap offset)
  "Returns the signed 32-bit dualword at Offset words from SAP."
  (declare (type system-area-pointer sap)
	   (type index offset))
  (signed-sap-ref-32 sap offset))

(defun %set-sap-ref-8 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type index offset)
	   (type (or (signed-byte 8) (unsigned-byte 8)) new-value))
  (setf (sap-ref-8 sap offset) new-value))

(defun %set-sap-ref-16 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type index offset)
	   (type (or (signed-byte 16) (unsigned-byte 16)) new-value))
  (setf (sap-ref-16 sap offset) new-value))

(defun %set-sap-ref-32 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type index offset)
	   (type (or (signed-byte 32) (unsigned-byte 32)) new-value))
  (if (minusp new-value)
      (truly-the (signed-byte 32) (setf (sap-ref-32 sap offset) new-value))
      (truly-the (unsigned-byte 32) (setf (sap-ref-32 sap offset) new-value))))

(defun %set-sap-ref-sap (sap offset new-value)
  (declare (type system-area-pointer sap new-value)
	   (type index offset))
  (setf (sap-ref-sap sap offset) new-value))

(defun %set-sap-ref-single (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type index offset)
	   (type single-float new-value))
  (setf (sap-ref-single sap offset) new-value))

(defun %set-sap-ref-double (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type index offset)
	   (type double-float new-value))
  (setf (sap-ref-double sap offset) new-value))

); #+New-Compiler


;;;; Miscellaneous primitives:

;;; Alien-Assign  --  Public
;;;
;;;    Just blt the bytes from one to the other...
;;;
(defun alien-assign (to-alien from-alien)
  "Copy the data in From-Alien into To-Alien.  The two alien values
  must be of the same size and type."
  (declare (type alien-value to-alien from-alien))
  (unless (equal (alien-value-type to-alien) (alien-value-type from-alien))
    (error "Arguments to Alien-Assign are of different types:~%~S~%~S"
	   to-alien from-alien))
  (let ((size (alien-value-size to-alien))
	(src-off (alien-value-offset from-alien))
	(dst-off (alien-value-offset to-alien)))
    (unless (= size (alien-value-size from-alien))
      (error "Arguments to Alien-Assign are of different sizes:~%~S~%~S"
	     to-alien from-alien))
    (unless (zerop (logand size (1- alien-address-unit)))
      (error "Size of assigned Alien is not a byte multiple:~%~S"
	     from-alien))
    (unless (zerop (logand src-off (1- alien-address-unit)))
      (error "Alien is not byte aligned:~%~S" from-alien))
    (unless (zerop (logand dst-off (1- alien-address-unit)))
      (error "Alien is not byte aligned:~%~S" to-alien))
    (let ((dst-start (ash dst-off (- alien-address-shift))))
      (%primitive byte-blt
		  (alien-value-sap from-alien)
		  (ash src-off (- alien-address-shift))
		  (alien-value-sap to-alien) dst-start
		  (+ dst-start (ash size (- alien-address-shift)))))
    to-alien))


;;; Alien-Type, Alien-Size, Alien-Address, Alien-SAP  --  Public
;;;
;;;    Return the corresponding fields out of the Alien-Value structure.
;;; Convert the address back into rational form.
;;;
(defun alien-type (alien)
  "Return the type of the Alien Value."
  (check-type alien alien-value)
  (alien-value-type alien))

(defun alien-size (alien)
  "Return the size in bits of the Alien." 
  (check-type alien alien-value)
  (alien-value-size alien))

(defun alien-address (alien)
  "Return the address of the data for Alien."
  (check-type alien alien-value)
  (+ (sap-int (alien-value-sap alien))
     (/ (alien-value-offset alien) alien-address-unit)))

(defun alien-sap (alien)
  "Return the system-area-pointer which corresponds to Alien.  Signal an
  error if the alien's address is not word-aligned."
  (check-type alien alien-value)
  (unless (zerop (alien-value-offset alien))
    (error "Argument to Alien-SAP is not word-aligned:~%~S" alien))
  (alien-value-sap alien))


;;;; Interface macros:

;;; Defalien  --  Public
;;;
;;;    Dump the right compiler info and code to create the value at
;;; load-time.
;;;
(defmacro defalien (name type size &optional (address :static))
  "defalien Name Type Size [address]
  Define a global Alien variable with the specified Name, Type, Size and
  Address.  If Address is not supplied allocate storage to hold it."
  `(progn
    (eval-when (load eval)
      (defparameter ,name (make-alien ',type ,size ,address)))
    (eval-when ,*alien-eval-when*
      (setf (info variable alien-value ',name)
	    (make-ct-a-val
	     :type ',type
	     :size ,size
	     :offset ,(if (numberp address)
			  (rem (* address alien-address-unit) alien-alignment)
			  0)
	     :sap `(alien-value-sap ,',name)
	     :alien ',name)))))


;;; Define-Alien-Stack  --  Public
;;;
;;;    Define some variables and a function to grow the stack.  Put good
;;; stuff on the property list.
;;;
(defmacro define-alien-stack (name type size)
  "Define-Stack-Alien Name Type Size
  Defines a new alien stack for use with the With-Stack-Alien macro.
  The aliens have the specifed Type and Size, and are allocated on the
  number stack."
  `(eval-when ,*alien-eval-when*
     (setf (info alien-stack info ',name)
	   (make-stack-info :type ',type :size ,size))))


;;; Defoperator  --  Public
;;;
;;;    Make the Alien-Info for the compiler and the function for the
;;; interpreter.
;;;
(defmacro defoperator ((name result-type) args body)
  (do ((arg args (cdr arg))
       (argnames ())
       (arg-types ())
       (n 0 (1+ n)))
      ((null arg)
       `(eval-when ,*alien-eval-when*
	  (setf (info function alien-operator ',name)
		(make-alien-info #'(lambda ,(nreverse argnames) ,body)
				 ,(length args) ',arg-types ',result-type))
	  (setf (info function source-transform ',name)
		#'c::alien=>lisp-transform)
	  (setf (fdefinition ',name)
		(make-operator-definition ',name))))
    (cond
     ((symbolp (car arg))
      (push (car arg) argnames))
     (t
      (push (cons n (cadar arg)) arg-types) 
      (push (caar arg) argnames)))))

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* nil))


;;;; Alien allocation:

;;;    In order to improve memory locality static alien values are allocated
;;; contiguously in a pre-validated area at the beginning of system space.  We
;;; keep a free pointer to the next word we can allocate.
;;;
#+new-compiler
(defparameter system-space-start (int-sap #x80000000)
  "The address of the first statically allocated alien.")

#+new-compiler
(defparameter alien-allocation-end (int-sap #x8fffffff)
  "The end of statically allocated aliens.")

#+new-compiler
(defvar *current-alien-free-pointer* system-space-start
  "The next word in system space for static alien allocation.")

;;; Allocate-Static-Alien  --  Internal
;;;
;;;    Allocate enough storage to hold the specified number of bits
;;; and return the address.
;;;
#+new-compiler
(defun allocate-static-alien (bits)
  (declare (fixnum bits))
  (let* ((alien *current-alien-free-pointer*)
	 (bytes (logand (ash (the fixnum (+ bits alien-alignment -1))
			     (- alien-address-shift))
			(lognot (1- (truncate alien-alignment
					      alien-address-unit)))))
	 (new (sap+ *current-alien-free-pointer* bytes)))
    (when (#-new-compiler %primitive pointer> new alien-allocation-end)
      (error "Not enough room to allocate a ~D bit alien." bits))
    (setq *current-alien-free-pointer* new)
    alien))


;;; ALLOCATE-SYSTEM-MEMORY -- public
;;;
;;; Allocate random memory from the system area.
;;; 
(defun allocate-system-memory (bytes)
  (declare (type index bytes))
  (gr-call* mach:vm_allocate *task-self* (int-sap 0) bytes t))

;;; REALLOCATE-SYSTEM-MEMORY -- public
;;;
;;; Either allocate more memory at the end of this block, or allocate a new
;;; block and move the old memory into it.
;;; 
(defun reallocate-system-memory (old old-size new-size)
  (declare (type system-area-pointer old)
	   (type index old-size new-size))
  ;; ### Got to work the page size into this somehow.  The vm_allocate
  ;; will fail much more often than it otherwise would 'cause if the old
  ;; block stops in the middle of a page, we can't extend it.
  (if (eql (mach:vm_allocate *task-self*
			     (sap+ old old-size)
			     (- new-size old-size)
			     nil)
	   mach:kern-success)
      old
      (let ((new (allocate-system-memory new-size)))
	(declare (type system-area-pointer new))
	(system-area-copy old 0 new 0 (* old-size vm:byte-bits))
	(deallocate-system-memory old old-size)
	new)))

;;; DEALLOCATE-SYSTEM-MEMORY -- public
;;;
;;; Deallocate that memory.
;;; 
(defun deallocate-system-memory (addr bytes)
  (declare (type system-area-pointer addr)
	   (type index bytes))
  (gr-call* mach:vm_deallocate *task-self* addr bytes))


;;; Make-Alien  --  Public
;;;
;;;    Create an Alien value structure, validating memory to hold the data
;;; if necessary.  We convert the rational address into a word address +
;;; bit offset.
;;;
(defun make-alien (type size &optional (address :dynamic))
  "Return an Alien value of the specified Type, whose size is Size bits.
  Address is the word address of the value to create, if this is not
  supplied then memory is allocated to contain the data."
  (case address
    (:dynamic
     (setq address (allocate-system-memory (ash size (- alien-address-shift)))))
    (:static
     (setq address (allocate-static-alien size)))
    (t
     (check-type address (or #+new-compiler system-area-pointer
			     (rational 0)))))
  (check-type size (integer 0))
  (if (numberp address)
      (multiple-value-bind (base frac) (truncate address)
	(let ((offset (* frac alien-address-unit)))
	  (unless (integerp frac)
	    (error "Address ~S does not fall on a bit position." address))
	  (make-alien-value (int-sap base) offset size type)))
      (make-alien-value address 0 size type)))


;;; Copy-Alien  --  Public
;;;
;;;   Validate some memory and byte-blt the contents to it.  Since we just move
;;; words we preserve a nonzero bit-offset when it might be desirable to
;;; eliminate it, but that is more trouble than it is worth, since non-aligned
;;; fields are probably rarely copied.
;;;
(defun copy-alien (alien)
  "Copy the storage pointed to by Alien and return a new alien value that
  describes it."
  (check-type alien alien-value)
  (let* ((offset (alien-value-offset alien))
	 (length (alien-value-size alien))
	 (bytes (ash (+ length offset alien-alignment -1)
		     (- alien-address-shift)))
	 (new (allocate-system-memory bytes)))
    (%primitive byte-blt
		(alien-value-sap alien) (ash offset (- alien-address-shift))
		new 0 bytes)
    (make-alien-value new offset length (alien-value-type alien))))


;;; Dispose-Alien  --  Public
;;;
;;;    Invalidate the memory pointed to by unless it is a statically
;;; allocated alien.
;;;
#+new-compiler
(defun dispose-alien (alien)
  "Release the storage allocated for Alien."
  (check-type alien alien-value)
  (let ((address (alien-value-sap alien)))
    (unless (not (or (pointer< address system-space-start)
		     (pointer> address alien-allocation-end)))
      (gr-call mach:vm_deallocate *task-self* address
	       (logand #x-200 (ash (+ (alien-value-size alien) #xFFF)
				   (- alien-address-shift)))))))


;;;; Operator definition primitives:

;;; Alien-Index  --  Public
;;;
;;;    Check that the selected field fits within the Alien, and add it
;;; in if it does.
;;;
(defun alien-index (alien offset size)
  "Return a new Alien value that is Offset bits within Alien, and is
  Size bits long."
  (check-type alien alien-value)
  (check-type offset (integer 0))
  (check-type size (integer 0))
  (when (> (+ offset size) (alien-value-size alien))
    (error "~S is too small to extract a ~A bit field at ~A."
	   alien size offset))
  (multiple-value-bind (words bits)
		       (truncate (+ offset (alien-value-offset alien))
				 alien-alignment)
    (make-alien-value
     (int-sap (+ (* words (/ alien-alignment alien-address-unit))
		 (sap-int (alien-value-sap alien))))
     bits
     size
     nil)))

;;; Alien-Indirect  --  Public
;;;
;;;    Check that Alien is word-aligned and 32 bits long.  If it is, grab
;;; the value.  Check that the value falls within the system area.  If
;;; it does then make a new Alien-Value out of it.
;;;
(defun alien-indirect (alien size)
  "Return a new Alien value that points to what the value of Alien points to
  which is Size bits long."
  (check-type alien alien-value)
  (check-type size (integer 0))
  (unless (= (alien-value-size alien) 32)
    (error "~S is not thirty-two bits long." alien))
  (unless (zerop (alien-value-offset alien))
    (error "~S is not word aligned."))
  (let* ((sap (alien-value-sap alien))
	 (value (sap-ref-sap sap 0)))
#|
    (unless (<= system-space-start value most-positive-fixnum)
      (error "The value of ~S, #x~X, does not point into system space."
	     alien value))
|#
    (make-alien-value value 0 size nil)))


;;; Bits, Bytes, Words, Long-Words  --  Public
;;;
;;;
(macrolet ((frob (name n)
	     `(progn
		(proclaim '(inline ,name))
		(defun ,name (n)
		  (declare (type (integer 0 ,(truncate most-positive-fixnum n))
				 n))
		  (* n ,n)))))
  (frob bits 1)
  (frob bytes 8)
  (frob words 16)
  (frob long-words 32))


;;;; General case versions of compiler internal functions:

;;; %Alien-Indirect  --  Internal
;;;
;;;    Alien-Indirect is transformed into this.  Calls can take place at
;;; run-time when the operation can't be proven safe at compile time.
;;;
(defun %alien-indirect (size sap offset exp)
  (unless (eql size 32)
    (error "Argument to Alien-Indirect is ~D bits, not 32:~% ~S." size exp))
  (unless (zerop (logand offset #x1F))
    (error "Offset ~D to Alien-Indirect is not long-word-aligned:~% ~S."
	   offset exp))
  (sap-ref-sap sap (ash offset -5)))


;;; %Aligned-SAP  --  Internal
;;;
;;;    Various things transform into this when the alien is declared to be
;;; aligned.  In this case, we absorb the offset into the SAP, and make the
;;; bound offset 0.
;;;
#+new-compiler
(defun %aligned-sap (sap offset form)
  (unless (zerop (logand offset #x1F))
    (error "Offset ~S was declared to be word aligned, but isn't:~% ~S"
	   offset form))
  (sap+ sap (ash offset (- alien-address-unit))))

#+new-compiler
;;; Naturalize-Integer  --  Internal
;;;
;;;    Read a possibly signed integer somewhere.
;;;
(defun naturalize-integer (signed sap offset size form)
  (declare (type boolean signed)
	   (type system-area-pointer sap)
	   (type index offset size))
  (let ((bit-offset (logand offset #x1f)))
    (cond
     ((and (= size 32) (zerop bit-offset))
      (if signed
	  (signed-sap-ref-32 sap (ash offset -5))
	  (sap-ref-32 sap (ash offset -5))))
     ((and (= size 16) (zerop (logand offset #xf)))
      (if signed
	  (signed-sap-ref-16 sap (ash offset -4))
	  (sap-ref-16 sap (ash offset -4))))
     ((and (= size 8) (zerop (logand offset #x7)))
      (if signed
	  (signed-sap-ref-8 sap (ash offset -3))
	  (sap-ref-8 sap (ash offset -3))))
     ((> size 32)
      (error "Access of ~D bit integers is not supported:~% ~S" size form))
     ((zerop bit-offset)
      (let ((value (ldb (byte size 0) (sap-ref-32 sap (ash offset -5)))))
	(if (and signed (logbitp value (1- size)))
	    (logior value (ash -1 size))
	    value)))
     ((<= (+ size bit-offset) 32)
      (let ((value (ldb (byte size bit-offset)
			(sap-ref-32 sap (ash offset -5)))))
	(if (and signed (logbitp value (1- size)))
	    (logior value (ash -1 size))
	    value)))
     (t
      (macrolet ((low-byte ()
			   (ecase vm:target-byte-order
			     (:little-endian 'offset)
			     (:bit-endian '(1+ offset))))
		 (high-byte ()
			    (ecase vm:target-byte-order
			      (:little-endian '(1+ offset))
			      (:bit-endian 'offset))))
	(let* ((high-bits (- 32 bit-offset))
	       (low-bits (- size high-bits))
	       (value (logior (ash (ldb (byte high-bits 0)
					(sap-ref-32 sap (high-byte)))
				   low-bits)
			      (ash (sap-ref-32 sap (low-byte))
				   (- (- 32 low-bits))))))
	  (if (and signed (logbitp value (1- size)))
	      (logior value (ash -1 size))
	      value)))))))


#+new-compiler
;;; Deport-Integer  --  Internal
;;;
;;;    Like Naturalize-Integer, but writes an integer.
;;;
(defun deport-integer (signed sap offset size value form)
  (declare (type boolean signed)
	   (type system-area-pointer sap)
	   (type index offset size)
	   (type integer value))
  (declare (ignore signed))
  (multiple-value-bind (q r) (truncate offset 32)
    (declare (fixnum r))
    (cond
     ((and (= size 32) (zerop r))
      (setf (sap-ref-32 sap q) value))
     ((and (= size 16) (zerop (logand r #xf)))
      (setf (sap-ref-16 sap (ash offset -4)) value))
     ((and (= size 8) (zerop (logand r #x7)))
      (setf (sap-ref-8 sap (ash offset -3)) value))
     ((> size 32)
      (error "Storing of ~D bit integers is not supported:~% ~S"
	     size form))
     ((zerop r)
      (setf (ldb (byte size 0) (sap-ref-32 sap q)) value))
     ((<= (+ r size) 32)
      (setf (ldb (byte size r) (sap-ref-32 sap q)) value))
     (t
      (macrolet ((low-byte ()
			   (ecase vm:target-byte-order
			     (:little-endian 'offset)
			     (:bit-endian '(1+ offset))))
		 (high-byte ()
			    (ecase vm:target-byte-order
			      (:little-endian '(1+ offset))
			      (:bit-endian 'offset))))
	(let* ((high-bits (- 32 r))
	       (low-bits (- size high-bits)))
	  (setf (ldb (byte high-bits 0)
		     (sap-ref-32 sap (high-byte)))
		(ash value (- low-bits)))
	  (setf (ldb (byte low-bits (- 32 low-bits))
		     (sap-ref-32 sap (low-byte)))
		value))))))
  nil)


;;; Naturalize and Deport Boolean  --  Internal
;;;
;;;    Handle the general case of boolean access.  The transforms
;;; should pick off the normal cases.
;;;
#+new-compiler
(defun naturalize-boolean (sap offset size form)
  (declare (type system-area-pointer sap)
	   (type index offset size))
  (declare (notinline naturalize-integer))
  (not (zerop (naturalize-integer nil sap offset size form))))
;;;
#+new-compiler
(defun deport-boolean (sap offset size value form)
  (declare (type system-area-pointer sap)
	   (type index offset size)
	   (type boolean value))
  (declare (notinline deport-integer))
  (deport-integer nil sap offset size (if value 1 0) form)
  nil)

;;; Check<=, Check=  --  Internal
;;;
;;;    Interpreter stubs for functions normally open-coded.  Note that the
;;; compiler will constant-fold these, possibly producing an error at compile
;;; time.
;;;
(defun check<=  (x y) (check<= x y))
(defun check= (x y)
  #|
  (check= x y)
  |#
  ;### Bootstrap hack:
  (unless (and (fixnump x) (>= x 0)
	       (fixnump y) (>= y 0)
	       (= x y))
    (error "Not ~S not = to ~S at compile time." x y)))


;;;; Utility functions used by macros and special forms:

;;; Check-Alien-Type  --  Internal
;;;
;;;    Check that Alien is of the specified Alien type, and give an
;;; error if it is not. 
;;;
(defun check-alien-type (alien type)
  (unless (alien-value-p alien)
    (error "~S is not an Alien value." alien))
  (unless (equal (alien-value-type alien) type)
    (error "Wrong Alien type ~S, should have been of type ~S."
	   (alien-value-type alien) type))
  alien)


;;; Assert-Alien-Type  --  Internal
;;;
;;;    Make a new Alien value having the specified type.
;;;
(defun assert-alien-type (alien type)
  (unless (alien-value-p alien)
    (error "~S is not an Alien value." alien))
  (make-alien-value (alien-value-sap alien)
		    (alien-value-offset alien)
		    (alien-value-size alien)
		    type))


;;; MAKE-OPERATOR-DEFINITION  --  Internal
;;;
;;;    To save space and load & compile time for defoperators, we don't
;;; actually generate the function for the operator until it is called.  This
;;; function returns a closure that, when called, computes the definition and
;;; installs as the definition of Name.  This results in a significant space
;;; savings at the cost of always running the operator definition interpreted
;;; when called that way.
;;;
(defun make-operator-definition (name)
  #'(lambda (&rest actual-args)
      (let ((info (info function alien-operator name)))
	(unless info
	  (error "Operator ~S has no Alien-Operator-Info property."))
	(let ((num-args (alien-info-num-args info))
	      (arg-types (alien-info-arg-types info)))
	  (collect ((args)
		    (binds))
	    (dotimes (i num-args)
	      (let ((type (assoc i arg-types))
		    (sym (gensym)))
		(args sym)
		(when type
		  (binds `(,sym ,sym ,(cdr type))))))

	    (let ((res (coerce `(lambda ,(args)
				  (alien-bind ,(binds)
				    (assert-alien-type
				     ,(apply (alien-info-function info) (args))
				     ',(alien-info-result-type info))))
			       'function)))
	      (setf (fdefinition name) res)
	      (apply res actual-args)))))))


;;;; Alien access method definition:
;;;
;;;    We describe how to access and store at an Alien's address in
;;; a way that permits the same code to be use both for the compiler
;;; and the interpreter.  What we do is have experts that ask things
;;; about the alien value and return a piece of code to do the access.
;;;
;;;
(defvar *alien-access-table* (make-hash-table :test #'eq)
  "Hashtable from lisp types to Alien access functions.")
(defvar *alien-only-access-table* (make-hash-table :test #'eq)
  "Hashtable from alien types to Alien access functions.")
(defvar *alien-to-lisp-types* (make-hash-table :test #'eq)
  "Hashtable we use to tell whether there is a unique lisp-type
   for a given alien-type.")

(defmacro mostcar (x)
  `(if (listp ,x) (car ,x) ,x))


;;; %Define-Alien-Access  --  Internal
;;;
(defun %define-alien-access (lisp-type atypes fun)
  (dolist (type atypes)
    (let ((res (gethash type *alien-to-lisp-types*)))
      (cond ((not res)
	     (setf (gethash type *alien-to-lisp-types*) lisp-type))
	    ((eq res lisp-type))
	    (t
	     (setf (gethash type *alien-to-lisp-types*) '%conflict%))))

    (setf (gethash type *alien-only-access-table*) fun)
    (setf (gethash lisp-type *alien-access-table*) fun)))


;;; Get-Alien-Access-Method  --  Internal
;;;
;;;    Returns the alien-access method corresponding to Alien-Type and
;;; Lisp-Type or dies trying.
;;;
(defun get-alien-access-method (alien-type lisp-type)
  (let* ((alien-type (mostcar alien-type))
	 (lisp-type (mostcar lisp-type))
	 (unique? (gethash alien-type *alien-to-lisp-types*)))
    (cond
     ((not unique?)
      (error "Alien type ~S does not correspond to any Lisp type." alien-type))
     ((and (null lisp-type)
	   (not (eq unique? '%conflict%))
	   (gethash alien-type *alien-only-access-table*)))
     ((not (eq unique? '%conflict%))
      (gethash unique? *alien-access-table*))
     ((not lisp-type)
      (error "Lisp-Type must be specified with Alien type ~S." alien-type))
     ((gethash lisp-type *alien-access-table*))
     (t
      (error "~S is not a Lisp-Type known to Alien-Access."
	     lisp-type)))))


;;; Define-Alien-Access  --  Internal
;;;
(defmacro define-alien-access ((lisp-type &optional (atype lisp-type)
					  &rest more-types)
			       (alien-var kind-var value-var
					  &optional
					  (source-var (gensym) source-p))
			       &body body)
  "Define-Alien-Access (Lisp-Type {Alien-Type}*) (Alien-Var Kind-Var Value-Var)
		       {form}*

  Define a new type for Alien-Access.  When Alien-Access is given the specified
  Lisp-Type and the alien is one of the specified Alien-Types, then body will
  be evaluated to get an expression that does the the appropriate access/store.
  If no Alien-Type is supplied, then the accepted Alien type is Lisp-Type.  If
  the type of the Alien is a list, then we use the car as the type.

  Alien-Var
      Bound to the Alien type of the Alien value to access.

  Kind-Var
      Bound to :read or :write, indicating whether to read or store a value.

  Value-Var
      When Kind-Var is :write, this is bound to an expression to evaluate to
      obtain the value to be stored.

  Source-Var
      If specified, this is bound to the original Alien-Access form
      (for use in error messages.)

  In order to obtain the Alien value at which the access is to be done, the
  local With-Alien macro should be used:

  With-Alien (Sap-Var) (Offset-Var {Key Value}*) (Size-Var {Key Value}*)
              {Form}*
  This macro is for use within the body of a Define-Alien-Access.  It
  analyzes and verifies assertions on the alien value to be accessed.
  Sap-Var, Offset-Var and Size-Var are bound to expressions for the
  system-area-pointer to the value, the offset from it in bits and
  the size of the Alien in bits.

  The keyword arguments for the offset and size may be used to place
  constraints on the values they may assume.  If the value
  is Nil, that is taken to be a null constraint.  The following
  keys are defined:
   :unit      -- A integer (default 32).
		 Asserts that the value is a multiple of this number, and
		 that the value is to be divided by this number before any
		 other options are processed.
   :constant  -- An integer (default Nil).
		 Asserts that the value must be exactly the supplied value.
   :minimum   -- An integer (default Nil).
		 Asserts that the value may not be less than the supplied
		 value.

  The value of With-Alien is the value of the last form."

  (let ((n-sap (gensym))
	(n-offset (gensym))
	(n-size (gensym)))
    `(progn
       (%define-alien-access
	',lisp-type '(,atype ,@more-types)
	#'(lambda (,n-sap ,n-offset ,n-size ,alien-var ,kind-var ,value-var
			  ,source-var)
	    ,@(unless source-p
		`((declare (ignore ,source-var))))
	    
	    (unless (member (mostcar ,alien-var) '(,atype ,@more-types)
			    :test #'eq)
	      (error "Wrong Alien type ~S, should have been ~S~
	      ~{~#[~; or~:;,~] ~S~}."
		     ,alien-var ',atype ',more-types))
	    
	    (macrolet ((with-alien ((sap)
				    (offset &key
					    ((:unit ounit) 32))
				    (size &key
					  ((:constant sconst) nil)
					  ((:minimum smin) nil)
					  ((:unit sunit) 32))
				    &body (body decls))
			 (%with-alien sap offset ounit
				      size sconst smin sunit
				      ',n-sap ',n-offset ',n-size
				      body decls)))
	      ,@body)))
       #-new-compiler
       (eval-when (compile)
	 (clc::clc-mumble "alien-access ~S compiled.~%" ',lisp-type)))))


(eval-when (compile load eval)

(defun %with-alien (sap offset ounit size sconst smin sunit
			n-sap n-offset n-size body decls)
  (let ((sunit (or sunit 1))
	(ounit (or ounit 1))
	(n-scaled-offset (gensym))
	(n-scaled-size (gensym)))
    `(let ((,sap ,n-sap)
	   (,offset ',n-scaled-offset)
	   (,size ',n-scaled-size))
       ,@decls
       `(let ((,',n-scaled-offset (/ ,,n-offset ,,ounit))
	      (,',n-scaled-size (/ ,,n-size ,,sunit)))
	  ,',n-scaled-size ; Ignorable...
	  ,,@(when sconst
	       `(`(check= ,',n-scaled-size ,,sconst)))
	  ,,@(when smin
	       `(`(check<= ,,smin ,',n-scaled-size)))
	  ,,@body))))

); Eval-When (Compile Load Eval)


;;; Alien-Access  --  Public
;;;
;;;    This is only called when we can't open-code because the types aren't
;;; constant (or are erroneous.)  In this case, we must look up the access
;;; method at run-time, compute the access form, then eval it.
;;;
(defun alien-access (alien &optional lisp-type)
  "Converts the bits described by Alien into an object of type Lisp-Type,
  or dies trying."
  (declare (type alien-value alien))
  (let* ((alien-type (alien-value-type alien))
	 (access (get-alien-access-method alien-type lisp-type))
	 (n-sap (gensym))
	 (n-offset (gensym))
	 (n-size (gensym)))
    (eval
     `(let ((,n-sap ',(alien-value-sap alien))
	    (,n-offset ',(alien-value-offset alien))
	    (,n-size ',(alien-value-size alien)))
	,(funcall access n-sap n-offset n-size alien-type :read nil
		  'alien-access)))))

(defsetf alien-access %set-alien-access
  "Stores the representation of the value into the alien.")

;;; %Set-Alien-Access  --  Public
;;;
;;;    Like Alien-Access, only we call the expert with :write and return
;;; the new value.
;;;
(defun %set-alien-access (alien lisp-type &optional (new-value nil nvp))
  (declare (type alien-value alien))
  (multiple-value-bind (lisp-type new-value)
		       (if nvp
			   (values lisp-type new-value)
			   (values nil lisp-type))
    (let* ((alien-type (alien-value-type alien))
	   (access (get-alien-access-method alien-type lisp-type))
	   (n-sap (gensym))
	   (n-offset (gensym))
	   (n-size (gensym))
	   (n-nval (gensym)))
      (eval
       `(let ((,n-sap ',(alien-value-sap alien))
	      (,n-offset ',(alien-value-offset alien))
	      (,n-size ',(alien-value-size alien))
	      (,n-nval ,new-value))
	  ,(funcall access n-sap n-offset n-size alien-type :write n-nval
		    '(setf alien-access))))
      new-value)))


;;;; Miscellaneous alien access methods:

;;; Alien-Access expert for Port  --  Internal
;;;
;;;    Just find the place and grab or store a 32-bit port, depending
;;; on the access kind, treating ports as 32-bit signed integers. --JRG
;;;
(define-alien-access (port) (type kind value form)
  ;;
  ;; We want to know if the offset is constant, and the size must be two
  ;; words.
  (with-alien (sap)
	      (offset :unit nil)
	      (size :constant 32
		    :unit nil)
    (declare (ignore size))
    (if (eq kind :read)
	`(naturalize-integer t ,sap ,offset 32 ',form)
	`(deport-integer t ,sap ,offset 32 ,value ',form))))

;;; Alien-Access expert for String-Char  --  Internal
;;;
;;;    Kind of easy, but we need to deal with random bit positions.  What we do
;;; is call Naturalize-Integer or Deport-Integer to do the access, and just do
;;; the type conversion.
;;;
(define-alien-access (string-char) (type kind value form)
  (with-alien (sap)
	      (offset :unit nil)
	      (size :unit nil :minimum 8)
    (if (eq kind :read)
	`(code-char (naturalize-integer nil ,sap ,offset ,size ',form))
	`(deport-integer nil ,sap ,offset ,size (char-code ,value) ',form))))

;;; Alien-Access experts for Unsigned-Byte, Signed-Byte  --  Internal
;;;
;;;    Like, just call Naturalize or Deport Integer.
;;;
(define-alien-access (unsigned-byte) (type kind value form)
  (with-alien (sap)
	      (offset :unit nil)
	      (size :minimum (cadr type)
		    :unit nil)
    (if (eq kind :read)
	`(naturalize-integer nil ,sap ,offset ,size ',form)
	`(deport-integer nil ,sap ,offset ,size ,value ',form))))
;;;
(define-alien-access (signed-byte) (type kind value form)
  (with-alien (sap)
	      (offset :unit nil)
	      (size :minimum (cadr type)
		    :unit nil)
    (if (eq kind :read)
	`(naturalize-integer t ,sap ,offset ,size ',form)
	`(deport-integer t ,sap ,offset ,size ,value ',form))))

#| 
I didn't write this, and it is wrong, since it assumes the size is constant.
In fact, it will never be constant at this point in the new compiler.  I also
don't know that it is supposed to be used for.  I suspect it is a PERQ crock.

(define-alien-access (unstructured) (type kind value)
  (with-alien (sap)
	      (offset :unit nil)
	      (size :unit nil
		    :minimum (cadr type))
    (if (eq kind :read)
	(if (<= size 32)
	    `(naturalize-integer nil ,sap ,offset ,size)
	    (if (and (= (mod offset 32) 0) (= (mod size 32) 0))
		(do ((i 32 (+ i 32))
		     (form `(naturalize-integer nil ,sap ,offset 32)))
		    ((>= i size) form)
		  (setq form
			`(logior (ash ,form 32)
				 (naturalize-integer nil ,sap
						     ,(+ offset i) 32))))
		(error
		 "Unstructured aliens of size ~D and offset ~D not supported."
		 size offset)))
	(if (<= size 32)
	    `(deport-integer nil ,sap ,offset ,size ,value)
	    (if (and (= (mod offset 32) 0) (= (mod size 32) 0))
		(do ((i (- size 32) (- i 32))
		     (shift 0 (- shift 32))
		     (form ()))
		    ((< i 0) (nreverse form))
		  (push `(deport-integer nil ,sap ,(+ offset i) 32
					 (logand (ash ,value ,shift)
						 #xFFFFFFFF))
			form))
		(error
		 "Unstructured aliens of size ~D and offset ~D not supported."
		 size offset)))))))

|#

;;; Alien-Access expert for Boolean  --  Internal 
;;;
;;;    A boolean is a single bit, represented in Lisp as T or NIL.
;;;
(define-alien-access (boolean) (type kind value form)
  (with-alien (sap)
	      (offset :unit 1)
	      (size :unit 1  :minimum 1)
    (if (eq kind :read)
	`(naturalize-boolean ,sap ,offset ,size ',form)
	`(deport-boolean ,sap ,offset ,size ,value ',form))))

;;; Alien-Access expert for single-floats
;;;
(define-alien-access (single-float) (type kind value)
  (with-alien (sap)
	      (offset)
	      (size :constant 1)
    (declare (ignore size))
    (if (eq kind :read)
	`(sap-ref-single ,sap ,offset)
	`(setf (sap-ref-single ,sap ,offset) ,value))))

;;; Alien-Access expert for double-floats
;;;
(define-alien-access (double-float) (type kind value)
  (with-alien (sap)
	      (offset)
	      (size :unit 64 :constant 1)
    (declare (ignore size))
    (if (eq kind :read)
	`(sap-ref-double ,sap ,offset)
	`(setf (sap-ref-double ,sap ,offset) ,value))))

;;; Alien-access expert for procedure objects.  These should be used
;;; with caution.
;;;
#+nil ; ### This will need work.
(define-alien-access (c-procedure) (type kind value)
  (with-alien (sap)
	      (offset)
	      (size :constant 2)
    (declare (ignore size))
    (if (eq kind :read)
	`(error "It is illegal to reference a c-procedure object.")
	`(%primitive set-c-procedure-pointer ,sap ,offset ,value))))


;;;; Strings accesses:

;;; Alien-Access expert for String  --  Internal
;;;
;;;    Read a Perq-String into a string or write a string out into
;;; a Perq-String.
;;;
(define-alien-access (simple-string perq-string) (type kind n-value)
  (with-alien (n-sap)
	      (n-offset :unit 8)
	      (size :unit 8
		    :minimum (1+ (cadr type)))
    (declare (ignore size))
    (if (eq kind :read)
	(let ((size (gensym))
	      (str (gensym)))
	  `(let* ((,size (sap-ref-8 ,n-sap ,n-offset))
		  (,str (make-string ,size)))
	     (%primitive byte-blt ,n-sap (1+ ,n-offset) ,str 0 ,size)
	     ,str))
	(let ((len (gensym))
	      (1+off (gensym)))
	  `(let ((,len (length (the simple-string
				    ,n-value)))
		 (,1+off (1+ ,n-offset)))
	     (check<= ,len ,(cadr type))
	     (setf (sap-ref-8 ,n-sap ,n-offset) ,len)
	     (%primitive byte-blt ,n-value 0 ,n-sap ,1+off
			 (+ ,1+off ,len)))))))


;;; Alien-Access expert for null terminated string  --  Internal
;;;
;;;    Read a null terminated string into a string or write a string out into
;;; a null terminated string.
;;;
(define-alien-access (simple-string null-terminated-string) (type kind n-value)
  (with-alien (n-sap)
	      (n-offset :unit 8)
	      (size :unit 8
		    :minimum (cadr type))
    (declare (ignore size))
    (if (eq kind :read)
	(let ((size (gensym))
	      (str (gensym))
	      (ptr (gensym))
	      (start (gensym)))
	  `(do* ((,start (sap+ ,n-sap ,n-offset))
		 (,ptr ,start (sap+ ,ptr 1)))
	       ((zerop (sap-ref-8 ,ptr 0))
		(let* ((,size (sap- ,ptr ,start))
		       (,str (make-string ,size)))
		  (declare (fixnum ,size)
			   (type simple-base-string ,str))
		  (copy-from-system-area ,start 0
					 ,str (* vm:vector-data-offset
						 vm:word-bits)
					 (* ,size vm:byte-bits))
		  ,str))
	     (declare (type system-area-pointer ,start ,ptr))))
	(let ((len (gensym)))
	  `(let ((,len (the fixnum (1+ (length (the simple-string
						    ,n-value))))))
	     (declare (fixnum ,len))
	     (check<= ,len ,(cadr type))
	     (copy-to-system-area ,n-value (* vm:vector-data-offset
					      vm:word-bits)
				  ,n-sap ,n-offset ,len))))))


;;;; Pointer alien access:

;;; Alien-Access expert for System-Area-Pointer  --  Internal
;;;
;;;    Get or store a pointer.  In the store case this is the same
;;; as Pointer.
;;;
(define-alien-access (system-area-pointer system-area-pointer alien)
  (type kind value)
  (with-alien (sap)
	      (offset)
	      (size :constant 1)
    (declare (ignore size))
    (if (eq kind :read)
	`(sap-ref-sap ,sap ,offset)
	`(setf (sap-ref-sap ,sap ,offset) ,value))))

;;; Alien-Access expert for (Alien <type> [<bits>])  --  Internal
;;;
;;;    Get or store an alien value.  On read, we cons up a value, getting
;;; the size of out the type.  On write, we just store the SAP.
;;;
(define-alien-access (alien) (type kind value)
  (with-alien (sap)
	      (offset)
	      (size :constant 1)
    (declare (ignore size))
    (unless (and (consp type) (consp (cdr type)))
      (error "Bad type for accessing as an Alien: ~S" type))
    (let ((atype (second type))
	  (size (third type)))
      (cond
       ((eq kind :read)
	(unless size
	  (error "Size not specified when reading alien type: ~S" type))
	(unless (and (integerp size) (>= size 0))
	  (error "Size is not a positive integer: ~S" type))
	`(make-alien-value
	  (sap-ref-sap ,sap ,offset)
	  0
	  ,size
	  ',atype))
       (t
	`(setf (sap-ref-sap ,sap ,offset)
	       (alien-value-sap ,value)))))))

;;; Alien-Access expert for (Pointer xxx)  --  Internal
;;;
;;;    We can't read pointers, and can only store pointers to unboxed
;;; things.
;;;
(define-alien-access (pointer pointer alien) (type kind value)
  (with-alien (sap)
	      (offset)
	      (size :constant 1)
    (declare (ignore size))
    (if (eq kind :read)
	`(error "Cannot reference pointer aliens")
	(let ((n-value (gensym)))
	  `(setf (sap-ref-sap ,sap ,offset)
		 (let ((,n-value ,value))
		   ,@(when (and (consp type) (consp (cdr type)))
		       `((declare (type ,(cadr type) ,n-value))))
		   (etypecase ,n-value
		     (null (int-sap 0))
		     (system-area-pointer ,n-value)
		     ((or simple-string
			  simple-bit-vector
			  (simple-array unsigned-byte (*)))
		      (%primitive c::vector-sap ,n-value)))))))))


;;;; Enumeration Alien access:

(compiler-let ((lisp::*bootstrap-defmacro* t))

;;; Defenumeration  --  Public
;;;
;;;    Cons up the from alist, keeping track of the minimum and maximum
;;; values, then decide whether to use a vector or alist to mapping.
;;;
(defmacro defenumeration (name &rest elements)
  "Defenumeration Name {{Element}+ | {(Element Value)}+}
  Define an enumeration for use with Alien-Access and the Enumeration
  Alien type."
  (let ((min nil)
	(max nil)
	(from-alist ()))
    (declare (list from-alist))
    (when (null elements)
      (error "An anumeration must contain at least one element."))
    (if (listp (car elements))
	(dolist (el elements)
	  (unless (listp el)
	    (error "Element value is not specified: ~S." el))
	  (push (cons (first el) (second el)) from-alist))
	(let ((num -1))
	  (dolist (el elements)
	    (push (cons el (incf num)) from-alist))))
    (do ((el from-alist (cdr el)))
	((null el))
      (let ((sym (caar el))
	    (val (cdar el)))
	(unless (keywordp sym)
	  (error "Enumeration element ~S is not a keyword." sym))
	(unless (integerp val)
	  (error "Element value ~S is not an integer." val))
	(unless (and max (> max val)) (setq max val))
	(unless (and min (< min val)) (setq min val))
	(when (rassoc val (cdr el))
	  (error "Element value ~S used more than once." val))
	(when (assoc sym (cdr el) :test #'eq)
	  (error "Enumeration element ~S used more than once." sym))))
    (let* ((signed (minusp min))
	   (to (intern (concatenate 'simple-string (string name)
				    "-TO-ENUMERATION-" (string (gensym)))))
	   (from (intern (concatenate 'simple-string (string name)
				      "-FROM-ENUMERATION-" (string (gensym)))))
	   (info (make-enumeration-info :signed signed
					:size (if signed
						  (1+ (max (integer-length min)
							   (integer-length max)))
						  (integer-length max))
					:from from  :to to))
	   to-thing)
      (cond
       ;;
       ;; If range is at least 20% dense, use vector mapping.  Crossover
       ;; point solely on basis of space would be 25%.  Vector mapping
       ;; is always faster, so give the benefit of the doubt.
       ((< 0.2 (/ (float (length from-alist)) (float (- max min))))
	;;
	;; If offset is small and ignorable, ignore it to save time.
	(when (< 0 min 10) (setq min 0))
	(setq to-thing (make-array (1+ (- max min))))
	(dolist (el from-alist)
	  (setf (svref to-thing (- (cdr el) min)) (car el)))
	(setf (enumeration-info-kind info) :vector)
	(setf (enumeration-info-offset info) (- min)))
       (t
	(setf (enumeration-info-kind info) :alist)
	(setq to-thing (mapcar #'(lambda (x) (cons (cdr x) (car x))) from-alist))))
      `(progn
	(eval-when (compile load eval)
	  (proclaim '(special ,to ,from))
	  (set ',to ',to-thing)
	  (set ',from ',from-alist))
	(eval-when ,*alien-eval-when*
	  (setf (info enumeration info ',name) ',info))
	',name))))

); compiler-let

;;; Enumeration-Error  --  Internal
;;;
;;;    Tell the luser what permissable values the enumeration has when she
;;; gives us something bogus.  We even give them a chance to specify
;;; something else.
;;;
(defun enumeration-error (alist)
  (loop
   (cerror "Prompt for a new value."
	   "Enumeration value is not one of the following: ~{~<~%  ~:;~S ~>~}"
	  (mapcar #'car alist))
   (write-string "New value: " *query-io*)
   (let* ((response (read *query-io*))
	  (res (cdr (assoc response alist :test #'eq))))
     (when res (return res)))))


(define-alien-access (enumeration) (type kind value form)
  (let* ((enum (cadr type))
	 (info (or (info enumeration info enum)
		   (error "~S is not a defined enumeration." enum)))
	 (signed (enumeration-info-signed info))
	 (to (enumeration-info-to info))
	 (from (enumeration-info-from info)))
    (with-alien (sap)
		(offset :unit nil)
		(size :unit nil
		      :minimum (enumeration-info-size info))
      (if (eq kind :read)
	  (ecase (enumeration-info-kind info)
	    (:vector
	     `(svref ,to
		     (+ ,(enumeration-info-offset info)
			(naturalize-integer ,signed ,sap ,offset ,size
					    ',form))))
	    (:alist
	     `(cdr (assoc (naturalize-integer ,signed ,sap ,offset ,size
					      ',form)
			  ,to))))
	  `(deport-integer
	    ,signed ,sap ,offset ,size
	    (or (cdr (assoc ,value ,from :test #'eq))
		(enumeration-error ,from))
	    ',form)))))

