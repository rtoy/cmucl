;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Functions to implement bit bashing.
;;;
;;; Written by William Lott.
;;;

(in-package "LISP")



;;;; Constants and Types.


(eval-when (compile load eval)

(defconstant unit-bits 32
  "The number of bits to process at a time.")

(defconstant max-bits (ash most-positive-fixnum -2)
  "The maximum number of bits that can be delt with during a single call.")


(deftype unit ()
  `(unsigned-byte ,unit-bits))

(deftype offset ()
  `(integer 0 ,max-bits))

(deftype bit-offset ()
  `(integer 0 (,unit-bits)))

(deftype word-offset ()
  `(integer 0 (,(ceiling max-bits unit-bits))))


); eval-when



;;;; Macros for generating bit-bashing routines.


(eval-when (compile eval)

(defmacro end-bits (count)
  "Returns the byte spec for COUNT bits at the end of a word, i.e. the bits
  at the largest address."
  (ecase vm:target-byte-order
    (:little-endian `(byte ,count (- unit-bits ,count)))
    (:big-endian `(byte ,count 0))))

(defmacro start-bits (count)
  "Returns the byte spec for COUNT bits at the start of a word, i.e. the bits
  at the smallest address."
  (ecase vm:target-byte-order
    (:little-endian `(byte ,count 0))
    (:big-endian `(byte ,count (- unit-bits ,count)))))

(defmacro middle-bits (count where)
  "Return the byte spec for COUNT bits starting at bit WHERE.  WHERE of zero
  corresponds to the start of the word (lowest address) and WHERE of
  unit-bits corresponds to the end of the word (highest address).  In other
  words, act like :little-endian"
  (ecase vm:target-byte-order
    (:little-endian `(byte ,count ,where))
    (:big-endian `(byte ,count (- unit-bits ,where ,count)))))


(defmacro bit-bash-bindings (&body guts)
  `(let* ((final-bits (mod (+ len dst-bit-offset) unit-bits))
	  (interior (floor (- len final-bits) unit-bits)))
     (declare (type bit-offset final-bits)
	      (type word-offset interior))
     ,@guts))

(defmacro bind-srcs ((kind incf-p ref-fn) &body body)
  (ecase kind
    (:constant `(progn
		  ,@body
		  ,@(when incf-p
		      `((incf dst-word-offset)))))
    (:unary `(let ((next-1 (,ref-fn src-1 src-1-word-offset)))
	       (declare (type unit next-1))
	       ,@body
	       ,@(when incf-p
		   '((incf dst-word-offset)
		     (incf src-1-word-offset)))))
    (:binary `(let ((next-1 (,ref-fn src-1 src-1-word-offset))
		    (next-2 (,ref-fn src-2 src-2-word-offset)))
		(declare (type unit next-1 next-2))
		,@body
		,@(when incf-p
		   '((incf dst-word-offset)
		     (incf src-1-word-offset)
		     (incf src-2-word-offset)))))))


(defmacro bit-bash-loop (kind ref-fn set-fn function &optional update)
  `(progn
     (unless (zerop dst-bit-offset)
       (bind-srcs (,kind t ,ref-fn)
	 (setf (,set-fn dst dst-word-offset)
	       (the unit
		    (dpb ,function
			 (end-bits (the (integer (0) (#.unit-bits))
					(- unit-bits dst-bit-offset)))
			 (,set-fn dst dst-word-offset))))
	 ,update))
     (dotimes (count interior)
       (declare (type word-offset count))
       (bind-srcs (,kind t ,ref-fn)
	 (setf (,set-fn dst dst-word-offset) ,function)
	 ,update))
     (unless (zerop final-bits)
       (bind-srcs (,kind nil ,ref-fn)
	 (setf (,set-fn dst dst-word-offset)
	       (the unit
		    (dpb ,function
			 (start-bits (the (integer (0) (#.unit-bits))
					  final-bits))
			 (,set-fn dst dst-word-offset))))))))


(defun pick-args (op kind arg1 arg2)
  (ecase kind
    (:constant
     op)
    (:unary
     (list op arg1))
    (:binary
     (list op arg1 arg2))))

(defmacro def-bit-basher (name op &optional (kind :binary) (ref-fn '%raw-bits)
			       (set-fn ref-fn))
  (let ((form
	 `(cond
	   ((<= (+ dst-bit-offset len) unit-bits)
	    ;; It's narrow.
	    (setf (,set-fn dst dst-word-offset)
		  (the unit
		       (dpb (the unit
				 ,(pick-args op kind
				    `(the unit
					  (ldb (middle-bits (the bit-offset len)
							    src-1-bit-offset)
					       (,ref-fn src-1
							src-1-word-offset)))
				    `(the unit
					  (ldb (middle-bits (the bit-offset len)
							    src-2-bit-offset)
					       (,ref-fn src-2
							src-2-word-offset)))))
			    (middle-bits len dst-bit-offset)
			    (,set-fn dst dst-word-offset)))))
	   (,(ecase kind
	       (:constant t)
	       (:unary '(= src-1-bit-offset dst-bit-offset))
	       (:binary '(= src-1-bit-offset src-2-bit-offset dst-bit-offset)))
	    ;; Everything is aligned evenly.
	    (bit-bash-bindings
	     (bit-bash-loop ,kind ,ref-fn ,set-fn
	       ,(pick-args op kind 'next-1 'next-2))))
	   ,@(when (eq kind :binary)
	       `(((= src-1-bit-offset dst-bit-offset)
		  ;; Src1 and the destination are aligned, but src2 is not.
		  (bit-bash-bindings
		   (when (> dst-bit-offset src-2-bit-offset)
		     (decf src-2-word-offset))
		   (let* ((src-2-shift
			   (mod (- dst-offset src-2-offset) unit-bits))
			  (prev-2 (,ref-fn src-2 src-2-word-offset)))
		     (declare (type bit-offset src-2-shift))
		     (declare (type unit prev-2))
		     (incf src-2-word-offset)
		     (bit-bash-loop ,kind ,ref-fn ,set-fn
		       (,op next-1 (merge-bits src-2-shift prev-2 next-2))
		       (setf prev-2 next-2)))))
		 ((= src-2-bit-offset
		     dst-bit-offset)
		  ;; Src2 and the destination are aligned, but src1 is not.
		  (bit-bash-bindings
		   (when (> dst-bit-offset src-1-bit-offset)
		     (decf src-1-word-offset))
		   (let* ((src-1-shift
			   (mod (- dst-offset src-1-offset) unit-bits))
			  (prev-1 (,ref-fn src-1 src-1-word-offset)))
		     (declare (type bit-offset src-1-shift))
		     (declare (type unit prev-1))
		     (incf src-1-word-offset)
		     (bit-bash-loop ,kind ,ref-fn ,set-fn
		       (,op (merge-bits src-1-shift prev-1 next-1) next-2)
		       (setf prev-1 next-1)))))))
	   ,@(unless (eq kind :constant)
	       `((t
		  ;; Nothing is aligned. Ack.
		  (bit-bash-bindings
		   (when (> dst-bit-offset src-1-bit-offset)
		     (decf src-1-word-offset))
		   ,@(when (eq kind :binary)
		       '((when (> dst-bit-offset src-2-bit-offset)
			   (decf src-2-word-offset))))
		   (let* ((src-1-shift
			   (mod (- dst-offset src-1-offset) unit-bits))
			  (prev-1 (,ref-fn src-1 src-1-word-offset))
			  ,@(when (eq kind :binary)
			      `((src-2-shift
				 (mod (- dst-offset src-2-offset) unit-bits))
				(prev-2 (,ref-fn src-2 src-2-word-offset)))))
		     (declare (type bit-offset src-1-shift
				    ,@(when (eq kind :binary)
					'(src-2-shift)))
			      (type unit prev-1
				    ,@(when (eq kind :binary) '(prev-2))))
		     (incf src-1-word-offset)
		     ,@(when (eq kind :binary)
			 '((incf src-2-word-offset)))
		     (bit-bash-loop ,kind ,ref-fn ,set-fn
		       ,(pick-args op kind
				   '(merge-bits src-1-shift prev-1 next-1)
				   '(merge-bits src-2-shift prev-2 next-2))
		       (setf prev-1 next-1
			     ,@(when (eq kind :binary)
				 '(prev-2 next-2)))))))))))
	(function-args '(len))
	(function-decls '(len)))
    (dolist (arg (ecase kind
		   (:constant '(dst))
		   (:unary '(dst src-1))
		   (:binary '(dst src-2 src-1))))
      (let* ((name (string arg))
	     (offset
	      (intern (concatenate 'simple-string name "-OFFSET")))
	     (bit-offset
	      (intern (concatenate 'simple-string name "-BIT-OFFSET")))
	     (word-offset
	      (intern (concatenate 'simple-string name "-WORD-OFFSET"))))
	(setf form
	      `(multiple-value-bind (,word-offset ,bit-offset)
				    (floor ,offset unit-bits)
		 (declare (type word-offset ,word-offset)
			  (type bit-offset ,bit-offset))
		 ,form))
	(push offset function-args)
	(push offset function-decls)
	(push arg function-args)))
    `(defun ,name ,function-args
       (declare (type offset ,@function-decls))
       ,form)))

); eval when


;;;; Support routines.

;;; These are compiler primitives.

(defun %raw-bits (object offset)
  (declare (type index offset))
  (%raw-bits object offset))

(defun (setf %raw-bits) (object offset value)
  (declare (type index offset)
	   (type unit value))
  (setf (%raw-bits object offset) value))

(defun merge-bits (shift prev next)
  "Return (ldb (byte 32 0) (ash (logior (ash prev 32) next) (- shift))) but
  stay out of bignum land."
  (declare (type bit-offset shift)
	   (type unit prev next))
  (merge-bits shift prev next))


;;; These are not supported as primitives.

#|

(proclaim '(inline 32bit-logical-eqv 32bit-logical-nand 32bit-logical-andc1
		   32bit-logical-andc2 32bit-logical-orc1 32bit-logical-orc2))

(defun 32bit-logical-eqv (x y)
  (32bit-logical-not (32bit-logical-xor x y)))

(defun 32bit-logical-nand (x y)
  (32bit-logical-not (32bit-logical-and x y)))

(defun 32bit-logical-andc1 (x y)
  (32bit-logical-and (32bit-logical-not x) y))

(defun 32bit-logical-andc2 (x y)
  (32bit-logical-and x (32bit-logical-not y)))

(defun 32bit-logical-orc1 (x y)
  (32bit-logical-or (32bit-logical-not x) y))

(defun 32bit-logical-orc2 (x y)
  (32bit-logical-or x (32bit-logical-not y)))



;;;; The actual bashers.

(proclaim '(optimize (speed 3) (safety 0)))

(def-bit-basher bit-bash-clear 0 :constant)
(def-bit-basher bit-bash-set (1- (ash 1 unit-bits)) :constant)

(def-bit-basher bit-bash-not 32bit-logical-not :unary)

(def-bit-basher bit-bash-and 32bit-logical-and)
(def-bit-basher bit-bash-ior 32bit-logical-or)
(def-bit-basher bit-bash-xor 32bit-logical-xor)
(def-bit-basher bit-bash-eqv 32bit-logical-eqv)
(def-bit-basher bit-bash-nand 32bit-logical-nand)
(def-bit-basher bit-bash-nor 32bit-logical-nor)
(def-bit-basher bit-bash-andc1 32bit-logical-andc1)
(def-bit-basher bit-bash-andc2 32bit-logical-andc2)
(def-bit-basher bit-bash-orc1 32bit-logical-orc1)
(def-bit-basher bit-bash-orc2 32bit-logical-orc2)


;;; Sap-ref-32 can be used to index into SAP objects.

(def-bit-basher system-area-clear 0 :constant sap-ref-32)

|#


;;;; Copy routines.

;;; These are written in assembler.

(defun copy-to-system-area (src src-offset dst dst-offset length)
  (declare (type (simple-unboxed-array (*)) src)
	   (type system-area-pointer dst)
	   (type index src-offset dst-offset length))
  (copy-to-system-area src src-offset dst dst-offset length))

(defun copy-from-system-area (src src-offset dst dst-offset length)
  (declare (type system-area-pointer src)
	   (type (simple-unboxed-array (*)) dst)
	   (type index src-offset dst-offset length))
  (copy-from-system-area src src-offset dst dst-offset length))

(defun system-area-copy (src src-offset dst dst-offset length)
  (declare (type system-area-pointer src dst)
	   (type index src-offset dst-offset length))
  (system-area-copy src src-offset dst dst-offset length))

(defun bit-bash-copy (src src-offset dst dst-offset length)
  (declare (type (simple-unboxed-array (*)) src dst)
	   (type index src-offset dst-offset length))
  (bit-bash-copy src src-offset dst dst-offset length))
