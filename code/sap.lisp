;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sap.lisp,v 1.1 1991/11/18 10:43:04 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file holds the support for System Area Pointers (saps).
;;;
(in-package "SYSTEM")

(export '(system-area-pointer sap-ref-8 sap-ref-16 sap-ref-32 sap-ref-sap
	  signed-sap-ref-8 signed-sap-ref-16 signed-sap-ref-32
	  sap+ sap- sap< sap<= sap= sap>= sap>))

(import '(%set-sap-ref-8 %set-sap-ref-16 %set-sap-ref-32 %set-sap-ref-sap
	  %set-sap-ref-single %set-sap-ref-double)
	"C")


;;;; Primitive SAP operations.

(defun sap< (x y)
  "Return T iff the SAP X points to a smaller address then the SAP Y."
  (declare (type system-area-pointer x y))
  (sap< x y))

(defun sap<= (x y)
  "Return T iff the SAP X points to a smaller or the same address as
   the SAP Y."
  (declare (type system-area-pointer x y))
  (sap<= x y))

(defun sap= (x y)
  "Return T iff the SAP X points to the same address as the SAP Y."
  (declare (type system-area-pointer x y))
  (sap= x y))

(defun sap>= (x y)
  "Return T iff the SAP X points to a larger or the same address as
   the SAP Y."
  (declare (type system-area-pointer x y))
  (sap>= x y))

(defun sap> (x y)
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



;;;; System memory allocation.

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


