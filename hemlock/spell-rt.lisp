;;; -*- Log: hemlock.log; Package: Spell -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles
;;;
;;; This file contains system dependent primitives for the spelling checking/
;;; correcting code in Spell-Correct.Lisp, Spell-Augment.Lisp, and
;;; Spell-Build.Lisp.

(in-package "SPELL" :use '("LISP" "EXTENSIONS" "SYSTEM"))


;;;; System Area Referencing and Setting

(eval-when (compile eval)

;;; MAKE-SAP returns pointers that *dictionary*, *descriptors*, and
;;; *string-table* are bound to.  Address is in the system area.
;;;
(defmacro make-sap (address)
  `(lisp::fixnum-to-sap ,address))

(defmacro system-address (sap)
  `(lisp::sap-to-fixnum ,sap))


(defmacro allocate-bytes (count)
  `(make-sap (lisp::do-validate 0 ,count -1)))

(defmacro deallocate-bytes (address byte-count)
  `(mach::vm_deallocate lisp::*task-self* ,address ,byte-count))


(defmacro sapref (sap offset)
  `(%primitive 16bit-system-ref ,sap ,offset))

(defsetf sapref (sap offset) (value)
  `(%primitive 16bit-system-set ,sap ,offset ,value))


(defmacro sap-replace (dst-string src-string src-start dst-start dst-end)
  `(%primitive byte-blt ,src-string ,src-start ,dst-string ,dst-start ,dst-end))

(defmacro string-sapref (sap index)
  `(%primitive 8bit-system-ref ,sap ,index))



;;;; Primitive String Hashing

;;; STRING-HASH employs the instruction SXHASH-SIMPLE-SUBSTRING which takes
;;; an end argument, so we do not have to use SXHASH.  SXHASH would mean
;;; doing a SUBSEQ of entry.
;;;
(defmacro string-hash (string length)
  `(%primitive sxhash-simple-substring ,string ,length))

) ;eval-when



;;;; Binary Dictionary File I/O

(defun open-dictionary (f)
  (multiple-value-bind (filename existsp)
		       (lisp::predict-name f :for-input)
    (unless existsp (error "Cannot find dictionary -- ~S." filename))
    (multiple-value-bind (fd err)
			 (mach:unix-open filename mach:o_rdonly 0)
      (unless fd
	(error "Opening ~S failed: ~A." filename err))
      (multiple-value-bind (winp dev-or-err) (mach:unix-fstat fd)
	(unless winp (error "Opening ~S failed: ~A." filename dev-or-err))
	fd))))

(defun close-dictionary (fd)
  (mach:unix-close fd))

(defun read-dictionary-structure (fd bytes)
  (let* ((structure (allocate-bytes bytes)))
    (multiple-value-bind (read-bytes err)
			 (mach:unix-read fd structure bytes)
      (when (or (null read-bytes) (not (= bytes read-bytes)))
	(deallocate-bytes (system-address structure) bytes)
	(error "Reading dictionary structure failed: ~A." err))
      structure)))
