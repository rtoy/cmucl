;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; This file contains support for collecting dynamic vop statistics.
;;; 
(in-package "C")

(export '(*collect-dynamic-statistics*))

(export '(count-me))


(defvar *collect-dynamic-statistics* nil
  "When T, emit extra code to collect dynamic statistics about vop usages.")

(defvar *dynamic-counts-tn* nil
  "Holds the TN for the counts vector.")


(defstruct (dyncount-info
	    (:print-function %print-dyncount-info))
  for
  (counts (required-argument) :type (simple-array (unsigned-byte 32) (*)))
  (vops (required-argument) :type simple-vector))


(defprinter dyncount-info
  for
  counts
  vops)

(defun setup-dynamic-count-info (component)
  (let* ((info (ir2-component-dyncount-info (component-info component)))
	 (vops (dyncount-info-vops info)))
    (do-ir2-blocks (block component)
      (when (ir2-block-start-vop block)
	(let* ((1block (ir2-block-block block))
	       (index (1- (block-number 1block)))
	       (counts (svref vops index)))
	  (do ((vop (ir2-block-start-vop block) (vop-next vop)))
	      ((null vop))
	    (let* ((vop-name (vop-info-name (vop-info vop)))
		   (entry (assoc vop-name counts :test #'eq)))
	      (if entry
		  (incf (cdr entry))
		  (setf counts (acons vop-name 1 counts)))))
	  (setf (svref vops index) counts))))))
