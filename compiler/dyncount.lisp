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
      (let* ((start-vop (ir2-block-start-vop block))
	     (1block (ir2-block-block block))
	     (block-number (block-number 1block)))
	(when (and start-vop block-number)
	  (let* ((index (1- block-number))
		 (counts (svref vops index))
		 (length (length counts)))
	    (do ((vop start-vop (vop-next vop)))
		((null vop))
	      (let ((vop-name (vop-info-name (vop-info vop))))
		(do ((i 0 (+ i 4)))
		    ((or (>= i length) (eq (svref counts i) vop-name))
		     (when (>= i length)
		       (incf length 4)
		       (let ((new-counts
			      (make-array length :initial-element 0)))
			 (when counts
			   (replace new-counts counts))
			 (setf counts new-counts))
		       (setf (svref counts i) vop-name))
		     (incf (svref counts (1+ i)))))))
	    (setf (svref vops index) counts)))))
    (count-instructions
     #'(lambda (vop bytes elsewherep)
	 (let ((block-number (block-number (ir2-block-block (vop-block vop)))))
	   (when block-number
	     (let* ((name (vop-info-name (vop-info vop)))
		    (counts (svref vops (1- block-number)))
		    (length (length counts)))
	       (do ((i 0 (+ i 4)))
		   ((>= i length)
		    (error "VOP ~S didn't exist earlier!~%  counts=~S"
			   name counts))
		 (when (eq (svref counts i) name)
		   (incf (svref counts (+ i (if elsewherep 3 2))) bytes)
		   (return)))))))
     *code-segment*
     *elsewhere*)
    (undefined-value)))
