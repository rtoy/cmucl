;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/dyncount.lisp,v 1.7 1993/03/12 15:34:04 hallgren Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains support for collecting dynamic vop statistics.
;;; 
(in-package "C")

(export '(*collect-dynamic-statistics* dyncount-info-counts
				       dyncount-info-costs))

(export '(count-me))


(defvar *collect-dynamic-statistics* nil
  "When T, emit extra code to collect dynamic statistics about vop usages.")

(defvar *dynamic-counts-tn* nil
  "Holds the TN for the counts vector.")


(defstruct (dyncount-info
	    (:print-function %print-dyncount-info)
	    (:make-load-form-fun :just-dump-it-normally))
  for
  (costs (required-argument) :type (simple-array (unsigned-byte 32) (*)))
  (counts (required-argument) :type (simple-array (unsigned-byte 32) (*))))


(defprinter dyncount-info
  for
  costs
  counts)

;;; FIND-INFO-FOR  --  Interface
;;;
;;;    Return the DYNCOUNT-INFO for FUNCTION.
;;;
(defun find-info-for (function)
  (declare (type function function))
  (let* ((function (%closure-function function))
	 (component (di::function-code-header function)))
    (do ((end (get-header-data component))
	 (i vm:code-constants-offset (1+ i)))
	((= end i))
      (let ((constant (code-header-ref component i)))
	(when (dyncount-info-p constant)
	  (return constant))))))

;;
(defun clear-dyncount-info (info)
  (declare (type dyncount-info info))
  (declare (optimize (speed 3) (safety 0)))
  (let ((counts (dyncount-info-counts info)))
    (dotimes (i (length counts))
      (setf (aref counts i) 0))))

;;
(defun dyncount-total-cost (info)
  (let ((costs (dyncount-info-costs info))
	(counts (dyncount-info-counts info))
	(sum 0))
    (dotimes (i (length costs))
      (incf sum (* (aref costs i) (aref counts i))))
    sum))

;; 
(defun get-stats (&optional (spaces '(:dynamic)) &key (clear nil))
  (locally
      (declare (optimize (speed 3) (safety 0))
	       (inline vm::map-allocated-objects))
    (without-gcing
      (dolist (space spaces)
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (kernel:code-component-p object)
	       (let ((info (kernel:code-header-ref object 5)))
		 (when (dyncount-info-p info)
		   (let ((debug-info (kernel:code-header-ref object 3)))
		     (format t "Function: ~S  Cost: ~S~&XS"
			     (compiled-debug-info-name debug-info)
			     (dyncount-total-cost info)))
		   (when clear
		     (clear-dyncount-info info))))))
	 space))))
  #+nil
  (let ((counts (make-hash-table :test #'equal)))
    (do-hash (k v (backend-template-names *backend*))
      (declare (ignore v))
      (let ((stats (get k 'vop-stats)))
	(when stats
	  (setf (gethash (symbol-name k) counts) stats)
	  (when clear
	    (remprop k 'vop-stats)))))
    counts))


#|
(defun setup-dynamic-count-info (component)
  (let* ((info (ir2-component-dyncount-info (component-info component)))
	 (vops (dyncount-info-vops info)))
    (when (producing-fasl-file)
      (fasl-validate-structure info *compile-object*))
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
    #+nil
    (assem:count-instructions
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
     *elsewhere*))
  (undefined-value))
|#
