;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/dyncount.lisp,v 1.8 1993/08/04 15:54:15 ram Exp $")
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

;;; FUNCTION-CYCLES -- External.
;;;
;;; When passed a function FUNCTION-CYCLES returns the number of cycles the
;;; function has executed.  :clear t clears the costs.
;;;
(defun function-cycles (function &key (clear nil) (verbose nil))
  (let ((info (find-info-for function)))
    (when info
      (when verbose
	(let* ((object (di::compiled-debug-function-component
			(di::function-debug-function function)))
	       (map (di::get-debug-info-function-map
		     (kernel:code-header-ref object 3)))
	       (i 0))
	  (map 'list
	       #'(lambda (e)
		   (when (compiled-debug-function-p e)
		     (di:do-debug-function-blocks
			 (ee (di::make-compiled-debug-function e object))
		       (let ((cl (di::compiled-debug-block-code-locations ee)))
			 (cond ((di:debug-block-elsewhere-p ee)
				#+nil (format t "Elsewhere~&"))
			       (t
				(format t "Kind: ~S, Cost: ~S~&"
					(compiled-debug-function-kind e)
					(* (aref (dyncount-info-costs info) i)
					   (aref (dyncount-info-counts info) i)))
				(incf i)
				(debug::print-code-location-source-form
				 (svref cl 0) 0)
				(format t "~&")))))))
	       map)))
      (format t "~S~&" (dyncount-total-cost info))
      (when clear
	(clear-dyncount-info info)))))

;;; GET-SPACE-LIST -- Internal.
;;;
;;;
(defun get-space-list (spaces clear)
  (locally
   (declare (optimize (speed 3) (safety 0))
	    (inline vm::map-allocated-objects))
   (without-gcing
    (let ((list nil))
      (dolist (space spaces)
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (kernel:code-component-p object)
	       (let ((info (kernel:code-header-ref object 5))
		     (j 0)
		     (sum 0)
		     (alist))
		 (declare (fixnum j))
		 (when (dyncount-info-p info)
		   (let* ((debug-info (kernel:code-header-ref object 3))
			  (map (di::get-debug-info-function-map debug-info)))
		     (declare (vector map))
		     (dotimes (i (length map))
		       (declare (fixnum i))
		       (let ((e (svref map i)))
			 (when (compiled-debug-function-p e)
			   (let ((fn (di::make-compiled-debug-function
				      e object)))
			     (di:do-debug-function-blocks (blk fn)
			       (unless (di:debug-block-elsewhere-p blk)
				 (incf sum
				       (* (aref (dyncount-info-costs info) j)
					  (aref (dyncount-info-counts info) j)))
				 (incf j)))
			     (let ((a (find (di:debug-function-name fn)
					    alist :key #'car)))
			       (cond (a (incf (third a) sum))
				     (t
				      (push (list (di:debug-function-name fn)
						  (compiled-debug-info-package
						   debug-info)
						   sum)
					    alist)))
			       (setf sum 0)))))))
		   (when clear
		     (clear-dyncount-info info)))
		 (dolist (e alist)
		   (push e list)))))
	 space))
      list))))

;;; GET-STATS -- External.
;;;
;;; Returns the cycles of all the functions in the spaces.
;;;
(defun get-stats (&key (spaces '(:dynamic)) (clear nil) (top-n 10) (cost 0))
  (let ((list (stable-sort (sort (get-space-list spaces clear)
				 #'> :key #'third) #'string< :key #'second))
	(package-name "")
	(i 0)
	(other 0))
    (dolist (e list)
      (unless (string= package-name (second e))
	(setf package-name (second e))
	(when (> other cost)
	  (format t " ~10:D: Other~&" other))
	(setf i 0)
	(setf other 0)
	(when (> (third e) cost)
	  (format t "Package: ~A~&" package-name)))
      (cond ((< i top-n)
	     (when (> (third e) cost)
	       (format t " ~10:D: ~S~&" (third e) (first e))))
	    (t
	     (incf other (third e))))
      (incf i))
    (when (> other cost)
      (format t " ~10:D: Other~&" other))))
