;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Runtime support for dynamic statistic collecting.
;;; 
(in-package "C")

(eval-when (compile)
  (when *collect-dynamic-statistics*
    (error "Compiling this file with dynamic stat collection turn on would ~
    be a very bad idea.")))


(eval-when (compile load eval)
  (defparameter *count-adjustments*
    '((return-multiple 152)
      (tail-call-variable 88)
      (unwind 92)
      (throw 116)
      (allocate-vector 72)
      (sxhash-simple-string 248)
      (sxhash-simple-substring 264)
      (copy-to-system-area 1200)
      (copy-from-system-area 1200)
      (system-area-copy 1204)
      (bit-bash-copy 1412)
      (vm::generic-+ 72)
      (vm::generic-- 72)
      (vm::generic-* 184)
      (vm::generic-< 68)
      (vm::generic-> 68)
      (vm::generic-eql 80)
      (vm::generic-= 80)
      (vm::generic-/= 104)
      (%make-weak-pointer 60)
      (make-value-cell 56)
      (vm::make-funcallable-instance 76)
      (make-closure 76)
      (make-complex 60)
      (make-ratio 60)
      (%allocate-bignum 72)
      (make-structure 72)
      (cons 50))))


(defvar *count-adjustments-hashtable*
  (let ((result (make-hash-table :test 'eq)))
    (dolist (entry *count-adjustments*)
      (setf (gethash (car entry) result) (cadr entry)))
    result))


(defvar *vop-counts* nil)


(defun note-dyncount-info (info)
  (declare (type dyncount-info info))
  (let ((counts (dyncount-info-counts info))
	(vops (dyncount-info-vops info)))
    (dotimes (index (length counts))
      (let ((count (aref counts index)))
	(unless (zerop count)
	  (let* ((vop-info (svref vops index))
		 (length (length vop-info)))
	    (do ((i 0 (+ i 4)))
		((>= i length))
	      (let* ((name (svref vop-info i))
		     (adjustment
		      (gethash name *count-adjustments-hashtable* 0))
		     (entry (gethash name *vop-counts*))
		     (counts (* (svref vop-info (1+ i)) count))
		     (cost (* (+ (svref vop-info (+ i 2)) adjustment)
			      count))
		     (elsewhere-cost (* (svref vop-info (+ i 3)) count)))
		(unless entry
		  (setf entry (make-array 4 :initial-element 0))
		  (setf (svref entry 0) name)
		  (setf (gethash name *vop-counts*) entry))
		(incf (svref entry 1) counts)
		(incf (svref entry 2) cost)
		(incf (svref entry 3) elsewhere-cost)))))))))


(defun clear-dyncount-info (info)
  (declare (type dyncount-info info))
  (fill (dyncount-info-counts info) 0)
  nil)


(defun find-info-for (function)
  (declare (type function function))
  (let* ((function (%primitive closure-function function))
	 (component (di::function-code-header function)))
    (do ((end (get-header-data component))
	 (i vm:code-constants-offset (1+ i)))
	((= end i))
      (let ((constant (code-header-ref component i)))
	(when (dyncount-info-p constant)
	  (return constant))))))

#+nil
(defun find-dyncount-info ()
  (declare (inline vm::map-allocated-objects)
	   (optimize (speed 3) (safety 0)))
  (let ((results nil))
    (dolist (space '(:read-only :static :dynamic))
      (vm::map-allocated-objects
       #'(lambda (object type-code size)
	   (declare (ignore type-code size))
	   (when (dyncount-info-p object)
	     (push object results)))
       space))
    (setf *all-dyncount-infos* results)
    (length results)))


(defun generate-report (cut-off sort-by)
  (let ((total-bytes 0)
	(list nil))
    (maphash #'(lambda (key value)
		 (push value list)
		 (unless (eq key 'c:count-me)
		   (incf total-bytes (svref value 2))))
	     *vop-counts*)
    (format t "~30<Vop~>  ~13<Count~> ~17<Cost~> ~5:@<Percent~>~%")
    (let ((total-bytes (coerce total-bytes 'double-float))
	  (entries (sort list #'>
			 :key #'(lambda (x)
				  (svref x
					 (ecase sort-by
					   (:count 1)
					   (:cost 2)))))))
      (dolist (entry entries)
	(when (and cut-off (minusp (decf cut-off)))
	  (return))
	(unless (eq (svref entry 0) 'c:count-me)
	  (format t "~30<~A~>: ~13:D ~17:D ~4,1,2F%~%"
		  (symbol-name (svref entry 0))
		  (svref entry 1)
		  (svref entry 2)
		  (/ (coerce (svref entry 2) 'double-float)
		     total-bytes))))))
  (values))

(defun report-dynamic-vop-counts (&key spaces (cut-off 15) (sort-by :cost))
  (let ((*vop-counts* (make-hash-table :test #'eq)))
    (without-gcing
     (dolist (space (or spaces '(:read-only :static :dynamic)))
       (format t "Scanning ~S space...~%" space)
       (locally
	(declare (optimize (speed 3) (safety 0)))
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (dyncount-info-p object)
	       (note-dyncount-info object)))
	 space))))
    (generate-report cut-off sort-by)))


(defun clear-dynamic-vop-counts (&rest spaces)
  (without-gcing
   (dolist (space (or spaces '(:read-only :static :dynamic)))
     (format t "Scanning ~S space...~%" space)
     (locally
      (declare (optimize (speed 3) (safety 0)))
      (vm::map-allocated-objects
       #'(lambda (object type-code size)
	   (declare (ignore type-code size))
	   (when (dyncount-info-p object)
	     (clear-dyncount-info object)))
       space)))))


(defun check-dynamic-vop-counts (&rest spaces)
  (without-gcing
   (dolist (space (or spaces '(:read-only :static :dynamic)))
     (format t "Scanning ~S space...~%" space)
     (locally
      (declare (optimize (speed 3) (safety 0)))
      (vm::map-allocated-objects
       #'(lambda (object type-code size)
	   (when (dyncount-info-p object)
	     (assert (= (length (dyncount-info-counts object))
			(length (dyncount-info-vops object))))))
       space)))))
