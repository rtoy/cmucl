;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Spice Lisp routines to suspend a process and create a core file.
;;; 
;;; Written David B. McDonald.
;;;
;;;**********************************************************************
;;;
;;; To see the format of Spice Lisp core files look at the document
;;; prva:<slisp.docs>core.mss.
(in-package "LISP")

(in-package "EXTENSIONS")
(export '(*environment-list*))
(defvar *environment-list* nil)
(in-package "LISP")


(proclaim '(special *task-self*))

(defconstant save-block-size (* 64 1024)
  "Amount to write for each call to write.  This is due to RFS limitations.")

(defvar lisp-environment-list)
(defvar original-lisp-environment)

;;;; Global state:

(defun save (file)
  "Save the current lisp core image in a core file.  When it returns in
  the current process, the number of bytes written is returned.
  When the saved core image is resumed, Nil is returned."
  (declare (optimize (speed 3) (safety 0)))
  (format t "~&[Building saved core image: ")
  (finish-output)
  (let ((size-to-allocate (* (current-space-usage) 2)))
    (declare (fixnum size-to-allocate))
    (let* ((addr (int-sap (gr-call* mach::vm_allocate *task-self*
				    0 size-to-allocate t)))
	   (byte-size (%primitive save *current-alien-free-pointer*
				  NIL addr)))
      (cond ((null byte-size)
	     (mach::vm_deallocate *task-self* addr size-to-allocate)
	     (error "Save failed."))
	    ((eq byte-size T)
	     (dolist (f *before-save-initializations*) (funcall f))
	     (dolist (f *after-save-initializations*) (funcall f))
	     (reinit)
	     (setq original-lisp-environment lisp-environment-list)
	     (let ((result nil))
	       (dolist (ele lisp-environment-list
			    (setf *environment-list* result))
		 (let ((=pos (position #\= (the simple-string ele))))
		   ;;
		   ;; This is dubious since all the strings have an =.
		   ;; What if one doesn't?  What does that mean?
		   (when =pos
		     (push (cons (intern (string-upcase (subseq ele 0 =pos))
					 *keyword-package*)
				 (subseq ele (1+ =pos)))
			   result)))))
	     NIL)
	    (T
	     (format t "~D bytes.~%" byte-size)
	     (format t "Writing to file: ~A~%" file)
	     (finish-output)
	     (multiple-value-bind (fd err) (mach:unix-creat file #o644)
	       (if (null fd)
		   (error "Failed to open file ~A, unix error: ~A"
			  file (mach:get-unix-error-msg err)))
	       
	       (do ((left byte-size (- left save-block-size))
		    (index 0 (+ index save-block-size)))
		   ((< left save-block-size)
		    (when (> left 0)
		      (multiple-value-bind (res err)
					   (mach:unix-write fd addr index left)
			(if (null res)
			    (error "Failed to write file ~A, unix error: ~A"
				   file (mach:get-unix-error-msg err))))))
		 (declare (fixnum left index))
		 (multiple-value-bind (res err)
				      (mach:unix-write fd addr index
						       save-block-size)
		   (if (null res)
		       (error "Failed to write file ~A, unix error: ~A"
			      file (mach:get-unix-error-msg err)))))
	       (multiple-value-bind (res err) (mach:unix-close fd)
		 (if (null res)
		     (error "Failed to close file ~A, unix error: ~A"
			    file (mach:get-unix-error-msg err)))))
	     (format t "done.]~%")
	     (mach::vm_deallocate *task-self* addr size-to-allocate)
	     (finish-output)
	     byte-size)))))

(defun current-space-usage ()
  (declare (optimize (speed 3) (safety 0)))
  (do ((sum 0)
       (type 0 (1+ type)))
      ((> type %last-pointer-type) sum)
    (declare (fixnum type sum))
    (if (not (or (eq type %short-+-float-type) (eq type %short---float-type)))
	(multiple-value-bind (dyn stat ro) (space-usage type)
	  (declare (fixnum dyn stat ro))
	  (setq sum (+ sum dyn stat ro))))))
