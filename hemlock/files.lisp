;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Hemlock File manipulation functions.
;;; Written by Skef Wholey, Horribly Hacked by Rob MacLachlan.
;;;

(in-package "HEMLOCK-INTERNALS")

(export '(read-file write-file))



;;; Read-File:

(defun read-file (pathname mark)
  "Inserts the contents of the file named by Pathname at the Mark."
  (with-mark ((mark mark :left-inserting))
    (let* ((tn (truename pathname))
	   (name (namestring tn))
	   (alien ())
	   (size 0))
      (declare (fixnum size))
      (multiple-value-bind (fd err) (mach:unix-open name mach:o_rdonly 0)
	(if (not (null fd))
	    (multiple-value-bind (res dev ino mode nlnk uid gid rdev len)
				 (mach:unix-fstat fd)
	      (declare (ignore ino mode nlnk uid gid rdev))
	      (setq err ())
	      (if (null res)
		  (setq err dev)
		  (multiple-value-bind (gr addr)
				       (mach::vm_allocate lisp::*task-self*
							  0 len t)
		    (gr-error 'mach::vm_allocate gr 'read-file)
		    (setq alien (lisp::fixnum-to-sap addr))
		    (setq size len)
		    (multiple-value-bind
			(bytes err3)
			(mach:unix-read fd (lisp::fixnum-to-sap addr) len)
		      (if (or (null bytes) (not (eq len bytes)))
			  (setq err err3)))))
	      (mach:unix-close fd)))
	(if err (error "Reading file ~A, unix error ~A."
		       name (mach:get-unix-error-msg err)))
	(when (zerop size) (return-from read-file nil))
	(let* ((sap alien)
	       (first-line (mark-line mark))
	       (buffer (line-%buffer first-line))
	       (index (%primitive find-character sap 0 size #\newline)))
	  (modifying-buffer buffer)
	  (let* ((len (or index size))
		 (chars (make-string len)))
	    (%primitive byte-blt sap 0 chars 0 len)
	    (insert-string mark chars))
	  (when index
	    (insert-character mark #\newline)
	    (do* ((old-index (1+ (the fixnum index)) (1+ (the fixnum index)))
		  (index (%primitive find-character sap old-index size
				     #\newline)
			 (%primitive find-character sap old-index size
				     #\newline))
		  (number (+ (line-number first-line) line-increment)
			  (+ number line-increment))
		  (previous first-line))
		 ((not index)
		  (let* ((length (- size old-index))
			 (chars (make-string length))
			 (line (mark-line mark)))
		    (declare (fixnum length))
		    (%primitive byte-blt sap old-index chars 0 length)
		    (insert-string mark chars)
		    (setf (line-next previous) line)
		    (setf (line-previous line) previous)
		    (do ((line line (line-next line))
			 (number number (+ number line-increment)))
			((null line))
		      (declare (fixnum number))
		      (setf (line-number line) number))))
	      (declare (fixnum number old-index))
	      (let ((line (make-line
			   :previous previous
			   :%buffer buffer
			   :number number
			   :chars (%primitive sap+ sap old-index)
			   :buffered-p
			   (the fixnum (- (the fixnum index) old-index)))))
		(setf (line-next previous) line)
		(setq previous line))) nil))))))


;;; Hackish stuff for disgusting speed:

(defun read-buffered-line (line)
  (let* ((len (line-buffered-p line))
  	 (chars (make-string len)))
    (%primitive byte-blt (line-%chars line) 0 chars 0 len)
    (setf (line-buffered-p line) nil)
    (setf (line-chars line) chars)))



;;; Write-File:

(defun write-file (region pathname &key
			  (keep-backup (value ed::keep-backup-files))
			  access)
  "Writes the characters in the Region to the file named by Pathname.
   Region is written using a stream opened with :if-exists :rename-and-delete,
   but keep-backup, when supplied as non-nil, causes :rename to be supplied
   instead of :rename-and-delete.  Access is an implementation dependent value
   that is suitable for setting pathname's access or protection bits."
  (let ((if-exists-action (if keep-backup
			      :rename
			      :rename-and-delete)))
    (with-open-file (file pathname :direction :output :element-type 'string-char
			  :if-exists if-exists-action)
      (close-line)
      (fast-write-file region file))
    (when access
      (multiple-value-bind
	  (winp code)
	  ;; Must do a TRUENAME in case the file has never been written.
	  ;; It may have Common Lisp syntax that Unix can't handle.
	  ;; If this is ever moved to the beginning of this function to use
	  ;; Unix CREAT to create the file protected initially, they TRUENAME
	  ;; will signal an error, and LISP::PREDICT-NAME will have to be used.
	  (mach:unix-chmod (namestring (truename pathname)) access)
	(unless winp
	  (error "Could not set access code: ~S"
		 (mach:get-unix-error-msg code)))))))

(proclaim '(special vm_page_size))

(ext:def-c-variable "vm_page_size" int)

(defvar *vm-page-size* (system:alien-access vm_page_size)
  "Size, in bytes, of each VM page.")

(defun fast-write-file (region file)
  (let* ((start (region-start region))
	 (start-line (mark-line start))
	 (start-charpos (mark-charpos start))
	 (end (region-end region))
	 (end-line (mark-line end))
	 (end-charpos (mark-charpos end)))
    (if (eq start-line end-line)
      (write-string (line-chars start-line) file
		    :start start-charpos :end end-charpos)
      (let* ((first-length (- (line-length start-line) start-charpos))
	     (length (+ first-length end-charpos 1)))
	(do ((line (line-next start-line) (line-next line)))
	    ((eq line end-line))
	  (incf length (1+ (line-length line))))
	(let ((bytes (* *vm-page-size*
			(ceiling length *vm-page-size*))))
	  (system:gr-bind (address)
			  (mach:vm_allocate system:*task-self* 0 bytes t)
	    (unwind-protect
		(let ((sap (system:int-sap address)))
		  (macrolet ((chars (line)
				    `(if (line-buffered-p ,line)
				       (line-%chars ,line)
				       (line-chars ,line))))
		    (system:%primitive byte-blt
				       (chars start-line) start-charpos
				       sap 0 first-length)
		    (system:%primitive 8bit-system-set
				       sap first-length #\newline)
		    (let ((offset (1+ first-length)))
		      (do ((line (line-next start-line)
				 (line-next line)))
			  ((eq line end-line))
			(let ((end (+ offset (line-length line))))
			  (system:%primitive byte-blt
					     (chars line) 0
					     sap offset end)
			  (system:%primitive 8bit-system-set
					     sap end #\newline)
			  (setf offset (1+ end))))
		      (unless (zerop end-charpos)
			(system:%primitive byte-blt
					   (chars end-line) 0
					   sap offset
					   (+ offset end-charpos)))))
		  (multiple-value-bind
		      (okay errno)
		      (mach:unix-write (system:fd-stream-fd file)
				       sap 0 length)
		    (unless okay
		      (error "Could not write ~S: ~A"
			     file
			     (mach:get-unix-error-msg errno))))
		  (system:gr-call mach:vm_deallocate system:*task-self*
				  address bytes)))))))))
