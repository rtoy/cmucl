;;; -*- Log: code.log; Package: inspect -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Tty interface for INSPECT.
;;;
;;; Written by Blaine Burks

(in-package "INSPECT")

;;; The tty inspector views LISP objects as being composed of parts.  A list,
;;; for example, would be divided into it's members, and a structure into its
;;; slots.  These parts are stored in a list.  The first two elements of this
;;; list are for bookkeeping.  The first element is a preamble string that will
;;; be displayed before the object.  The second element is a boolean value that
;;; indicates whether a label will be printed in front of a value, or just the
;;; value.  Symbols and structures need to display both a slot name and a
;;; value, while lists, vectors, and atoms need only display a value.  If the
;;; second member of a parts list is t, then the third and successive members
;;; must be an association list of slot names and values.  When the second slot
;;; is nil, the third and successive slots must be the parts of an object.
;;;

;;; *tty-object-stack* is an assoc list of objects to their parts.  
;;;
(defvar *tty-object-stack* ())

(proclaim '(inline numbered-parts-p))
(defun numbered-parts-p (parts)
  (second parts))

(defconstant parts-offset 2)

(defun nth-parts (parts n)
  (if (numbered-parts-p parts)
      (cdr (nth (+ n parts-offset) parts))
      (nth (+ n parts-offset) parts)))

(defun tty-inspect (object)
  (unwind-protect
      (input-loop object (describe-parts object) *standard-output*)
    (setf *tty-object-stack* nil)))

;;; When %illegal-object% occurs in a parts list, it indicates that that slot
;;; is unbound.
(defvar %illegal-object% (cons nil nil))

(defun input-loop (object parts s)
  (tty-display-object parts s)
  (loop
    (format s "~%> ")
    (let ((command (read))
	  (parts-len-2 (- (length parts) 2)))
      (typecase command
	(number
	 (cond ((< -1 command parts-len-2)
		(cond ((eq (nth-parts parts command) %illegal-object%)
		       (format s "~%That slot is unbound.~%"))
		      (t
		       (push (cons object parts) *tty-object-stack*)
		       (setf object (nth-parts parts command))
		       (setf parts (describe-parts object))
		       (tty-display-object parts s))))
	       (t
		(format s "~%Enter a VALID number.~%"))))
	(symbol
	 (case (find-symbol (symbol-name command) (find-package "KEYWORD"))
	   ((:q :e)
	    (return object))
	   (:u
	    (cond (*tty-object-stack*
		   (setf object (caar *tty-object-stack*))
		   (setf parts (cdar *tty-object-stack*))
		   (pop *tty-object-stack*)
		   (tty-display-object parts s))
		  (t (format s "~%Bottom of Stack.~%"))))
	   (:r
	    (setf parts (describe-parts object))
	    (tty-display-object parts s))
	   (:d
	    (tty-display-object parts s))
	   ((:h :? :help)
	    (show-help s))
	   (t
	    (format s "~%Invalid command.  Type H for help.~%"))))))))

(defun show-help (s)
  (terpri)
  (write-line "TTY-Inspector Help:" s)
  (write-line "  R           -  recompute current object." s)
  (write-line "  D           -  redisplay current object." s)
  (write-line "  U           -  Move upward through the object stack." s)
  (write-line "  Q, E        -  Quit TTY-INSPECTOR." s)
  (write-line "  ?, H, Help  -  Show this help." s))

(defun tty-display-object (parts stream)
  (format stream "~%~a" (car parts))
  (let ((numbered-parts-p (numbered-parts-p parts))
	(parts (cddr parts)))
    (do ((part parts (cdr part))
	 (i 0 (1+ i)))
	((endp part) nil)
      (if numbered-parts-p
	  (format stream "~d. ~a: ~a~%" i (caar part)
		  (if (eq (cdar part) %illegal-object%)
		      "Unbound"
		      (cdar part)))
	  (format stream "~d. ~a~%" i (car part))))))



;;;; DESCRIBE-PARTS

(defun describe-parts (object)
  (typecase object
    (symbol (describe-symbol-parts object))
    (structure (describe-structure-parts object))
    (function (describe-function-parts object))
    (vector (describe-vector-parts object))
    (array (describe-array-parts object))
    (cons (describe-cons-parts object))
    (t (describe-atomic-parts object))))

(defun describe-symbol-parts (object)
  (list (format nil "~s is a symbol.~%" object) t
	(cons "Value" (if (boundp object)
			  (symbol-value object)
			  %illegal-object%))
	(cons "Function" (if (fboundp object)
			     (symbol-function object)
			     %illegal-object%))
	(cons "Plist" (symbol-plist object))
	(cons "Package" (symbol-package object))))

(defun describe-structure-parts (object)
  (let ((dd-slots
	 (c::dd-slots
	  (ext:info type structure-info
		    (system:%primitive header-ref object
				       system:%g-vector-structure-name-slot))))
	(parts-list ()))
    (push (format nil "~s is a structure.~%" object) parts-list)
    (push t parts-list)
    (dolist (dd-slot dd-slots (nreverse parts-list))
      (push (cons (c::dsd-%name dd-slot)
		  (system:%primitive header-ref object (c::dsd-index dd-slot)))
	    parts-list))))

(defun describe-function-parts (object)
  (list (format nil "Function ~s.~%Argument List: ~a." object
		(system:%primitive header-ref object
				   lisp::%function-arg-names-slot)
		#|###
		(system:%primitive header-ref object
				   lisp::%function-defined-from-slot)
		~%Defined from: ~a
		|#
		)
	t))

(defun describe-vector-parts (object)
  (list* (format nil "Object is a ~:[~;displaced ~]vector of length ~d.~%"
		 (lisp::%displacedp object) (length object))
	 nil
	 (coerce object 'list)))

(defun describe-cons-parts (object)
  (list* (format nil "Object is a LIST of length ~d.~%" (length object))
	 nil
	 object))

(defun describe-array-parts (object)
  (let* ((length (min (array-total-size object) inspect-length))
	 (reference-array (make-array length :displaced-to object))
	 (dimensions (array-dimensions object))
	 (parts ()))
    (push (format nil "Object is ~:[a displaced~;an~] array of ~a.~%~
                       Its dimensions are ~s.~%"
		  (array-element-type object) (lisp::%displacedp object)
		  dimensions)
	  parts)
    (push t parts)
    (dotimes (i length (nreverse parts))
      (push (cons (format nil "~a " (index-string i (reverse dimensions)))
		  (aref reference-array i))
	    parts))))

(defun describe-atomic-parts (object)
  (list (format nil "Object is an atom.~%") nil object))

