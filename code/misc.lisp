;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Assorted miscellaneous functions for Spice Lisp.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
(in-package "LISP")
(export '(documentation *features* common variable room
	  lisp-implementation-type lisp-implementation-version machine-type
	  machine-version machine-instance software-type software-version
	  short-site-name long-site-name dribble))


(defun documentation (symbol doc-type)
  "Returns the documentation string of Doc-Type for the Symbol, or NIL if
  none exists.  System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  and SETF."
  (case doc-type
    (variable (get symbol '%var-documentation))
    (function (get symbol '%fun-documentation))
    (structure (get symbol '%struct-documentation))
    (type (get symbol '%type-documentation))
    (setf (get symbol '%setf-documentation))
    (t (cdr (assoc doc-type (get symbol '%documentation))))))

(defun %set-documentation (symbol doc-type string)
  (case doc-type
    (variable (%put symbol '%var-documentation string))
    (function (%put symbol '%fun-documentation string))
    (structure (%put symbol '%struct-documentation string))
    (type (%put symbol '%type-documentation string))
    (setf (%put symbol '%setf-documentation string))
    (t (let ((pair (assoc doc-type (get symbol '%documentation))))
	 (if pair (%rplacd pair string)
	     (push (cons doc-type string) (get symbol '%documentation))))))
  string)

(defvar *features* '(:common :cmu :mach :ibm-rt-pc :clos :new-compiler)
  "Holds a list of symbols that describe features provided by the
   implementation.")

(defun featurep (x)
  "If X is an atom, see if it is present in *FEATURES*.  Also
  handle arbitrary combinations of atoms using NOT, AND, OR."
  (cond ((atom x) (memq x *features*))
	((eq (car x) ':not) (not (featurep (cadr x))))
	((eq (car x) ':and)
	 (every #'featurep (cdr x)))
	((eq (car x) ':or)
	 (some #'featurep (cdr x)))
	(t nil)))



;;; Other Environment Inquiries.

(defun lisp-implementation-type ()
  "Returns a string describing the implementation type."
  "CMU Common Lisp")

(defun lisp-implementation-version ()
  "Returns a string describing the implementation version."
  *lisp-implementation-version*)

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "IBM RT PC")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  (let ((version (system:%primitive 16bit-system-ref
				    (int-sap
				     (+ (ash clc::romp-data-base 16)
					clc::floating-point-hardware-available))
				    1)))
    (if (or (not (= (logand version clc::float-mc68881) 0))
	    (not (= (logand version clc::float-afpa) 0)))
	"IBM RT PC/APC"
	"IBM RT PC")))

(defun machine-instance ()
  "Returns a string giving the name of the local machine."
  (mach::unix-gethostname))

(defun software-type ()
  "Returns a string describing the supporting software."
  "MACH/4.3BSD")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  NIL)

(defun short-site-name ()
  "Returns a string with the abbreviated site name."
  "CMU-CSD")

(defun long-site-name ()
  "Returns a string with the long form of the site name."
  "Carnegie-Mellon University Computer Science Department")



;;;; Dribble stuff:

(defun dribble (&optional pathname &key (if-exists :append))
  "With a file name as an argument, dribble opens the file and
   sends a record of the output to that file.  Without an
   argument, it closes the open dribble file."
  (if pathname
      (with-open-file (f pathname :direction :output  :if-exists if-exists
			 :if-does-not-exist :create)
	(catch 'dribble-punt
	  (let ((*terminal-io*
		 (make-two-way-stream
		  (make-echo-stream *terminal-io* f)
		  (make-broadcast-stream *terminal-io* f))))
	    (%top-level))))
      (throw 'dribble-punt nil)))
