;;; -*- Package: Profile -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/profile.lisp,v 1.24 2002/11/05 22:45:41 cracauer Exp $")
;;;
;;; **********************************************************************
;;;
;;; Description: Simple profiling facility.
;;;
;;; Author: Skef Wholey, Rob MacLachlan
;;;
;;; Compatibility: Runs in any valid Common Lisp.  Three small implementation-
;;;   dependent changes can be made to improve performance and prettiness.
;;;
;;; Dependencies: The macro Quickly-Get-Time and the function
;;;   Required-Arguments should probably be tailored to the implementation for
;;;   the best results.  They will default to working, albeit inefficent, forms
;;;   in non-CMU implementations.  The Total-Consing macro is used to profile
;;;   consing: in unknown implementations 0 will be used.
;;;   See the "Implementation Parameters" section.
;;;
;;; Note: a timing overhead factor is computed when REPORT-TIME is first
;;; called.  This will be incorrect if profiling code is run in a different
;;; environment than the first call to REPORT-TIME.  For example, saving a core
;;; image on a high performance machine and running it on a low performance one
;;; will result in use of an erroneously small timing overhead factor.  In CMU
;;; CL, this cache is invalidated when a core is saved.
;;;

(defpackage "PROFILE"
  (:export *timed-functions* profile profile-all unprofile reset-time 
	   report-time report-time-custom *default-report-time-printfunction*
	   with-spacereport print-spacereports reset-spacereports
	   delete-spacereports *insert-spacereports*))

(in-package "PROFILE")


;;;; Implementation dependent interfaces:


(progn
  #-cmu
  (eval-when (compile eval)
    (warn
     "You may want to supply an implementation-specific ~
     Quickly-Get-Time function."))

  ;; In CMUCL, get-internal-run-time is good enough, so we just use it.

  (defconstant quick-time-units-per-second internal-time-units-per-second)
  
  (defmacro quickly-get-time ()
    `(the time-type (get-internal-run-time))))


;;; The type of the result from quickly-get-time.
#+cmu
(deftype time-type () '(unsigned-byte 29))
#-cmu
(deftype time-type () 'unsigned-byte)


;;; To avoid unnecessary consing in the "encapsulation" code, we find out the
;;; number of required arguments, and use &rest to capture only non-required
;;; arguments.  The function Required-Arguments returns two values: the first
;;; is the number of required arguments, and the second is T iff there are any
;;; non-required arguments (e.g. &optional, &rest, &key).

#+cmu 
(defun required-arguments (name)
  (let ((type (ext:info function type name)))
    (cond ((not (kernel:function-type-p type))
	   (values 0 t))
	  (t
	   (values (length (kernel:function-type-required type))
		   (if (or (kernel:function-type-optional type)
			   (kernel:function-type-keyp type)
			   (kernel:function-type-rest type))
		       t nil))))))

#-cmu
(progn
 (eval-when (compile eval)
   (warn
    "You may want to add an implementation-specific Required-Arguments function."))
 (eval-when (load eval)
   (defun required-arguments (name)
     (declare (ignore name))
     (values 0 t))))



;;; The Total-Consing macro is called to find the total number of bytes consed
;;; since the beginning of time.

(declaim (inline total-consing))
#+cmu
(defun total-consing () (ext:get-bytes-consed-dfixnum))

#-cmu
(eval-when (compile eval)
  (error "No consing will be reported unless a Total-Consing function is ~
           defined."))

(eval-when (compile load eval)
;; Some macros to implement a very simple "bignum" package consisting
;; of 2 fixnums.  This is intended to extend the range of the consing
;; for profiling without adding lots of extra consing to the profiling
;; routines.
(defconstant +fixnum-bits+
  #.(integer-length most-positive-fixnum)
  "The number of bits in a fixnum")
(defconstant +fixnum-byte+
  (byte +fixnum-bits+ 0)
  "A byte for extracting out a fixnum-sized byte from an least significant part of an integer")

(defmacro dfix-add ((a-hi a-lo) (b-hi b-lo))
  ;; Add the 2 fixnums and return the sum as two values
  (let ((hi (gensym))
	(lo (gensym)))
    `(let ((,hi (+ ,a-hi ,b-hi))
	   (,lo (+ ,a-lo ,b-lo)))
       (if (<= ,lo most-positive-fixnum)
	   (values ,hi ,lo)
	   (values (+ ,hi 1)
		   (ldb +fixnum-byte+
			,lo))))))
(defmacro dfix-sub ((a-hi a-lo) (b-hi b-lo))
  ;; Subtract 2 fixnums and return the difference as two values
  (let ((hi (gensym))
	(lo (gensym)))
    `(let ((,hi (- ,a-hi ,b-hi))
	   (,lo (- ,a-lo ,b-lo)))
       (if (>= ,lo)
	   (values ,hi ,lo)
	   (values (- ,hi 1)
		   (+ ,lo #.(1+ most-positive-fixnum)))))))
  
(defmacro dfix-incf ((res-hi res-lo) (a-hi a-lo))
  ;; Like incf, except for pairs of fixnum
  `(multiple-value-setq (,res-hi ,res-lo)
     (dfix-add (,res-hi ,res-lo) (,a-hi ,a-lo))))
)

#-cmu
(progn
  (eval-when (compile eval)
    (warn "No consing will be reported unless a Total-Consing function is ~
           defined."))

  (defmacro total-consing () '0))


;;; The type of the result of TOTAL-CONSING.
#+cmu
(deftype consing-type () '(and fixnum unsigned-byte))
#-cmu
(deftype consing-type () 'unsigned-byte)

;;; On the CMUCL x86 port the return address is represented as a SAP
;;; and to save the costly calculation of the SAPs code object the
;;; profiler maintains callers as SAPs. These SAPs will become invalid
;;; if a caller code object moves, so this should be prevented by the
;;; use of purify or by moving code objects into an older generation
;;; when using GENCGC.
;;;
#+cmu
(progn
  (defmacro get-caller-info ()
    `(nth-value 1 (kernel:%caller-frame-and-pc)))
  #-(and cmu x86)
  (defun print-caller-info (info stream)
    (prin1 (kernel:lra-code-header info) stream))
  #+(and cmu x86)
  (defun print-caller-info (info stream)
    (prin1 (nth-value 1 (di::compute-lra-data-from-pc info)) stream)))

#-cmu
(progn
  (defmacro get-caller-info () 'unknown)
  (defun print-caller-info (info stream)
    (prin1 "no caller info" stream)))


;;;; Global data structures:

(defvar *timed-functions* ()
  "List of functions that are currently being timed.")
(defvar *no-calls* nil
  "A list of profiled functions which weren't called.")
(defvar *no-calls-limit* 20
  "If the number of profiled functions that were not called is less than
this, the functions are listed.  If NIL, then always list the functions.")

;;; We associate a PROFILE-INFO structure with each profiled function name.
;;; This holds the functions that we call to manipulate the closure which
;;; implements the encapsulation.
;;;
(defvar *profile-info* (make-hash-table :test #'equal))
(defstruct profile-info
  (name nil)
  (old-definition (error "Required keyword arg not supplied.") :type function)
  (new-definition (error "Required keyword arg not supplied.") :type function)
  (read-time (error "Required keyword arg not supplied.") :type function)
  (reset-time (error "Required keyword arg not supplied.") :type function))

;;; PROFILE-INFO-OR-LOSE  --  Internal
;;;
(defun profile-info-or-lose (name)
  (or (gethash name *profile-info*)
      (error "~S is not a profiled function." name)))


;;; We keep around a bunch of functions that make encapsulations, one of each
;;; (min-args . optional-p) signature we have encountered so far.  We also
;;; precompute a bunch of encapsulation functions.
;;;
(defvar *existing-encapsulations* (make-hash-table :test #'equal))


;;; These variables are used to subtract out the time and consing for recursive
;;; and other dynamically nested profiled calls.  The total resource consumed
;;; for each nested call is added into the appropriate variable.  When the
;;; outer function returns, these amounts are subtracted from the total.
;;;
;;; *enclosed-consing-hi* and *enclosed-consing* represent the total
;;; consing as a pair of fixnum-sized integers to reduce consing and
;;; allow for about 2^58 bytes of total consing.  (Assumes positive
;;; fixnums are 29 bits long).
(defvar *enclosed-time* 0)
(defvar *enclosed-consing-h* 0)
(defvar *enclosed-consing-l* 0)
(defvar *enclosed-profilings* 0)
(declaim (type time-type *enclosed-time*))
(declaim (type dfixnum:dfparttype *enclosed-consing-h*))
(declaim (type dfixnum:dfparttype *enclosed-consing-l*))
(declaim (fixnum *enclosed-profilings*))


;;; The number of seconds a bare function call takes.  Factored into the other
;;; overheads, but not used for itself.
;;;
(defvar *call-overhead*)

;;; The number of seconds that will be charged to a profiled function due to
;;; the profiling code.
(defvar *internal-profile-overhead*)

;;; The number of seconds of overhead for profiling that a single profiled call
;;; adds to the total runtime for the program.
;;;
(defvar *total-profile-overhead*)

(declaim (single-float *call-overhead* *internal-profile-overhead*
		       *total-profile-overhead*))


;;;; Profile encapsulations:

(eval-when (compile load eval)

(defun make-profile-encapsulation (min-args optionals-p)
  (let ((required-args ()))
    (dotimes (i min-args)
      (push (gensym) required-args))
    `(lambda (name callers-p)
       (let* ((time 0)
	      (count 0)
	      (consed-h 0)
	      (consed-l 0)
	      (consed-w/c-h 0)
	      (consed-w/c-l 0)
	      (profile 0)
	      (callers ())
	      (old-definition (fdefinition name)))
	 (declare (type time-type time)
		  (type dfixnum:dfparttype consed-h consed-l)
		  (type dfixnum:dfparttype consed-w/c-h consed-w/c-l)
		  (fixnum count))
	 (pushnew name *timed-functions*)

	 (setf (fdefinition name)
	       #'(lambda (,@required-args
			  ,@(if optionals-p
				#+cmu
				`(c:&more arg-context arg-count)
				#-cmu
				`(&rest optional-args)))
		   (incf count)
		   (when callers-p
		     (let ((caller (get-caller-info)))
		       (do ((prev nil current)
			    (current callers (cdr current)))
			   ((null current)
			    (push (cons caller 1) callers))
			 (let ((old-caller-info (car current)))
			   (when #-(and cmu x86) (eq caller
						     (car old-caller-info))
				 #+(and cmu x86) (sys:sap=
						  caller (car old-caller-info))
			     (if prev
				 (setf (cdr prev) (cdr current))
				 (setq callers (cdr current)))
			     (setf (cdr old-caller-info)
				   (the fixnum
					(+ (cdr old-caller-info) 1)))
			     (setf (cdr current) callers)
			     (setq callers current)
			     (return))))))
			       
		   (let ((time-inc 0)
			 (cons-inc-h 0)
			 (cons-inc-l 0)
			 (profile-inc 0))
		     (declare (type time-type time-inc)
			      (type dfixnum:dfparttype cons-inc-h cons-inc-l)
			      (fixnum profile-inc))
		     (multiple-value-prog1
			 (let ((start-time (quickly-get-time))
			       (start-consed-h 0)
			       (start-consed-l 0)
			       (end-consed-h 0)
			       (end-consed-l 0)
			       (*enclosed-time* 0)
			       (*enclosed-consing-h* 0)
			       (*enclosed-consing-l* 0)
			       (*enclosed-profilings* 0))
			   (dfixnum:dfixnum-set-pair start-consed-h
						     start-consed-l
						     (total-consing))
			   (multiple-value-prog1
			       ,(if optionals-p
				    #+cmu
				    `(multiple-value-call
					 old-definition
				       (values ,@required-args)
				       (c:%more-arg-values arg-context
							   0
							   arg-count))
				    #-cmu
				    `(apply old-definition
					    ,@required-args optional-args)
				    `(funcall old-definition ,@required-args))
			     (setq time-inc
				   #-BSD
				   (- (quickly-get-time) start-time)
				   #+BSD
				   (max (- (quickly-get-time) start-time) 0))
			     ;; How much did we cons so far?
			     (dfixnum:dfixnum-set-pair end-consed-h
						       end-consed-l
						       (total-consing))
			     (dfixnum:dfixnum-copy-pair cons-inc-h cons-inc-l
							end-consed-h
							end-consed-l)
			     (dfixnum:dfixnum-dec-pair cons-inc-h cons-inc-l
						       start-consed-h
						       start-consed-l)
			     ;; (incf consed (- cons-inc *enclosed-consing*))
			     (dfixnum:dfixnum-inc-pair consed-h consed-l
						       cons-inc-h cons-inc-l)
			     (dfixnum:dfixnum-inc-pair consed-w/c-h
						       consed-w/c-l
						       cons-inc-h cons-inc-l)

			     (setq profile-inc *enclosed-profilings*)
			     (incf time
				   (the time-type
				     #-BSD
				     (- time-inc *enclosed-time*)
				     #+BSD
				     (max (- time-inc *enclosed-time*) 0)))
			     (dfixnum:dfixnum-dec-pair consed-h consed-l
						       *enclosed-consing-h*
						       *enclosed-consing-l*)
			     (incf profile profile-inc)))
		       (incf *enclosed-time* time-inc)
		       ;; *enclosed-consing* = *enclosed-consing + cons-inc
		       (dfixnum:dfixnum-inc-pair *enclosed-consing-h*
						 *enclosed-consing-l*
						 cons-inc-h
						 cons-inc-l)))))
	 
	 (setf (gethash name *profile-info*)
	       (make-profile-info
		:name name
		:old-definition old-definition
		:new-definition (fdefinition name)
		:read-time
		#'(lambda ()
		    (values count time
			    (dfixnum:dfixnum-pair-integer consed-h consed-l)
			    (dfixnum:dfixnum-pair-integer consed-w/c-h
							  consed-w/c-l)
			    profile callers))
		:reset-time
		#'(lambda ()
		    (setq count 0)
		    (setq time 0)
		    (setq consed-h 0)
		    (setq consed-l 0)
		    (setq consed-w/c-h 0)
		    (setq consed-w/c-l 0)
		    (setq profile 0)
		    (setq callers ())
		    t)))))))

); EVAL-WHEN (COMPILE LOAD EVAL)



;;; Precompute some encapsulation functions:
;;;
(macrolet ((frob ()
	     (let ((res ()))
	       (dotimes (i 4)
		 (push `(setf (gethash '(,i . nil) *existing-encapsulations*)
			      #',(make-profile-encapsulation i nil))
		       res))
	       (dotimes (i 2)
		 (push `(setf (gethash '(,i . t) *existing-encapsulations*)
			      #',(make-profile-encapsulation i t))
		       res))
	       `(progn ,@res))))
  (frob))



;;; Interfaces:

;;; PROFILE-1-FUNCTION  --  Internal
;;;
;;;    Profile the function Name.  If already profiled, unprofile first.
;;;
(defun profile-1-function (name callers-p)
  (cond ((fboundp name)
	 (when (gethash name *profile-info*)
	   (warn "~S already profiled, so unprofiling it first." name)
	   (unprofile-1-function name))
	 (multiple-value-bind (min-args optionals-p)
			      (required-arguments name)
	   (funcall (or (gethash (cons min-args optionals-p)
				 *existing-encapsulations*)
			(setf (gethash (cons min-args optionals-p)
				       *existing-encapsulations*)
			      (compile nil (make-profile-encapsulation
					    min-args optionals-p))))
		    name
		    callers-p)))
	(t
	 (warn "Ignoring undefined function ~S." name))))


;;; PROFILE  --  Public
;;;
(defmacro profile (&rest names)
  "PROFILE Name*
   Wraps profiling code around the named functions.  As in TRACE, the names are
   not evaluated.  If a function is already profiled, then unprofile and
   reprofile (useful to notice function redefinition.)  If a name is undefined,
   then we give a warning and ignore it.  If :CALLERS T appears, subsequent
   names have counts of the most common calling functions recorded.
   See also UNPROFILE, REPORT-TIME and RESET-TIME."
  (let ((names names)
	(callers nil)
	(res ()))
    (loop
      (unless names (return))
      (let ((name (pop names)))
	(case name
	  (:callers
	   (setq callers (pop names)))
	  (t
	   (push `(profile-1-function ',name ,callers) res)))))
    `(progn ,@res (values))))

;;; PROFILE-ALL -- Public
;;;
;;; Add profiling to all symbols in the given package.
;;;
(defun profile-all (&key (package *package*) (callers-p nil))
  "PROFILE-ALL

 Wraps profiling code around all functions in PACKAGE, which defaults
 to *PACKAGE*. If a function is already profiled, then unprofile and
 reprofile (useful to notice function redefinition.)  If a name is
 undefined, then we give a warning and ignore it.  If CALLERS-P is T
 names have counts of the most common calling functions recorded. See
 also UNPROFILE, REPORT-TIME and RESET-TIME. "
  (let ((package (if (packagep package)
		     package
		     (find-package package))))
    (do-symbols (symbol package (values))
      (when (and (eq (symbol-package symbol) package)
		 (fboundp symbol))
	(profile-1-function symbol callers-p)))))

;;; UNPROFILE  --  Public
;;;
(defmacro unprofile (&rest names)
  "Unwraps the profiling code around the named functions.  Names defaults to
  the list of all currently profiled functions."
  `(dolist (name ,(if names `',names '*timed-functions*) (values))
     (unprofile-1-function name)))


;;; UNPROFILE-1-FUNCTION  --  Internal
;;;
(defun unprofile-1-function (name)
  (let ((info (profile-info-or-lose name)))
    (remhash name *profile-info*)
    (setq *timed-functions*
	  (delete name *timed-functions*
		  :test #'equal))
    (if (eq (fdefinition name) (profile-info-new-definition info))
	(setf (fdefinition name) (profile-info-old-definition info))
	(warn "Preserving current definition of redefined function ~S."
	      name))))

;;; COMPENSATE-TIME  --  Internal
;;;
;;;    Return our best guess for the run time in a function, subtracting out
;;; factors for profiling overhead.  We subtract out the internal overhead for
;;; each call to this function, since the internal overhead is the part of the
;;; profiling overhead for a function that is charged to that function.
;;;
;;;    We also subtract out a factor for each call to a profiled function
;;; within this profiled function.  This factor is the total profiling overhead
;;; *minus the internal overhead*.  We don't subtract out the internal
;;; overhead, since it was already subtracted when the nested profiled
;;; functions subtracted their running time from the time for the enclosing
;;; function.
;;;
(defun compensate-time (calls time profile)
  (let ((compensated
	 (- (/ (float time) (float quick-time-units-per-second))
	    (* *internal-profile-overhead* (float calls))
	    (* (- *total-profile-overhead* *internal-profile-overhead*)
	       (float profile)))))
    (if (minusp compensated) 0.0 compensated)))


;; Compute and return the total time, total cons, total-calls, and the
;; width of the field needed to hold the total time, total cons,
;; total-calls, and the max time/call.
(defun compute-totals-and-widths (info)
  (let ((total-time 0)
	(total-cons 0)
	(total-calls 0)
	(max-time/call 0))
    ;; Find the total time, total consing, total calls, and the max
    ;; time/call
    (dolist (item info)
      (let ((time (time-info-time item)))
	(incf total-time time)
	(incf total-cons (time-info-consing item))
	(incf total-calls (time-info-calls item))
	(setf max-time/call (max max-time/call
				 (/ time (float (time-info-calls item)))))))

    ;; Figure out the width needed for total-time, total-cons,
    ;; total-calls and the max-time/call.  The total-cons is more
    ;; complicated because we print the consing with comma
    ;; separators. For total-time, we assume a default of "~10,3F";
    ;; for total-calls, "~7D"; for time/call, "~10,5F".  This is where
    ;; the constants come from.
    (flet ((safe-log10 (x)
	     ;; log base 10 of x, but any non-positive value of x, 0
	     ;; is ok for what we want.
	     (if (zerop x)
		 0.0
		 (log x 10))))
      (let ((cons-length (ceiling (safe-log10 total-cons))))
	(incf cons-length (floor (safe-log10 total-cons) 3))
	(values total-time
		total-cons
		total-calls
		(+ 3 (max 7 (ceiling (safe-log10 total-time))))
		(max 9 cons-length)
		(max 7 (ceiling (safe-log10 total-calls)))
		(+ 5 (max 5 (ceiling (safe-log10 max-time/call)))))))))

(defstruct (time-info
	    (:constructor make-time-info
			  (name calls time consing consing-w/c callers)))
  name
  calls
  time
  consing
  consing-w/c
  callers)

(defstruct (time-totals)
  (time 0.0)
  (consed 0)
  (calls 0))

(defun report-times-time (time action)
  (case action
    (:head
     (format *trace-output*
	     "~&  Consed    |   Calls   |    Secs   | Sec/Call  | Bytes/C.  | Name:~@
	       -----------------------------------------------------------------------~%")
     (return-from report-times-time))

    (:tail
     (format *trace-output*
	     "-------------------------------------------------------------------~@
	      ~11:D |~10:D |           |           |           | Total~%"
	     (time-totals-consed time) (time-totals-calls time)))
    (:sort (sort time #'>= :key #'time-info-time))
    (:one-function
     (format *trace-output*
	     "~11:D |~10:D |~10,3F |~10,5F |~10:D | ~S~%"
	     (floor (time-info-consing time))
	     (time-info-calls time)
	     (time-info-time time)
	     (/ (time-info-time time) (float (time-info-calls time)))
	     (round
	       (/ (time-info-consing time) (float (time-info-calls time))))
	     (time-info-name time)))
    (t
     (error "Unknown action for profiler report: ~s" action))))

(defun report-times-space (time action)
  (case action
    (:head
     (format *trace-output*
	     "~& Consed w/c |  Consed    |   Calls   | Sec/Call  | Bytes/C.  | Name:~@
	       -----------------------------------------------------------------------~%")
     (return-from report-times-space))
    
    (:tail
     (format *trace-output*
	     "-------------------------------------------------------------------~@
	      :-)         |~11:D |~10:D |           |           | Total~%"
	     (time-totals-consed time) (time-totals-calls time)))
    (:sort (sort time #'>= :key #'time-info-consing))
    (:one-function
     (format *trace-output*
	     "~11:D |~11:D |~10:D |~10,5F |~10:D | ~S~%"
	     (floor (time-info-consing-w/c time))
	     (floor (time-info-consing time))
	     (time-info-calls time)
	     (/ (time-info-time time) (float (time-info-calls time)))
	     (round
	      (/ (time-info-consing time) (float (time-info-calls time))))
	     (time-info-name time)))
    (t
     (error "Unknown action for profiler report"))))

(defparameter *default-report-time-printfunction* #'report-times-time)

(defun %report-times (names
		      &key (printfunction *default-report-time-printfunction*))
  (declare (optimize (speed 0)))
  (unless (boundp '*call-overhead*)
    (compute-time-overhead))
  (let ((info ())
	(no-call ()))
    (dolist (name names)
      (let ((pinfo (profile-info-or-lose name)))
	(unless (eq (fdefinition name)
		    (profile-info-new-definition pinfo))
	  (warn "Function ~S has been redefined, so times may be inaccurate.~@
	         PROFILE it again to record calls to the new definition."
		name))
	(multiple-value-bind
	    (calls time consing consing-w/c profile callers)
	    (funcall (profile-info-read-time pinfo))
	  (if (zerop calls)
	      (push name no-call)
	      (push (make-time-info name calls
				    (compensate-time calls time profile)
				    consing
				    consing-w/c
				    (sort (copy-seq callers)
					  #'>= :key #'cdr))
		    info)))))
    
    (setq info (funcall printfunction info :sort))

    (funcall printfunction nil :head)

    (let ((totals (make-time-totals)))
      (dolist (time info)
	(incf (time-totals-time totals) (time-info-time time))
	(incf (time-totals-calls totals) (time-info-calls time))
	(incf (time-totals-consed totals) (time-info-consing time))

	(funcall printfunction time :one-function)

	#+nil
	(let ((callers (time-info-callers time)))
	  (when callers
	    (dolist (x (subseq callers 0 (min (length callers) 5)))
	      (format *trace-output* "~10:D: " (cdr x))
	      (print-caller-info (car x) *trace-output*)
	      (terpri *trace-output*))
	    (terpri *trace-output*))))
      (funcall printfunction totals :tail))
    
    #+nil
    (when no-call
      (format *trace-output*
	      "~%These functions were not called:~%~{~<~%~:; ~S~>~}~%"
	      (sort no-call #'string<
		    :key #'(lambda (n)
			     (cond ((symbolp n)
				    (symbol-name n))
				   ((and (listp n)
					 (eq (car n) 'setf)
					 (consp (cdr n))
					 (symbolp (cadr n)))
				    (symbol-name (cadr n)))
				   (t
				    (princ-to-string n)))))))
    (values)))


(defmacro reset-time (&rest names)
  "Resets the time counter for the named functions.  Names defaults to the list
  of all currently profiled functions."
  `(%reset-time ,(if names `',names '*timed-functions*)))

(defun %reset-time (names)
  (dolist (name names)
    (funcall (profile-info-reset-time (profile-info-or-lose name))))
  (values))


(defmacro report-time (&rest names)
  "Reports the time spent in the named functions.  Names defaults to the list
  of all currently profiled functions."
  `(%report-times ,(if names `',names '*timed-functions*)))

(defun report-time-custom (&key names printfunction)
  "Reports the time spent in the named functions.  Names defaults to the list
  of all currently profiled functions.  Uses printfunction."
  (%report-times (or names *timed-functions*)
		 :printfunction
		 (or (typecase printfunction
		       (null *default-report-time-printfunction*)
		       (function printfunction)
		       (symbol
		        (case printfunction
			  (:space #'report-times-space)
			  (:time #'report-times-time))))
		     (error "Cannot handle printfunction ~s" printfunction))))


;;;; Overhead computation.

;;; We average the timing overhead over this many iterations.
;;;
(defconstant timer-overhead-iterations 5000)


;;; COMPUTE-TIME-OVERHEAD-AUX  --  Internal
;;;
;;;    Dummy function we profile to find profiling overhead.  Declare
;;; debug-info to make sure we have arglist info.
;;;
(declaim (notinline compute-time-overhead-aux))
(defun compute-time-overhead-aux (x)
  (declare (ext:optimize-interface (debug 2)))
  (declare (ignore x)))


;;; COMPUTE-TIME-OVERHEAD  --  Internal
;;;
;;;    Initialize the profiling overhead variables.
;;;
(defun compute-time-overhead ()
  (macrolet ((frob (var)
	       `(let ((start (quickly-get-time))
		      (fun (symbol-function 'compute-time-overhead-aux)))
		  (dotimes (i timer-overhead-iterations)
		    (funcall fun fun))
		  (setq ,var
			(/ (float (- (quickly-get-time) start))
			   (float quick-time-units-per-second)
			   (float timer-overhead-iterations))))))
    (frob *call-overhead*)
    
    (unwind-protect
	(progn
	  (profile compute-time-overhead-aux)
	  (frob *total-profile-overhead*)
	  (decf *total-profile-overhead* *call-overhead*)
	  (let ((pinfo (profile-info-or-lose 'compute-time-overhead-aux)))
	    (multiple-value-bind (calls time)
				 (funcall (profile-info-read-time pinfo))
	      (declare (ignore calls))
	      (setq *internal-profile-overhead*
		    (/ (float time)
		       (float quick-time-units-per-second)
		       (float timer-overhead-iterations))))))
      (unprofile compute-time-overhead-aux))))

#+cmu
(pushnew #'(lambda ()
	     (makunbound '*call-overhead*))
	 ext:*before-save-initializations*)


;;;
;;; (with-spacereport <tag> <body> ...) and friends
;;;

;;; TODO:
;;; - if counting place haven't been allocated at compile time, try to do it
;;;   at load time
;;; - Introduce a mechanism that detects whether *all* calls were the same
;;;   amount of bytes (single variable).
;;; - record the source file and place this report appears in
;;; - detect whether this is a nested spacereport and if so, record
;;;   the outer reports

;; This struct is used for whatever counting the checkpoints do
;; AND
;; stores information we find at compile time
(defstruct spacereport-info
  (n 0 :type fixnum)
  (consed-h 0 :type dfixnum:dfparttype)
  (consed-l 0 :type dfixnum:dfparttype)
  (codesize -1 :type fixnum))

;; In the usual case, the hashtable with entries will be allocated at
;; compile or load time
(eval-when (load eval)
  (defvar *spacereports* (make-hash-table)))

;;
;; Helper functions
;;
(defun format-quotient (p1 p2 width komma)
  (let (format)
    (cond ((= 0 p2)
	   (make-string width :initial-element #\ ))
	  ((and (integerp p1)
		(integerp p2)
		(zerop (rem p1 p2)))
	   (setf format (format nil "~~~d:D!~a"
				(- width komma 1)
				(make-string komma :initial-element #\ )))
	   (format nil format (/ p1 p2)))
	  (t
	   (setf format (format nil "~~~d,~df" width komma))
	   (format nil format (/ (float p1) (float p2)))))))

(defun deep-list-length (list)
  (let ((length 0))
    (dolist (e list)
      (when (listp e)
	(incf length (deep-list-length e)))
      (incf length))
    length))      

;; bunch for tests for above
#+nil
(defun test-format-quotient ()
  (print (format-quotient 10 5 10 2))
  (print (format-quotient 10 3 10 2))
  (print (format-quotient 10 5 10 0))
  (print (format-quotient 10 3 10 0))
  (print (format-quotient 10 0 10 0)))

(defvar *insert-spacereports* t)

;; Main wrapper macro for user - exported
(defmacro with-spacereport (name-or-args &body body)
  (if (not *insert-spacereports*)
      `(progn ,@body)
      (let ((name
	     (typecase name-or-args
	       (symbol name-or-args)
	       (cons (first name-or-args))
	       (t (error "Spacereport args neither symbol nor cons") nil)))
	    (options (if (consp name-or-args)
			 (rest name-or-args)
			 nil)))
	(when (gethash name *spacereports*)
	  (unless (find :mok options)
	    (warn "spacereport for ~a was requested before, resetting it"
		  name)))
	(setf (gethash name *spacereports*) (make-spacereport-info))
	(setf (spacereport-info-codesize (gethash name *spacereports*))
	      (deep-list-length body))

	`(let* ((counterplace nil)
		(place (gethash ,name *spacereports*))
		(start-h 0)
		(start-l 0))
	  (declare (type dfixnum:dfparttype start-h start-l))
	  (declare (type (or dfixnum:dfixnum null) counterplace))
	  (declare (type (or spacereport-info null) place))

	  ;; Make sure counter is there
	  (unless place
	    ;; Ups, it isn't, so create it...
	    (setf place (make-spacereport-info))
	    (setf (gethash ,name *spacereports*) place)
	    (print
	     "with-spaceprofile had to create place, leaked bytes to outer
              spacereports in nested calls"))

	  ;; Remember bytes already consed at start
	  (setf counterplace (total-consing))
	  (dfixnum:dfixnum-set-pair start-h start-l counterplace)

	  (prog1
	      (progn ,@body)

	    (incf (spacereport-info-n place))
	    ;; Add bytes newly consed.
	    ;; first update counterplace.
	    (total-consing)
	    (dfixnum:dfixnum-inc-pair (spacereport-info-consed-h place)
				      (spacereport-info-consed-l place)
				      (dfixnum::dfixnum-h counterplace)
				      (dfixnum::dfixnum-l counterplace))
	    (dfixnum:dfixnum-dec-pair (spacereport-info-consed-h place)
				      (spacereport-info-consed-l place)
				      start-h
				      start-l))))))

(defun print-spacereports (&optional (stream *trace-output*))
  (maphash #'(lambda (key value)
	       (format
		stream
		"~&~10:D bytes ~9:D calls ~a b/call: ~a (sz ~d)~%"
		(dfixnum:dfixnum-pair-integer
		 (spacereport-info-consed-h value)
		 (spacereport-info-consed-l value))
		(spacereport-info-n value)
		(format-quotient (dfixnum:dfixnum-pair-integer
				  (spacereport-info-consed-h value)
				  (spacereport-info-consed-l value))
				 (spacereport-info-n value)
				 10 2)
		key
		(spacereport-info-codesize value)))
	   *spacereports*))

(defun reset-spacereports ()
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (setf (spacereport-info-consed-h value) 0)
	       (setf (spacereport-info-consed-l value) 0)
	       (setf (spacereport-info-n value) 0))
	   *spacereports*))

(defun delete-spacereports ()
  (maphash #'(lambda (key value)
	       (declare (ignore value))
	       (remhash key *spacereports*))
	   *spacereports*))
