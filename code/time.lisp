;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/time.lisp,v 1.14 1993/08/25 01:15:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the definitions for the Spice Lisp time functions.
;;; They are mostly fairly straightforwardly implemented as calls to the 
;;; time server.
;;;
;;;    Written by Rob MacLachlan.
;;;
(in-package "LISP")
(export '(internal-time-units-per-second get-internal-real-time
	  get-internal-run-time get-universal-time
	  get-decoded-time encode-universal-time decode-universal-time))

(defconstant internal-time-units-per-second 100
  "The number of internal time units that fit into a second.  See
  Get-Internal-Real-Time and Get-Internal-Run-Time.")

(defconstant micro-seconds-per-internal-time-unit
  (/ 1000000 internal-time-units-per-second))


(defmacro not-leap-year (year)
  (let ((sym (gensym)))
    `(let ((,sym ,year))
       (cond ((eq (mod ,sym 4) 0)
	      (and (eq (mod ,sym 100) 0)
		   (not (eq (mod ,sym 400) 0))))
	     (T T)))))


;;; The base number of seconds for our internal "epoch".  We initialize this to
;;; the time of the first call to G-I-R-T, and then subtract this out of the
;;; result.
;;;
(defvar *internal-real-time-base-seconds* nil)
(declaim (type (or (unsigned-byte 32) null) *internal-real-time-base-seconds*))

;;; Get-Internal-Real-Time  --  Public
;;;
(defun get-internal-real-time ()
  "Return the real time in the internal time format.  This is useful for
  finding elapsed time.  See Internal-Time-Units-Per-Second."
  (locally (declare (optimize (speed 3) (safety 0)))
    (multiple-value-bind (ignore seconds useconds) (unix:unix-gettimeofday)
      (declare (ignore ignore) (type (unsigned-byte 32) seconds useconds))
      (let ((base *internal-real-time-base-seconds*)
	    (uint (truncate useconds
			    micro-seconds-per-internal-time-unit)))
	(declare (type (unsigned-byte 32) uint))
	(cond (base
	       (truly-the (unsigned-byte 32)
		    (+ (the (unsigned-byte 32)
			    (* (the (unsigned-byte 32) (- seconds base))
			       internal-time-units-per-second))
		       uint)))
	      (t
	       (setq *internal-real-time-base-seconds* seconds)
	       uint))))))


;;; Get-Internal-Run-Time  --  Public
;;;
(defun get-internal-run-time ()
  "Return the run time in the internal time format.  This is useful for
  finding CPU usage."
  (declare (values (unsigned-byte 32)))
  (locally (declare (optimize (speed 3) (safety 0)))
    (multiple-value-bind (ignore utime-sec utime-usec stime-sec stime-usec)
			 (unix:unix-fast-getrusage unix:rusage_self)
      (declare (ignore ignore)
	       (type (unsigned-byte 31) utime-sec stime-sec)
	       (type (mod 1000000) utime-usec stime-usec))
      (+ (the (unsigned-byte 32)
	      (* (the (unsigned-byte 32) (+ utime-sec stime-sec))
		 internal-time-units-per-second))
	 (truncate (+ utime-usec stime-usec)
		   micro-seconds-per-internal-time-unit)))))


;;; Subtract from the returned Internal_Time to get the universal time.
;;; The offset between our time base and the Perq one is 2145 weeks and
;;; five days.
;;;
(defconstant seconds-in-week (* 60 60 24 7))
(defconstant weeks-offset 2145)
(defconstant seconds-offset 432000)
(defconstant minutes-per-day (* 24 60))
(defconstant quarter-days-per-year (1+ (* 365 4)))
(defconstant quarter-days-per-century 146097)
(defconstant november-17-1858 678882)
(defconstant weekday-november-17-1858 2)
(defconstant unix-to-universal-time 2208988800)

;;; Make-Universal-Time  --  Internal
;;;
;;;    Convert a Unix Internal_Time into a universal time.
;;;
(defun make-universal-time (weeks msec)
  (+ (* (- weeks weeks-offset) seconds-in-week)
     (- (truncate msec 1000) seconds-offset)))


;;; Get-Universal-Time  --  Public
;;;
;;;
(defun get-universal-time ()
  "Returns a single integer for the current time of
   day in universal time format."
  (multiple-value-bind (res secs) (unix:unix-gettimeofday)
    (declare (ignore res))
    (+ secs unix-to-universal-time)))

(defun get-decoded-time ()
  "Returns nine values specifying the current time as follows:
   second, minute, hour, date, month, year, day of week (0 = Monday), T
   (daylight savings times) or NIL (standard time), and timezone."
  (decode-universal-time (get-universal-time)))

(defun decode-universal-time (universal-time &optional time-zone)
  "Converts a universal-time to decoded time format returning the following
  nine values: second, minute, hour, date, month, year, day of week (0 =
  Monday), T (daylight savings time) or NIL (standard time), and timezone.
  Completely ignores daylight-savings-time when time-zone is supplied."
  (declare (type (or fixnum null) time-zone))
  (multiple-value-bind (weeks secs)
		       (truncate (+ universal-time seconds-offset)
				 seconds-in-week)
    (let ((weeks (+ weeks weeks-offset))
	  (second NIL)
	  (minute NIL)
	  (hour NIL)
	  (date NIL)
	  (month NIL)
	  (year NIL)
	  (day NIL)
	  (daylight NIL)
	  (timezone (if (null time-zone)
			(multiple-value-bind (res s us tz)
					     (unix:unix-gettimeofday)
			  (declare (ignore s us))
			  (if res tz 0))
			(* time-zone 60))))
      (declare (fixnum timezone))
      (multiple-value-bind (t1 seconds) (truncate secs 60)
	(setq second seconds)
	(setq t1 (- t1 timezone))
	(let* ((tday (if (< t1 0)
			 (1- (truncate (1+ t1) minutes-per-day))
			 (truncate t1 minutes-per-day))))
	  (multiple-value-setq (hour minute)
	    (truncate (- t1 (* tday minutes-per-day)) 60))
	  (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
		 (tcent (truncate t2 quarter-days-per-century)))
	    (setq t2 (mod t2 quarter-days-per-century))
	    (setq t2 (+ (- t2 (mod t2 4)) 3))
	    (setq year (+ (* tcent 100) (truncate t2 quarter-days-per-year)))
	    (let ((days-since-mar0 (1+ (truncate (mod t2 quarter-days-per-year)
						 4))))
	      (setq day (mod (+ tday weekday-november-17-1858) 7))
	      (unless time-zone
		(if (setq daylight (dst-check days-since-mar0 hour day))
		    (cond ((eq hour 23)
			   (setq hour 0)
			   (setq day (mod (1+ day) 7))
			   (setq days-since-mar0 (1+ days-since-mar0))
			   (if (>= days-since-mar0 366)
			       (if (or (> days-since-mar0 366)
				       (not-leap-year (1+ year)))
				   (setq days-since-mar0 368))))
			  (T (setq hour (1+ hour))))))
	      (let ((t3 (+ (* days-since-mar0 5) 456)))
		(cond ((>= t3 1989)
		       (setq t3 (- t3 1836))
		       (setq year (1+ year))))
		(multiple-value-setq (month t3) (truncate t3 153))
		(setq date (1+ (truncate t3 5))))))))
      (values second minute hour date month year day
	      daylight (truncate timezone 60)))))

;;; Encode-Universal-Time  --  Public
;;;
;;;    Just do a TimeUser:T_UserToInt.  If the year is between 0 and 99 we 
;;; have to figure out which the "obvious" year is.
;;;

(defun encode-universal-time (second minute hour date month year
				     &optional time-zone)
  "The time values specified in decoded format are converted to 
   universal time, which is returned."
  (let* ((year (if (< year 100)
		   (multiple-value-bind (sec min hour day month now-year)
					(get-decoded-time)
		     (declare (ignore sec min hour day month))
		     (do ((y (+ year (* 100 (1- (truncate now-year 100))))
			     (+ y 100)))
			 ((<= (abs (- y now-year)) 50) y)))
		   year))
	 (zone (if time-zone (* time-zone 60)
		   (multiple-value-bind (res s us tz) (unix:unix-gettimeofday)
		     (declare (ignore s us))
		     (if res tz))))
	 (tmonth (- month 3)))
    (cond ((< tmonth 0)
	   (setq tmonth (+ tmonth 12))
	   (setq year (1- year))))
    (let ((days-since-mar0 (+ (truncate (+ (* tmonth 153) 2) 5) date)))
      (multiple-value-bind (tcent tyear) (truncate year 100)
	(let* ((tday (- (+ (truncate (* tcent quarter-days-per-century) 4)
			   (truncate (* tyear quarter-days-per-year) 4)
			   days-since-mar0)
			november-17-1858))
	       (daylight (dst-check days-since-mar0 (1- hour)
				    (mod (+ tday weekday-november-17-1858) 7)))
	       (tminutes (+ (* hour 60) minute zone)))
	  (if daylight (setq tminutes (- tminutes 60)))
	  (do ((i tminutes (+ i minutes-per-day)))
	      ((>= i 0) (setq tminutes i))
	    (declare (fixnum i))
	    (decf tday 1))
	  (do ((i tminutes (- i minutes-per-day)))
	      ((< i minutes-per-day) (setq tminutes i))
	    (declare (fixnum i))
	    (incf tday 1))
	  (multiple-value-bind (weeks dpart) (truncate tday 7)
	    (make-universal-time weeks (* (+ (* (+ (* dpart minutes-per-day)
						   tminutes) 60)
					     second) 1000))))))))

;;; Dst-check -- Internal
(defconstant april-1 (+ (truncate (+ (* (- 4 3) 153) 2) 5) 1))
(defconstant october-31 (+ (truncate (+ (* (- 10 3) 153) 2) 5) 31))

(eval-when (compile eval)
  
  (defmacro dst-check-start-of-month-ge (day hour weekday daybound)
    (let ((d (gensym))
	  (h (gensym))
	  (w (gensym))
	  (db (gensym)))
      `(let ((,d ,day)
	     (,h ,hour)
	     (,w ,weekday)
	     (,db ,daybound))
	 (declare (fixnum ,d ,h ,w ,db))
	 (cond ((< ,d ,db) NIL)
	       ((> (the fixnum (- ,d ,w)) ,db) T)
	       ((and (eq ,w 6) (> ,h 0)) T)
	       (T NIL)))))
  
  (defmacro dst-check-end-of-month-ge (day hour weekday daybound)
    (let ((d (gensym))
	  (h (gensym))
	  (w (gensym))
	  (db (gensym)))
      `(let ((,d ,day)
	     (,h ,hour)
	     (,w ,weekday)
	     (,db ,daybound))
	 (declare (fixnum ,d ,h ,w ,db))
	 (cond ((< (the fixnum (+ ,d 6)) ,db) NIL)
	       ((> (the fixnum  (- (the fixnum (+ ,d 6)) ,w)) ,db) T)
	       ((and (eq ,w 6) (> ,h 0)) T)
	       (T NIL)))))
  )

(defun dst-check (day hour weekday)
  (and (dst-check-start-of-month-ge day hour weekday april-1)
       (not (dst-check-end-of-month-ge day hour weekday october-31))))

;;;; Time:

(defmacro time (form)
  "Evaluates the Form and prints timing information on *Trace-Output*."
  `(%time #'(lambda () ,form)))

;;; MASSAGE-TIME-FUNCTION  --  Internal
;;;
;;;    Try to compile the closure arg to %TIME if it is interpreted.
;;;
(defun massage-time-function (fun)
  (cond
   ((eval:interpreted-function-p fun)
    (multiple-value-bind (def env-p)
			 (function-lambda-expression fun)
      (declare (ignore def))
      (cond
       (env-p
	(warn "TIME form in a non-null environment, forced to interpret.~@
	       Compiling entire form will produce more accurate times.")
	fun)
       (t
	(compile nil fun)))))
   (t fun)))

;;; TIME-GET-SYS-INFO  --  Internal
;;;
;;;    Return all the files that we want time to report.
;;;
(defun time-get-sys-info ()
  (multiple-value-bind (user sys faults)
		       (system:get-system-info)
    (values user sys faults (get-bytes-consed))))

;;; %TIME  --  Internal
;;;
;;;    The guts of the TIME macro.  Compute overheads, run the (compiled)
;;; function, report the times.
;;;
(defun %time (fun)
  (let ((fun (massage-time-function fun))
	old-run-utime
        new-run-utime
        old-run-stime
        new-run-stime
        old-real-time
        new-real-time
        old-page-faults
        new-page-faults
        real-time-overhead
        run-utime-overhead
        run-stime-overhead
        page-faults-overhead
        old-bytes-consed
        new-bytes-consed
        cons-overhead)
    ;; Calculate the overhead...
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    ;; Do it a second time to make sure everything is faulted in.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    (multiple-value-setq
        (new-run-utime new-run-stime new-page-faults new-bytes-consed)
      (time-get-sys-info))
    (setq run-utime-overhead (- new-run-utime old-run-utime))
    (setq run-stime-overhead (- new-run-stime old-run-stime))
    (setq page-faults-overhead (- new-page-faults old-page-faults))
    (setq old-real-time (get-internal-real-time))
    (setq old-real-time (get-internal-real-time))
    (setq new-real-time (get-internal-real-time))
    (setq real-time-overhead (- new-real-time old-real-time))
    (setq cons-overhead (- new-bytes-consed old-bytes-consed))
    ;; Now get the initial times.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    (setq old-real-time (get-internal-real-time))
    (let ((start-gc-run-time *gc-run-time*))
    (multiple-value-prog1
        ;; Execute the form and return its values.
        (funcall fun)
      (multiple-value-setq
	  (new-run-utime new-run-stime new-page-faults new-bytes-consed)
	(time-get-sys-info))
      (setq new-real-time (- (get-internal-real-time) real-time-overhead))
      (let ((gc-run-time (max (- *gc-run-time* start-gc-run-time) 0)))
	(format *trace-output*
		"~&Evaluation took:~%  ~
		 ~S second~:P of real time~%  ~
		 ~S second~:P of user run time~%  ~
		 ~S second~:P of system run time~%  ~
		 ~@[[Run times include ~S second~:P GC run time]~%  ~]~
		 ~S page fault~:P and~%  ~
		 ~S bytes consed.~%"
		(max (/ (- new-real-time old-real-time)
			(float internal-time-units-per-second))
		     0.0)
		(max (/ (- new-run-utime old-run-utime) 1000000.0) 0.0)
		(max (/ (- new-run-stime old-run-stime) 1000000.0) 0.0)
		(unless (zerop gc-run-time)
		  (/ (float gc-run-time)
		     (float internal-time-units-per-second)))
		(max (- new-page-faults old-page-faults) 0)
		(max (- new-bytes-consed old-bytes-consed) 0)))))))
