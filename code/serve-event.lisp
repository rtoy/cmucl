;;; -*- Log: code.log; Package: LISP -*-

;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/serve-event.lisp,v 1.16 1992/07/17 18:16:26 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; SYSTEM:SERVE-EVENT, now in it's own file.
;;;
;;; Re-written by William Lott, July 1989 - January 1990.
;;; 
;;; **********************************************************************

(in-package "SYSTEM")

(export '(with-fd-handler add-fd-handler remove-fd-handler invalidate-descriptor
	  serve-event serve-all-events wait-until-fd-usable
	  make-object-set object-set-operation *xwindow-table*
	  map-xwindow add-xwindow-object remove-xwindow-object))

(in-package "EXTENSIONS")

(export '(*display-event-handlers*))

(in-package "LISP")



;;;; Object set stuff.

;;;
;;;    Hashtable from ports to objects.  Each entry is a cons (object . set).
;;;
;(defvar *port-table* (make-hash-table :test #'eql))

;;; Hashtable from windows to objects.  Each entry is a cons (object . set).
;;;
(defvar *xwindow-table* (make-hash-table :test #'eql))


(defstruct (object-set
	    (:constructor make-object-set
			  (name &optional
				(default-handler #'default-default-handler)))
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<Object Set ~S>" (object-set-name s)))))
  name					; Name, for descriptive purposes.
  (table (make-hash-table :test #'eq))  ; Message-ID or xevent-type --> handler fun.
  default-handler)

(setf (documentation 'make-object-set 'function)
      "Make an object set for use by a RPC/xevent server.  Name is for
      descriptive purposes only.")

;;; Default-Default-Handler  --  Internal
;;;
;;;    If no such operation defined, signal an error.
;;;
(defun default-default-handler (object)
  (error "You lose, object: ~S" object))


;;; MAP-XWINDOW and MAP-PORT return as multiple values the object and
;;; object set mapped to by a xwindow or port in *xwindow-table* or
;;; *port-table*.
;;; 
(macrolet ((defmapper (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "MAP-" (symbol-name name)))
		      (,name)
		 ,(format nil "Return as multiple values the object and ~
		               object-set mapped to by ~A."
			  (string-downcase (symbol-name name)))
		 (let ((temp (gethash ,name ,table)))
		   (if temp
		       (values (car temp) (cdr temp))
		       (values nil nil))))))
  ;(defmapper port *port-table*)
  (defmapper xwindow *xwindow-table*))


;;; ADD-PORT-OBJECT and ADD-XWINDOW-OBJECT store an object/object-set pair
;;; mapped to by a port or xwindow in either *port-table* or *xwindow-table*.
;;; 
(macrolet ((def-add-object (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "ADD-" (symbol-name name)
					    "-OBJECT"))
		      (,name object object-set)
		 ,(format nil "Add a new ~A/object/object-set association."
			  (string-downcase (symbol-name name)))
		 (check-type object-set object-set)
		 (setf (gethash ,name ,table) (cons object object-set))
		 object)))
  ;(def-add-object port *port-table*)
  (def-add-object xwindow *xwindow-table*))


;;; REMOVE-PORT-OBJECT and REMOVE-XWINDOW-OBJECT remove a port or xwindow and
;;; its associated object/object-set pair from *port-table* or *xwindow-table*.
;;; 
(macrolet ((def-remove-object (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "REMOVE-" (symbol-name name)
					    "-OBJECT"))
		      (,name)
		 ,(format nil
			  "Remove ~A and its associated object/object-set pair."
			  (string-downcase (symbol-name name)))
		 (remhash ,name ,table))))
  ;(def-remove-object port *port-table*)
  (def-remove-object xwindow *xwindow-table*))


;;; Object-Set-Operation  --  Public
;;;
;;;    Look up the handler function for a given message ID.
;;;
(defun object-set-operation (object-set message-id)
  "Return the handler function in Object-Set for the operation specified by
   Message-ID, if none, NIL is returned."
  (check-type object-set object-set)
  (check-type message-id fixnum)
  (values (gethash message-id (object-set-table object-set))))

;;; %Set-Object-Set-Operation  --  Internal
;;;
;;;    The setf inverse for Object-Set-Operation.
;;;
(defun %set-object-set-operation (object-set message-id new-value)
  (check-type object-set object-set)
  (check-type message-id fixnum)
  (setf (gethash message-id (object-set-table object-set)) new-value))
;;;
(defsetf object-set-operation %set-object-set-operation
  "Sets the handler function for an object set operation.")



;;;; File descriptor IO noise.

(defstruct (handler
	    (:print-function %print-handler)
	    (:constructor make-handler (direction descriptor function)))
  (direction nil :type (member :input :output)) ; Either :input or :output
  (descriptor 0 :type (mod 32)) ; File descriptor this handler is tied to.
  active		      ; T iff this handler is running.
  (function nil :type function) ; Function to call.
  bogus			      ; T if this descriptor is bogus. 
  )

(defun %print-handler (handler stream depth)
  (declare (ignore depth))
  (format stream "#<Handler for ~A on ~:[~;BOGUS ~]descriptor ~D: ~S>"
	  (handler-direction handler)
	  (handler-bogus handler)
	  (handler-descriptor handler)
	  (handler-function handler)))

(defvar *descriptor-handlers* nil
  "List of all the currently active handlers for file descriptors")


;;; ADD-FD-HANDLER -- public
;;;
;;;   Add a new handler to *descriptor-handlers*.
;;;
(defun add-fd-handler (fd direction function)
  "Arange to call FUNCTION whenever FD is usable. DIRECTION should be
  either :INPUT or :OUTPUT. The value returned should be passed to
  SYSTEM:REMOVE-FD-HANDLER when it is no longer needed."
  (assert (member direction '(:input :output))
	  (direction)
	  "Invalid direction ~S, must be either :INPUT or :OUTPUT" direction)
  (let ((handler (make-handler direction fd function)))
    (push handler *descriptor-handlers*)
    handler))

;;; REMOVE-FD-HANDLER -- public
;;;
;;;   Remove an old handler from *descriptor-handlers*.
;;;
(defun remove-fd-handler (handler)
  "Removes HANDLER from the list of active handlers."
  (setf *descriptor-handlers*
	(delete handler *descriptor-handlers*
		:test #'eq)))

;;; INVALIDATE-DESCRIPTOR -- public
;;;
;;;   Search *descriptor-handlers* for any reference to fd, and nuke 'em.
;;; 
(defun invalidate-descriptor (fd)
  "Remove any handers refering to fd. This should only be used when attempting
  to recover from a detected inconsistancy."
  (setf *descriptor-handlers*
	(delete fd *descriptor-handlers*
		:key #'handler-descriptor)))

;;; WITH-FD-HANDLER -- Public.
;;;
;;; Add the handler to *descriptor-handlers* for the duration of BODY.
;;;
(defmacro with-fd-handler ((fd direction function) &rest body)
  "Establish a handler with SYSTEM:ADD-FD-HANDLER for the duration of BODY.
   DIRECTION should be either :INPUT or :OUTPUT, FD is the file descriptor to
   use, and FUNCTION is the function to call whenever FD is usable."
  (let ((handler (gensym)))
    `(let (,handler)
       (unwind-protect
	   (progn
	     (setf ,handler (add-fd-handler ,fd ,direction ,function))
	     ,@body)
	 (when ,handler
	   (remove-fd-handler ,handler))))))


;;; HANDLER-DESCRIPTORS-ERROR -- Internal.
;;;
;;; First, get a list and mark bad file descriptors.  Then signal an error
;;; offering a few restarts.
;;;
(defun handler-descriptors-error ()
  (let ((bogus-handlers nil))
    (dolist (handler *descriptor-handlers*)
      (unless (or (handler-bogus handler)
		  (unix:unix-fstat (handler-descriptor handler)))
	(setf (handler-bogus handler) t)
	(push handler bogus-handlers)))
    (restart-case (error "~S ~[have~;has a~:;have~] bad file descriptor~:P."
			 bogus-handlers (length bogus-handlers))
      (remove-them () :report "Remove bogus handlers."
       (setf *descriptor-handlers*
	     (delete-if #'handler-bogus *descriptor-handlers*)))
      (retry-them () :report "Retry bogus handlers."
       (dolist (handler bogus-handlers)
	 (setf (handler-bogus handler) nil)))
      (continue () :report "Go on, leaving handlers marked as bogus."))))



;;;; Serve-all-events, serve-event, and friends.

(declaim (start-block wait-until-fd-usable start-block serve-event
		      serve-all-events))

;;; DECODE-TIMEOUT  --  Internal
;;;
;;;    Break a real timeout into seconds and microseconds.
;;;
(defun decode-timeout (timeout)
  (declare (values (or index null) index))
  (typecase timeout
    (integer (values timeout 0))
    (null (values nil 0))
    (real
     (multiple-value-bind (q r)
			  (truncate (coerce timeout 'single-float))
       (declare (type index q) (single-float r))
       (values q (the index (truncate (* r 1f6))))))
    (t
     (error "Timeout is not a real number or NIL: ~S" timeout))))


;;; WAIT-UNTIL-FD-USABLE -- Public.
;;;
;;; Wait until FD is usable for DIRECTION. The timeout given to serve-event is
;;; recalculated each time through the loop so that WAIT-UNTIL-FD-USABLE will
;;; timeout at the correct time irrespective of how many events are handled in
;;; the meantime.
;;;
(defun wait-until-fd-usable (fd direction &optional timeout)
  "Wait until FD is usable for DIRECTION. DIRECTION should be either :INPUT or
  :OUTPUT. TIMEOUT, if supplied, is the number of seconds to wait before giving
  up."
  (declare (type (or real null) timeout))
  (let (usable)
    (multiple-value-bind (to-sec to-usec)
			 (decode-timeout timeout)
      (declare (type (or index null) to-sec to-usec))
      (multiple-value-bind
	  (stop-sec stop-usec)
	  (if to-sec
	      (multiple-value-bind (okay start-sec start-usec)
				   (unix:unix-gettimeofday)
		(declare (ignore okay))
		(let ((usec (+ to-usec start-usec))
		      (sec (+ to-sec start-sec)))
		  (declare (type (unsigned-byte 31) usec sec))
		  (if (>= usec 1000000)
		      (values (1+ sec) (- usec 1000000))
		      (values sec usec))))
	      (values 0 0))
	(declare (type (unsigned-byte 31) stop-sec stop-usec))
	(with-fd-handler (fd direction #'(lambda (fd)
					   (declare (ignore fd))
					   (setf usable t)))
	  (loop
	    (sub-serve-event to-sec to-usec)
	    
	    (when usable
	      (return t))
	    
	    (when timeout
	      (multiple-value-bind (okay sec usec)
				   (unix:unix-gettimeofday)
		(declare (ignore okay))
		(when (or (> sec stop-sec)
			  (and (= sec stop-sec) (>= usec stop-usec)))
		  (return nil))
		(setq to-sec (- stop-sec sec))
		(setq to-usec (- stop-usec usec))))))))))


(defvar *display-event-handlers* nil
  "This is an alist mapping displays to user functions to be called when
   SYSTEM:SERVE-EVENT notices input on a display connection.  Do not modify
   this directly; use EXT:ENABLE-CLX-EVENT-HANDLING.  A given display
   should be represented here only once.")

;;; SERVE-ALL-EVENTS -- public
;;;
;;;   Wait for up to timeout seconds for an event to happen. Make sure all
;;; pending events are processed before returning.
;;;
(defun serve-all-events (&optional timeout)
  "SERVE-ALL-EVENTS calls SERVE-EVENT with the specified timeout.  If
  SERVE-EVENT does something (returns T) it loops over SERVE-EVENT with timeout
  0 until all events have been served.  SERVE-ALL-EVENTS returns T if
  SERVE-EVENT did something and NIL if not."
  (do ((res nil)
       (sval (serve-event timeout) (serve-event 0)))
      ((null sval) res)
    (setq res t)))


;;; SERVE-EVENT -- public
;;;
;;;   Serve a single event.
;;;
(defun serve-event (&optional timeout)
  "Receive on all ports and Xevents and dispatch to the appropriate handler
  function.  If timeout is specified, server will wait the specified time (in
  seconds) and then return, otherwise it will wait until something happens.
  Server returns T if something happened and NIL otherwise."
  (multiple-value-bind (to-sec to-usec)
		       (decode-timeout timeout)
    (sub-serve-event to-sec to-usec)))


;;; Check for any X displays with pending events.
;;;
#+clx
(defun handle-queued-clx-event ()
  (dolist (d/h *display-event-handlers*)
    (let* ((d (car d/h))
	   (disp-fd (fd-stream-fd (xlib::display-input-stream d))))
      (declare (inline member))
      ;;
      ;; If in the *descriptor-handlers*, then we are already waiting for input
      ;; on that display, and we don't want to do it recursively.
      (when (and (dolist (hand *descriptor-handlers* t)
		   (when (and (eql (handler-descriptor hand) disp-fd)
			      (not (eq (handler-function hand)
				       #'ext::call-display-event-handler)))
		     (return nil)))
		 (xlib::event-listen d))
	(handler-bind ((error #'(lambda (condx)
				  (declare (ignore condx))
				  (flush-display-events d))))
	  (unless (funcall (cdr d/h) d)
	    (disable-clx-event-handling d)
	    (error "Event-listen was true, but handler didn't handle: ~%~S"
		   d/h)))
	(return-from handle-queued-clx-event t)))))


;;; Call file descriptor handlers according to the readable and writable masks
;;; returned by select.
;;;
(defun call-fd-handler (readable writeable)
  (let ((result nil))
    (dolist (handler *descriptor-handlers*)
      (when (logbitp (handler-descriptor handler)
		     (ecase (handler-direction handler)
		       (:input readable)
		       (:output writeable)))
	(unwind-protect
	    (progn
	      ;; Doesn't work -- ACK
	      ;(setf (handler-active handler) t)
	      (funcall (handler-function handler)
		       (handler-descriptor handler)))
	  (setf (handler-active handler) nil))
	(macrolet ((frob (var)
		     `(setf ,var
			    (logand (32bit-logical-not
				     (ash 1
					  (handler-descriptor
					   handler)))
				    ,var))))
	  (ecase (handler-direction handler)
	    (:input (frob readable))
	    (:output (frob writeable))))
	(setf result t)))
    result))


;;; SUB-SERVE-EVENT  --  Internal
;;;
;;;    Takes timeout broken into seconds and microseconds.
;;;
(defun sub-serve-event (to-sec to-usec)
  #+clx
  (when (handle-queued-clx-event) (return-from sub-serve-event t))

  ;; Next, wait for something to happen.
  (multiple-value-bind
      (value readable writeable)
      (multiple-value-bind (count read-mask write-mask except-mask)
			   (calc-masks)
	;; Do the select.
	(unix:unix-select count read-mask write-mask except-mask
			  to-sec to-usec))
    (declare (type (unsigned-byte 32) readable writeable))
    ;; Now see what it was (if anything)
    (cond ((fixnump value)
	   (unless (zerop value)
	     (call-fd-handler readable writeable)))
	  ((eql readable unix:eintr)
	   ;; We did an interrupt.
	   t)
	  (t
	   ;; One of the file descriptors is bad.
	   (handler-descriptors-error)
	   nil))))


;;; CALC-MASKS -- Internal.
;;;
;;; Return the correct masks to use for UNIX-SELECT.  The four return values
;;; are: fd count, read mask, write mask, and exception mask.  The exception
;;; mask is currently unused.
;;;
(defun calc-masks ()
  (let ((count 0)
	(read-mask 0)
	(write-mask 0)
	(except-mask 0))
    (declare (type index count)
	     (type (unsigned-byte 32) read-mask write-mask except-mask))
    (dolist (handler *descriptor-handlers*)
      (unless (or (handler-active handler)
		  (handler-bogus handler))
	(let ((fd (handler-descriptor handler)))
	  (ecase (handler-direction handler)
	    (:input
	     (setf read-mask
		   (logior read-mask
			   (the (unsigned-byte 32) (ash 1 fd)))))
	    (:output
	     (setf write-mask
		   (logior write-mask
			   (the (unsigned-byte 32) (ash 1 fd))))))
	  (when (> fd count)
	    (setf count fd)))))
    (values (1+ count)
	    read-mask
	    write-mask
	    except-mask)))
