;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; CLX -- excldep.cl
;;;
;;; Copyright (c) 1987, 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.
;;;
;;; Franz Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib :use '(:foreign-functions :lisp :excl))

(eval-when (load)
  (provide :clxexcldep)
  (provide :clx))

(require :foreign)
(require :process)			; Needed even if scheduler is not
					; running.  (Must be able to make
					; a process-lock.)

(import '(excl::if*
	  excl::type-error
	  excl::type-error-datum
	  excl::type-error-expected-type))
#+allegro
(import '(excl::without-interrupts))

#-(or little-endian big-endian)
(eval-when (eval compile load)
  (let ((x '#(1)))
    (if (not (eq 0 (sys::memref x
				#.(comp::mdparam 'comp::md-svector-data0-adj)
				0 :unsigned-byte)))
	(pushnew :little-endian *features*)
      (pushnew :big-endian *features*))))


(defmacro define-condition (name (parent-type) &optional slots &rest args)
  `(excl::define-condition ,name (,parent-type) ,slots ,@args))


(defmacro correct-case (string)
  ;; This macro converts the given string to the 
  ;; current preferred case, or leaves it alone in a case-sensitive mode.
  (let ((str (gensym)))
    `(let ((,str ,string))
       (case excl::*current-case-mode*
	 (:case-insensitive-lower
	  (string-downcase ,str))
	 (:case-insensitive-upper
	  (string-upcase ,str))
	 ((:case-sensitive-lower :case-sensitive-upper)
	  ,str)))))


(defun underlying-simple-vector (array)
  (cond ((excl::svectorp array)
	 array)
	((arrayp array)
	 (cdr (excl::ah_data array)))
	(t
	 (error "~s is not an array" array))))


(defconstant type-pred-alist
  '(
    (card8  . card8p)
    (card16 . card16p)
    (card29 . card29p)
    (card32 . card32p)
    (int8   . int8p)
    (int16  . int16p)
    (int32  . int32p)
    (mask16 . card16p)
    (mask32 . card32p)
    (pixel  . card32p)
    (resource-id . card29p)
    (keysym . card32p)
    ))

;; This (if (and ...) t nil) stuff has a purpose -- it lets the old 
;; sun4 compiler opencode the `and'.

(defun card8p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (fixnump x) (> #.(expt 2 8) x) (>= x 0))
      t
    nil))

(defun card16p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (fixnump x) (> #.(expt 2 16) x) (>= x 0))
      t
    nil))

(defun card29p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (fixnump x) (>= (the fixnum x) 0))
	  (and (bignump x) (> #.(expt 2 29) (the bignum x))
	       (>= (the bignum x) 0)))
      t
    nil))

(defun card32p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (fixnump x) (>= (the fixnum x) 0))
	  (and (bignump x) (> #.(expt 2 32) (the bignum x))
	       (>= (the bignum x) 0)))
      t
    nil))

(defun int8p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (fixnump x) (> #.(expt 2 7) x) (>= x #.(expt -2 7)))
      t
    nil))

(defun int16p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (fixnump x) (> #.(expt 2 15) x) (>= x #.(expt -2 15)))
      t
    nil))

(defun int32p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (fixnump x)
	  (and (bignump x) (> #.(expt 2 31) (the bignum x))
	       (>= (the bignum x) #.(expt -2 31))))
      t
    nil))

(comp::def-tr comp::new-tr-typep typep (form type)
   (let (ent)
      (if* (and (consp type)
		(eq 'quote (car type))
		(consp (cdr type)))
	 then (setq ent (franz:assq (cadr type) type-pred-alist)))
      (if* ent
	 then `(,(cdr ent) ,form)
	 else (if* (and (consp type)
			(eq 'quote (car type))
			(consp (cdr type)))
		 then (setq ent (franz:assq (cadr type)
					    excl::type-pred-alist)))
	      (if* ent
		 then `(,(cdr ent) ,form)
		 else (comp::no-transform)))))


;; Return t if there is a character available for reading or on error,
;; otherwise return nil.
(defun fd-char-avail-p (fd)
  (multiple-value-bind (available-p errcode)
      (comp::.primcall-sargs 'sys::filesys excl::fs-char-avail fd)
    (if* errcode
       then t
       else available-p)))

(defmacro with-interrupt-checking-on (&body body)
  `(locally (declare (optimize (safety 1)))
     ,@body))

;; Read from the given fd into 'vector', which has element type card8.
;; Start storing at index 'start-index' and read exactly 'length' bytes.
;; Return t if an error or eof occurred, nil otherwise.
(defun fd-read-bytes (fd vector start-index length)
  (declare (fixnum fd start-index length)
	   (type (simple-array (unsigned-byte 8) (*)) vector))
  (with-interrupt-checking-on
   (do ((rest length))
       ((eq 0 rest) nil)
     (declare (fixnum rest))
     (multiple-value-bind (numread errcode)
	 (comp::.primcall-sargs 'sys::filesys excl::fs-read-bytes fd vector
				start-index rest)
       (declare (fixnum numread))
       (if* errcode
	  then (if (not (eq errcode
			    excl::*error-code-interrupted-system-call*))
		   (return t))
	elseif (eq 0 numread)
	  then (return t)
	  else (decf rest numread)
	       (incf start-index numread))))))


(when (plusp (ff:get-entry-points
	      (make-array 1 :initial-contents
			  (list (ff:convert-to-lang "fd_wait_for_input")))
	      (make-array 1 :element-type '(unsigned-byte 32))))
  (ff:remove-entry-point (ff:convert-to-lang "fd_wait_for_input"))
  (load "excldep.o"))

(when (plusp (ff:get-entry-points
	      (make-array 1 :initial-contents
			  (list (ff:convert-to-lang "connect_to_server")))
	      (make-array 1 :element-type '(unsigned-byte 32))))
  (ff:remove-entry-point (ff:convert-to-lang "connect_to_server" :language :c))
  (load "socket.o"))

(ff:defforeign-list `((connect-to-server
		       :entry-point
		       ,(ff:convert-to-lang "connect_to_server")
		       :return-type :fixnum
		       :arg-checking nil
		       :arguments (string fixnum))
		      (fd-wait-for-input
		       :entry-point ,(ff:convert-to-lang "fd_wait_for_input")
		       :return-type :fixnum
		       :arg-checking nil
		       :call-direct t
		       :callback nil
		       :allow-other-keys t
		       :arguments (fixnum fixnum))))


#-allegro
(defmacro without-interrupts (&body body)
  `(let ((excl::*without-interrupts* t)) ,@body))


(in-package :excl)

#-allegro
(defun type-array-element-type-to-array (type &aux temp) 
   ;; type is a type descriptor, return a descriptor which tells
   ;; the array code what kind of array to make

   ; convert the given element type to one of the symbols which
   ; is in the car of the array-descriptors list
   ;(msg "beginning type is " type 'N)
   (if* (symbolp type)
      then (if* (franz:memq type '(t bit string-char fixnum))
	      thenret	; it is ok as it is
	      else (let ((temp (get type 'deftype-expander)))
		     (if* temp
			then
			     (return-from type-array-element-type-to-array
			       (type-array-element-type-to-array
				(funcall temp (list type))))
			else
			     (setq type (case type
					  (standard-char 'string-char)
					  ((single-float short-float) 'single-float)
					  ((double-float long-float) 'double-float)
					  (t t))))))
    elseif (consp type)
      then (setq type
		 (case (car type)
		    (mod (if* (integerp (setq temp (cadr type)))
			    then (cond ((< temp 1) t)
				       ((<= temp 2) 'bit)
				       ((<= temp 256) 'ubyte)
				       ((<= temp 65536) 'uword)
				       ((<= temp 4294967296) 'ulong)
				       (t t))
			    else t))
		    (signed-byte
		       (if* (integerp (setq temp (cadr type)))
			  then (cond ((<= temp 0) t)
				     ((<= temp 8) 'byte)
				     ((<= temp 16) 'word)
				     ((<= temp 29) 'fixnum)
				     ((<= temp 32) 'long)
				     (t  t))
			  else t))
		    (unsigned-byte
		       (if* (integerp (setq temp (cadr type)))
			  then (cond ((<= temp 0) t)
				     ((<= temp 8) 'ubyte)
				     ((<= temp 16) 'uword)
				     ((<= temp 32) 'ulong)
				     (t  t))
			  else t))
		    (t t)))
      else (setq type t))
   ; type is now one of the valid types.  We return a descriptor
   ; based on that name
   ;(msg "resulting type is " type 'N)
   (let ((res (franz:assq type array-descriptors)))
      ;(msg " resulting decriptor " res 'N)
      res))

#-allegro
(defun make-sequence (type length &rest rest &key initial-element)
  "Returns a sequence of the given Type and Length, with elements initialized
  to :Initial-Element."
  (declare (fixnum length)
	   (ignore initial-element))
  (case (type-specifier type)
    (list (apply #'make-list length rest))
    ((simple-string string)
     (apply #'make-string length rest))
    ((array simple-array vector simple-vector)
     (if* (listp type)
	 then (apply #'make-array length :element-type (cadr type) rest)
	 else (apply #'make-array length rest)))
    ((bit-vector simple-bit-vector)
     (apply #'make-array length :element-type 'bit rest))
    (t
     ;; Now, we can either have a user-defined type symbol, or an error.
     (if* (symbolp type)
	then (let ((temp (get type 'excl::deftype-expander)))
	       (if* temp
		  then (cond (rest (return-from make-sequence
				     (make-sequence (funcall temp (list type)) length
						   :initial-element (cadr rest))))
			      (t (return-from make-sequence (make-sequence
				   (funcall temp (list type)) length)))))))
     (error "~s is a bad type specifier for sequences." type ))))

;; special patch for CLX (various process fixes)
;; patch1000.2

(in-package 'patch :use '(lisp excl))

(defvar *patches* nil)

#+allegro
(eval-when (compile eval load)
  (when (and (= excl::cl-major-version-number 3)
	     (or (= excl::cl-minor-version-number 0)
		 (and (= excl::cl-minor-version-number 1)
		      (< excl::cl-generation-number 9))))
    (push :clx-r4-process-patches *features*)))

#+clx-r4-process-patches
(push (cons 1000.2 "special patch for CLX (various process fixes)")
      *patches*)


(in-package :mp)

#+clx-r4-process-patches
(export 'wait-for-input-available)


#+clx-r4-process-patches
(defun with-timeout-event (seconds fnc args)
  (unless *scheduler-stack-group* (start-scheduler)) ;[spr670]
  (let ((clock-event (make-clock-event)))
    (when (<= seconds 0) (setq seconds 0))
    (multiple-value-bind (secs msecs) (truncate seconds)
      ;; secs is now a nonegative integer, and msecs is either fixnum zero
      ;; or else something interesting.
      (unless (eq 0 msecs)
	(setq msecs (truncate (* 1000.0 msecs))))
      ;; Now msecs is also a nonnegative fixnum.
      (multiple-value-bind (now mnow) (excl::cl-internal-real-time)
	(incf secs now)
	(incf msecs mnow)
	(when (>= msecs 1000)
	  (decf msecs 1000)
	  (incf secs))
	(unless (fixnump secs) (setq secs most-positive-fixnum))
	(setf (clock-event-secs clock-event) secs
	      (clock-event-msecs clock-event) msecs
	      (clock-event-function clock-event) fnc
	      (clock-event-args clock-event) args)))
    clock-event))


#+clx-r4-process-patches
(defmacro with-timeout ((seconds &body timeout-body) &body body)
  `(let* ((clock-event (with-timeout-event ,seconds
					   #'process-interrupt
					   (cons *current-process*
						 '(with-timeout-internal))))
	  (excl::*without-interrupts* t)
	  ret)
     (unwind-protect
	 ;; Warning: Branch tensioner better not reorder this code!
	 (setq ret (catch 'with-timeout-internal
		     (add-to-clock-queue clock-event)
		     (let ((excl::*without-interrupts* nil))
		       (multiple-value-list (progn ,@body)))))
       (if* (eq ret 'with-timeout-internal)
	  then (let ((excl::*without-interrupts* nil))
		 (setq ret (multiple-value-list (progn ,@timeout-body))))
	  else (remove-from-clock-queue clock-event)))
     (values-list ret)))


#+clx-r4-process-patches
(defun process-lock (lock &optional (lock-value *current-process*)
				    (whostate "Lock") timeout)
  (declare (optimize (speed 3)))
  (unless (process-lock-p lock)
    (error "First argument to PROCESS-LOCK must be a process-lock: ~s" lock))
  (without-interrupts
   (if* (null (process-lock-locker lock))
      then (setf (process-lock-locker lock) lock-value)
      else (if* timeout
	      then (if* (or (eq 0 timeout) ;for speed
			    (zerop timeout))
		      then nil
		      else (with-timeout (timeout)
			     (process-lock-1 lock lock-value whostate)))
	      else (process-lock-1 lock lock-value whostate)))))


#+clx-r4-process-patches
(defun process-lock-1 (lock lock-value whostate)
  (declare (type process-lock lock)
	   (optimize (speed 3)))
  (let ((process *current-process*))
    (declare (type process process))
    (unless process
      (error
       "PROCESS-LOCK may not be called on the scheduler's stack group."))
    (loop (unless (process-lock-locker lock)
	    (return (setf (process-lock-locker lock) lock-value)))
      (push process (process-lock-waiting lock))
      (let ((saved-whostate (process-whostate process)))
	(unwind-protect
	    (progn (setf (process-whostate process) whostate)
		   (process-add-arrest-reason process lock))
	  (setf (process-whostate process) saved-whostate))))))


#+clx-r4-process-patches
(defun process-wait (whostate function &rest args)
  (declare (optimize (speed 3)))
  ;; Run the wait function once here both for efficiency and as a
  ;; first line check for errors in the function.
  (unless (apply function args)
    (process-wait-1 whostate function args)))


#+clx-r4-process-patches
(defun process-wait-1 (whostate function args)
  (declare (optimize (speed 3)))
  (let ((process *current-process*))
    (declare (type process process))
    (unless process
      (error
       "Process-wait may not be called within the scheduler's stack group."))
    (let ((saved-whostate (process-whostate process)))
      (unwind-protect
	  (without-scheduling-internal
	   (without-interrupts
	    (setf (process-whostate process) whostate
		  (process-wait-function process) function
		  (process-wait-args process) args)
	    (chain-rem-q process)
	    (chain-ins-q process *waiting-processes*))
	   (process-resume-scheduler nil))
	(setf (process-whostate process) saved-whostate
	      (process-wait-function process) nil
	      (process-wait-args process) nil)))))


#+clx-r4-process-patches
(defun process-wait-with-timeout (whostate seconds function &rest args)
  ;; Now returns T upon completion, NIL upon timeout. -- 6Jun89 smh
  ;; [spr1135] [rfe939] Timeout won't throw out of interrupt level code.
  ;;  -- 28Feb90 smh
  ;; Run the wait function once here both for efficiency and as a
  ;; first line check for errors in the function.
  (if* (apply function args)
     then t
     else (let ((ret (list nil)))
            (without-interrupts
             (let ((clock-event
                    (with-timeout-event seconds #'identity '(nil))))
               (add-to-clock-queue clock-event)
               (process-wait-1 whostate
                               #'(lambda (clock-event function args ret)
                                   (or (null (chain-next clock-event))
                                       (and (apply function args)
                                            (setf (car ret) 't))))
                               (list clock-event function args ret))))
            (car ret))))


;;
;; Returns nil on timeout, otherwise t.
;;
#+clx-r4-process-patches
(defun wait-for-input-available
    (stream-or-fd &key (wait-function #'listen)
		       (whostate "waiting for input")
		       timeout)
  (let ((fd (if* (fixnump stream-or-fd) then stream-or-fd
	     elseif (streamp stream-or-fd)
	       then (excl::stream-input-fn stream-or-fd)
	       else (error "wait-for-input-available expects a stream or file descriptor: ~s" stream-or-fd))))
    ;; At this point fd could be nil, since stream-input-fn returns nil for
    ;; streams that are output only, or for certain special purpose streams.
    (if fd
	(unwind-protect
	    (progn
	      (mp::mpwatchfor fd)
	      (if* timeout
		 then (mp::process-wait-with-timeout
		       whostate timeout wait-function stream-or-fd)
		 else (mp::process-wait whostate wait-function stream-or-fd)
		      t))
	  (mp::mpunwatchfor fd))
      (if* timeout
	 then (mp::process-wait-with-timeout
	       whostate timeout wait-function stream-or-fd)
	 else (mp::process-wait whostate wait-function stream-or-fd)
	      t))))
