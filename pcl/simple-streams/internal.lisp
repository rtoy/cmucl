;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/internal.lisp,v 1.6 2007/10/25 15:17:07 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Various functions needed by simple-streams

(in-package "STREAM")

(declaim (inline buffer-sap bref (setf bref) buffer-copy
		 allocate-buffer free-buffer))

(defun buffer-sap (thing &optional offset)
  (declare (type simple-stream-buffer thing) (type (or fixnum null) offset)
           (optimize (speed 3) (space 2) (debug 0) (safety 0)
                     ;; Suppress the note about having to box up the return:
                     (ext:inhibit-warnings 3)))
  (let ((sap (if (vectorp thing) (sys:vector-sap thing) thing)))
    (if offset (sys:sap+ sap offset) sap)))

(defun bref (buffer index)
  (declare (type simple-stream-buffer buffer)
           (type (integer 0 #.most-positive-fixnum) index))
  (sys:sap-ref-8 (buffer-sap buffer) index))

(defun (setf bref) (octet buffer index)
  (declare (type (unsigned-byte 8) octet)
           (type simple-stream-buffer buffer)
           (type (integer 0 #.most-positive-fixnum) index))
  (setf (sys:sap-ref-8 (buffer-sap buffer) index) octet))

(defun buffer-copy (src soff dst doff length)
  (declare (type simple-stream-buffer src dst)
           (type fixnum soff doff length))
  (sys:without-gcing ;; is this necessary??
   (kernel:system-area-copy (buffer-sap src) (* soff 8)
                            (buffer-sap dst) (* doff 8)
                            (* length 8))))

(defun allocate-buffer (size)
  (if (= size lisp::bytes-per-buffer)
      (lisp::next-available-buffer)
      (make-array size :element-type '(unsigned-byte 8))))

(defun free-buffer (buffer)
  (when (sys:system-area-pointer-p buffer)
    (push buffer lisp::*available-buffers*))
  t)



(defun make-control-table (&rest inits)
  (let ((table (make-array 32 :initial-element nil)))
    (do* ((char (pop inits) (pop inits))
	  (func (pop inits) (pop inits)))
	 ((null char))
      (when (< (char-code char) 32)
	(setf (aref table (char-code char)) func)))
    table))

(defun std-newline-out-handler (stream character)
  (declare (ignore character))
  (with-stream-class (simple-stream stream)
    (setf (sm charpos stream) -1)
    nil))

(defun std-tab-out-handler (stream character)
  (declare (ignore character))
  (with-stream-class (simple-stream stream)
    (let ((col (sm charpos stream)))
      (when col
	(setf (sm charpos stream) (1- (* 8 (1+ (floor col 8)))))))
    nil))

(defun std-dc-newline-in-handler (stream character)
  (with-stream-class (dual-channel-simple-stream stream)
    (setf (sm charpos stream) -1) ;; set to 0 "if reading" ???
    character))

(defvar *std-control-out-table*
  (make-control-table #\Newline #'std-newline-out-handler
		      #\Tab     #'std-tab-out-handler))



#-(or big-endian little-endian)
(eval-when (:compile-toplevel)
  (push (c::backend-byte-order c::*target-backend*) *features*))

(defun vector-elt-width (vector)
  ;; Return octet-width of vector elements
  (etypecase vector
    ;; (simple-array fixnum (*)) not supported
    ;; (simple-array base-char (*)) treated specially; don't call this
    ((simple-array bit (*)) 1)
    ((simple-array (unsigned-byte 2) (*)) 1)
    ((simple-array (unsigned-byte 4) (*)) 1)
    ((simple-array (signed-byte 8) (*)) 1)
    ((simple-array (unsigned-byte 8) (*)) 1)
    ((simple-array (signed-byte 16) (*)) 2)
    ((simple-array (unsigned-byte 16) (*)) 2)
    ((simple-array (signed-byte 32) (*)) 4)
    ((simple-array (unsigned-byte 32) (*)) 4)
    ((simple-array single-float (*)) 4)
    ((simple-array double-float (*)) 8)
    ((simple-array (complex single-float) (*)) 8)
    ((simple-array (complex double-float) (*)) 16)))

(defun endian-swap-value (vector endian-swap)
  (case endian-swap
    (:network-order #+big-endian 0
		    #+little-endian (1- (vector-elt-width vector)))
    (:byte-8 0)
    (:byte-16 1)
    (:byte-32 3)
    (:byte-64 7)
    (:byte-128 15)
    (otherwise endian-swap)))


#+(or)
(defun %read-vector (vector stream start end endian-swap blocking)
  (declare (type (kernel:simple-unboxed-array (*)) vector)
	   (type stream stream))
  ;; move code from read-vector
  )

#+(or)
(defun %write-vector (... blocking)
  ;; implement me
  )

(defun read-octets (stream buffer start end blocking)
  (declare (type simple-stream stream)
	   (type (or null simple-stream-buffer) buffer)
	   (type fixnum start)
	   (type (or null fixnum) end)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (with-stream-class (simple-stream stream)
    (let ((fd (sm input-handle stream))
	  (end (or end (sm buf-len stream)))
	  (buffer (or buffer (sm buffer stream))))
      (declare (fixnum end))
      (typecase fd
	(fixnum
	 (let ((flag #+MP (mp:process-wait-until-fd-usable fd :input
							   (if blocking nil 0))
		     #-MP (sys:wait-until-fd-usable fd :input
						    (if blocking nil 0))))
	   (cond
	     ((and (not blocking) (= start end)) (if flag -3 0))
	     ((and (not blocking) (not flag)) 0)
	     (t (block nil
		  (let ((count 0))
		    (declare (type fixnum count))
		    (tagbody
		     again
		       ;; Avoid CMUCL gengc write barrier
		       (do ((i start (+ i #.(unix:unix-getpagesize))))
			   ((>= i end))
			 (declare (type fixnum i))
			 (setf (bref buffer i) 0))
		       (setf (bref buffer (1- end)) 0)
		       (multiple-value-bind (bytes errno)
			   (unix:unix-read fd (buffer-sap buffer start)
					   (the fixnum (- end start)))
			 (declare (type (or null fixnum) bytes)
				  (type (integer 0 100) errno))
			 (when bytes
			   (incf count bytes)
			   (incf start bytes))
			 (cond ((null bytes)
				(format t "~&;; UNIX-READ: errno=~D~%" errno)
				(cond ((= errno unix:eintr) (go again))
				      ((and blocking
					    (or (= errno unix:eagain)
						(= errno unix:ewouldblock)))
				       #+MP
				       (mp:process-wait-until-fd-usable fd
									:input
									nil)
				       #-MP
				       (sys:wait-until-fd-usable fd :input nil)
				       (go again))
				      (t (return (- -10 errno)))))
			       ((zerop count) (return -1))
			       (t (return count)))))))))))
	(t (%read-vector buffer fd start end :byte-8
			 (if blocking :bnb nil)))))))

(defun write-octets (stream buffer start end blocking)
  (declare (type simple-stream stream)
	   (type simple-stream-buffer buffer)
	   (type fixnum start)
	   (type (or null fixnum) end))
  (with-stream-class (simple-stream stream)
    (when (sm handler stream)
      (do ()
	  ((null (sm pending stream)))
	(system:serve-all-events)))

    (let ((fd (sm output-handle stream))
	  (end (or end (length buffer))))
      (typecase fd
	(fixnum
	 (let ((flag #+MP (mp:process-wait-until-fd-usable fd :output
							   (if blocking nil 0))
		     #-MP (sys:wait-until-fd-usable fd :output
						    (if blocking nil 0))))
	   (cond
	     ((and (not blocking) (= start end)) (if flag -3 0))
	     ((and (not blocking) (not flag)) 0)
	     (t
	      (block nil
		(let ((count 0))
		  (tagbody again
		     (multiple-value-bind (bytes errno)
			 (unix:unix-write fd (buffer-sap buffer) start
					  (- end start))
		       (when bytes
			 (incf count bytes)
			 (incf start bytes))
		       (cond ((null bytes)
			      (format t "~&;; UNIX-WRITE: errno=~D~%" errno)
			      (cond ((= errno unix:eintr) (go again))
				    ;; don't block for subsequent chars
				    (t (return (- -10 errno)))))
			     (t (return count)))))))))))
	(t (error "implement me"))))))



(defun do-some-output (stream)
  ;; Do some pending output; return T if completed, NIL if more to do
  (with-stream-class (simple-stream stream)
    (let ((fd (sm output-handle stream)))
      (loop
	(let ((list (pop (sm pending stream))))
	  (unless list
	    (sys:remove-fd-handler (sm handler stream))
	    (setf (sm handler stream) nil)
	    (return t))
	  (let* ((buffer (first list))
		 (start (second list))
		 (end (third list))
		 (len (- end start)))
	    (declare (type simple-stream-buffer buffer)
		     (type lisp::index start end len))
	    (tagbody again
	       (multiple-value-bind (bytes errno)
		   (unix:unix-write fd (buffer-sap buffer) start len)
		 (cond ((null bytes)
			(if (= errno unix:eintr)
			    (go again)
			    (progn (push list (sm pending stream))
				   (return nil))))
		       ((< bytes len)
			(setf (second list) (+ start bytes))
			(push list (sm pending stream))
			(return nil))
		       ((= bytes len)
			(free-buffer buffer)))))))))))

(defun queue-write (stream buffer start end)
  ;; Queue a write; return T if buffer needs changing, NIL otherwise
  (declare (type simple-stream stream)
	   (type simple-stream-buffer buffer)
	   (type lisp::index start end))
  (with-stream-class (simple-stream stream)
    (when (sm handler stream)
      (unless (do-some-output stream)
	(let ((last (last (sm pending stream))))
	  (setf (cdr last) (list (list buffer start end)))
	  (return-from queue-write t))))
    (let ((bytes (write-octets stream buffer start end nil)))
      (unless (or (= bytes (- end start)) ; completed
		  (= bytes -3))	; empty buffer; shouldn't happen
	(setf (sm pending stream) (list (list buffer start end)))
	(setf (sm handler stream)
	      (sys:add-fd-handler (sm output-handle stream) :output
				  (lambda (fd)
				    (declare (ignore fd))
				    (do-some-output stream))))
	t))))
