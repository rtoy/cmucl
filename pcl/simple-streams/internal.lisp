;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/internal.lisp,v 1.3 2003/06/18 09:23:08 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Various functions needed by simple-streams

(in-package "STREAM")

(declaim (inline buffer-sap bref (setf bref) buffer-copy))

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



(defun find-external-format (name)
  nil)



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
    (:network-order (1- (vector-elt-width vector)))
    (:byte-8 0)
    (:byte-16 1)
    (:byte-32 3)
    (:byte-64 7)
    (:byte-128 15)
    (otherwise endian-swap)))



(defun read-vector (vector stream &key (start 0) end (endian-swap :byte-8))
  (declare (type (kernel:simple-unboxed-array (*)) vector)
	   (type stream stream))
  ;; START and END are octet offsets, not vector indices!  [Except for strings]
  ;; Return value is index of next octet to be read into (i.e., start+count)
  (etypecase stream
    (simple-stream
     (with-stream-class (simple-stream stream)
       (if (stringp vector)
	   (let* ((start (or start 0))
		  (end (or end (length vector)))
		  (char (funcall-stm-handler j-read-char stream nil nil t)))
	     (when char
	       (setf (schar vector start) char)
	       (incf start)
	       (+ start (funcall-stm-handler j-read-chars stream vector nil
					     start end nil))))
	   (do* ((j-read-byte
		  (cond ((any-stream-instance-flags stream :string)
			 (error "Can't READ-BYTE on string streams."))
			((any-stream-instance-flags stream :dual)
			 #'dc-read-byte)
			(t
			 #'sc-read-byte)))
		 (index (or start 0) (1+ index))
		 (end (or end (* (length vector) (vector-elt-width vector))))
		 (endian-swap (endian-swap-value vector endian-swap))
		 (byte (funcall j-read-byte stream nil nil t)
		       (funcall j-read-byte stream nil nil nil)))
		((or (null byte) (>= index end)) index)
	     (setf (bref vector (logxor index endian-swap)) byte)))))
    ((or lisp-stream #+GRAY-STREAMS fundamental-stream)
     (unless (typep vector '(or string
			     (simple-array (signed-byte 8) (*))
			     (simple-array (unsigned-byte 8) (*))))
       (error "Bad vector."))
     (read-sequence vector stream :start (or start 0) :end end))))

#|(defun write-vector ...)|#



(defun read-octets (stream buffer start end blocking)
  (declare (type simple-stream stream)
	   (type (or null simple-stream-buffer) buffer)
	   (type fixnum start)
	   (type (or null fixnum) end)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (with-stream-class (single-channel-simple-stream stream) ;@@
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
	(t (error "implement me"))))))

(defun write-octets (stream buffer start end blocking)
  (declare (type simple-stream stream)
	   (type (or null simple-stream-buffer) buffer)
	   (type fixnum start)
	   (type (or null fixnum) end))
  (with-stream-class (simple-stream stream)
    (let ((fd (sm output-handle stream))
	  (end (or end (error "WRITE-OCTETS: end=NIL")))
	  (buffer (or buffer (error "WRITE-OCTETS: buffer=NIL"))))
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
