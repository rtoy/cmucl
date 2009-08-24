;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/strategy.lisp,v 1.21 2009/08/24 16:04:26 rtoy Rel $")
;;;
;;; **********************************************************************
;;;
;;; Strategy functions for base simple-stream classes

(in-package "STREAM")

;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ext:define-function-name-syntax sc (name)
    ;; (sc <name> <external-format> [<access>])
    (if (and (<= 3 (length name) 4)
	     (symbolp (second name))
	     (keywordp (third name)))
	(values t (second name))
	(values nil nil)))

  (ext:define-function-name-syntax dc (name)
    ;; (dc <name> <external-format>)
    (if (and (= (length name) 3)
	     (symbolp (second name))
	     (keywordp (third name)))
	(values t (second name))
	(values nil nil)))

  (ext:define-function-name-syntax str (name)
    ;; (str <name> [<composing-format>])
    (if (and (<= 2 (length name) 3)
	     (symbolp (second name))
	     (or (null (third name))
		 (keywordp (third name))))
	(values t (second name))
	(values nil nil))))


;;;; Helper functions
(defun refill-buffer (stream blocking)
  (with-stream-class (simple-stream stream)
    (let* ((unread (sm last-char-read-size stream))
           (buffer (sm buffer stream))
	   (bufptr (sm buffer-ptr stream)))
      (unless (or (zerop unread) (zerop bufptr))
        (buffer-copy buffer (- bufptr unread) buffer 0 unread))
      (let ((bytes (device-read stream nil unread nil blocking)))
        (declare (type fixnum bytes))
        (setf (sm buffpos stream) unread
              (sm buffer-ptr stream) (if (plusp bytes)
                                         (+ bytes unread)
                                         unread))
        bytes))))

(defun sc-set-dirty (stream)
  (with-stream-class (single-channel-simple-stream stream)
    (setf (sm mode stream)
          (if (<= (sm buffpos stream)
                  (sm buffer-ptr stream))
              3    ; read-modify
              1    ; write
              ))))

(defun sc-set-clean (stream)
  (with-stream-class (single-channel-simple-stream stream)
    (setf (sm mode stream) 0)))

(defun sc-dirty-p (stream)
  (with-stream-class (single-channel-simple-stream stream)
    (> (sm mode stream) 0)))

(defun flush-buffer (stream blocking)
  (with-stream-class (single-channel-simple-stream stream)
    (let ((ptr 0)
          (bytes (sm buffpos stream)))
      (declare (type fixnum ptr bytes))
      (when (and (> (sm mode stream) 0)
		 (> (sm buffer-ptr stream) 0))
        ;; The data read in from the file could have been changed if
        ;; the stream is opened in read-write mode -- write back
        ;; everything in the buffer at the correct position just in
        ;; case.
        (setf (device-file-position stream)
              (- (device-file-position stream) (sm buffer-ptr stream))))
      (loop
	(when (>= ptr bytes)
	  (setf (sm buffpos stream) 0)
	  (setf (sm mode stream) 0)
	  (return 0))
        (let ((bytes-written (device-write stream nil ptr nil blocking)))
          (declare (fixnum bytes-written))
          (when (minusp bytes-written)
            (error "DEVICE-WRITE error."))
          (incf ptr bytes-written))))))

(defun flush-out-buffer (stream blocking)
  (with-stream-class (dual-channel-simple-stream stream)
    (let ((ptr 0)
          (bytes (sm outpos stream)))
      (declare (type fixnum ptr bytes))
      (loop
        (when (>= ptr bytes) (setf (sm outpos stream) 0) (return 0))
        (let ((bytes-written (device-write stream nil ptr nil blocking)))
          (declare (fixnum bytes-written))
          (when (minusp bytes-written)
            (error "DEVICE-WRITE error."))
          (incf ptr bytes-written))))))

(defun read-byte-internal (stream eof-error-p eof-value blocking)
  (with-stream-class (simple-stream stream)
    (let ((ptr (sm buffpos stream)))
      (when (>= ptr (sm buffer-ptr stream))
        (let ((bytes (device-read stream nil 0 nil blocking)))
          (declare (type fixnum bytes))
          (if (plusp bytes)
              (setf (sm buffer-ptr stream) bytes
                    ptr 0)
              (return-from read-byte-internal
                (lisp::eof-or-lose stream eof-error-p eof-value)))))
      (setf (sm buffpos stream) (1+ ptr))
      (setf (sm last-char-read-size stream) 0)
      (bref (sm buffer stream) ptr))))



(defconstant +ss-ef-strlen+ 0)
(defconstant +ss-ef-listen+ 1)
(defconstant +ss-ef-listen-mmapped+ 2)
(defconstant +ss-ef-cin+ 3)
(defconstant +ss-ef-cin-mmapped+ 4)
(defconstant +ss-ef-sin+ 5)
(defconstant +ss-ef-sin-mmapped+ 6)
(defconstant +ss-ef-unread+ 7)
(defconstant +ss-ef-out-sc+ 8)
(defconstant +ss-ef-sout-sc+ 9)
(defconstant +ss-ef-out-dc+ 10)
(defconstant +ss-ef-sout-dc+ 11)
(defconstant +ss-ef-max+ 12)

;; ef-strlen implements file-string-length (see impl.lisp)
(def-ef-macro ef-strlen (ef simple-streams +ss-ef-max+ +ss-ef-strlen+)
  (if (= (ef-min-octets (find-external-format ef))
	 (ef-max-octets (find-external-format ef)))
      `(lambda (stream object)
	 (declare (ignore stream))
	 (etypecase object
	   (character ,(ef-min-octets (find-external-format ef)))
	   (string (* ,(ef-min-octets (find-external-format ef))
		      (length object)))))
      `(lambda (stream object &aux (count 0))
	 (labels ((eflen (char)
		    ;;@@ FIXME: don't mess up stream state
		    (char-to-octets ,ef char (sm co-state stream)
				    (lambda (octet)
				      (declare (ignore octet))
				      (incf count)))))
	   (declare (dynamic-extent #'eflen))
	   (etypecase object
	     (character (eflen object))
	     (string (dovector (ch object) (eflen ch)))))
	 count)))

(def-ef-macro (sc listen :ef) (ef simple-streams +ss-ef-max+ +ss-ef-listen+)
  `(lambda (stream)
     (with-stream-class (simple-stream stream)
       (let ((lcrs (sm last-char-read-size stream))
	     (buffer (sm buffer stream))
	     (buffpos (sm buffpos stream))
	     (cnt 0))
	 (if (>= (- (sm buffer-ptr stream) buffpos)
		 ,(ef-max-octets (find-external-format ef)))
	     t
	     ;;@@ FIXME: don't mess up stream state
	     (and (octets-to-char ,ef (sm oc-state stream) cnt
		      (prog2
			  (when (>= buffpos (sm buffer-ptr stream))
			    (setf (sm last-char-read-size stream)
				(+ lcrs cnt))
			    (let ((bytes (refill-buffer stream nil)))
			      (setf (sm last-char-read-size stream) lcrs)
			      (cond ((= bytes 0)	 ; no data available
				     (return-from listen nil))
				    ((< bytes 0)	 ; would block
				     (return-from listen t))
				    ((>= (+ bytes cnt)
					 ,(ef-max-octets
					   (find-external-format ef)))
				     (return-from listen nil))
				    (t
				     (setf buffpos
					 (sm buffpos stream))))))
			  (bref buffer buffpos)
			(incf buffpos))
		      (lambda (n)
			(decf buffpos n)))
		  nil))))))

;; for memory-mapped streams, READ-CHAR will never block:
(def-ef-macro (sc listen :ef mapped) (ef simple-stream +ss-ef-max+
					 +ss-ef-listen-mmapped+)
  `(lambda (stream)
     (declare (ignore stream))
     t))

(def-ef-macro (sc read-char :ef) (ef simple-streams +ss-ef-max+ +ss-ef-cin+)
  `(lambda (stream eof-error-p eof-value blocking)
     (with-stream-class (simple-stream stream)
       (let* ((buffer (sm buffer stream))
	      (buffpos (sm buffpos stream))
	      (char (octets-to-char ,ef (sm oc-state stream)
			(sm last-char-read-size stream)
			(prog2
			    (when (>= buffpos (sm buffer-ptr stream))
			      (let ((bytes (refill-buffer stream blocking)))
				(cond ((= bytes 0)
				       (return-from read-char nil))
				      ((< bytes 0)
				       (return-from read-char
					 (lisp::eof-or-lose stream
							    eof-error-p
							    eof-value))))
				(setf buffpos (sm buffpos stream))))
			    (bref buffer buffpos)
			  (incf buffpos))
			(lambda (n) (decf buffpos n))))
	      (code (char-code char))
	      (ctrl (sm control-in stream)))
	 (setf (sm buffpos stream) buffpos)
	 (when (and (< code 32) ctrl (svref ctrl code))
	   (setq char (funcall (the (or symbol function) (svref ctrl code))
			       stream char)))
	 (if (null char)
	     (lisp::eof-or-lose stream eof-error-p eof-value)
	     char)))))

(def-ef-macro (sc read-char :ef mapped) (ef simple-streams +ss-ef-max+
					    +ss-ef-cin-mmapped+)
  `(lambda (stream eof-error-p eof-value blocking)
     (declare (ignore blocking))
     (with-stream-class (simple-stream stream)
       (let* ((buffer (sm buffer stream))
	      (buffpos (sm buffpos stream))
	      (char (octets-to-char ,ef (sm oc-state stream)
			(sm last-char-read-size stream)
			(prog2
			    (when (>= buffpos (sm buf-len stream))
			      (return-from read-char
				(lisp::eof-or-lose stream
						   eof-error-p eof-value)))
			    (bref buffer buffpos)
			  (incf buffpos))
			(lambda (n) (decf buffpos n))))
	      (code (char-code char))
	      (ctrl (sm control-in stream)))
	 (setf (sm buffpos stream) buffpos)
	 (when (and (< code 32) ctrl (svref ctrl code))
	   (setq char (funcall (the (or symbol function) (svref ctrl code))
			       stream char)))
	 (if (null char)
	     (lisp::eof-or-lose stream eof-error-p eof-value)
	     char)))))

(def-ef-macro (sc read-chars :ef) (ef simple-streams +ss-ef-max+ +ss-ef-sin+)
  `(lambda (stream string search start end blocking)
     ;; string is filled from START to END, or until SEARCH is found
     ;; Return two values: count of chars read and
     ;;  NIL if SEARCH was not found
     ;;  T if SEARCH was found
     ;;  :EOF if eof encountered before end
     (with-stream-class (simple-stream stream)
       ;; if stream is single-channel and mode == 3, flush buffer (if dirty)
       (do ((buffer (sm buffer stream))
	    (buffpos (sm buffpos stream))
	    (buffer-ptr (sm buffer-ptr stream))
	    (lcrs 0)
	    (ctrl (sm control-in stream))
	    (posn start (1+ posn))
	    (count 0 (1+ count)))
	   ((>= posn end)
	    (setf (sm buffpos stream) buffpos
		  (sm last-char-read-size stream) lcrs)
	    (values count nil))
	 (declare (type lisp::index buffpos buffer-ptr posn count))
	 (let* ((cnt 0)
		(char (octets-to-char ,ef (sm oc-state stream) cnt
			  (prog2
			      (when (>= buffpos buffer-ptr)
				(setf (sm last-char-read-size stream) lcrs)
				(let ((bytes (refill-buffer stream blocking)))
				  (setf buffpos (sm buffpos stream)
					buffer-ptr (sm buffer-ptr stream))
				  (unless (plusp bytes)
				    (if (zerop bytes)
					(return (values count nil))
					(return (values count :eof))))))
			      (bref buffer buffpos)
			    (incf buffpos))
			  (lambda (n) (decf buffpos n))))
		(code (char-code char)))
	   (setq lcrs cnt)
	   (when (and (< code 32) ctrl (svref ctrl code))
	     (setq char (funcall (the (or symbol function) (svref ctrl code))
				 stream char)))
	   (cond ((null char)
		  (setf (sm buffpos stream) buffpos
			(sm last-char-read-size stream) lcrs)
		  (return (values count :eof)))
		 ((and search (char= char search))
		  (setf (sm buffpos stream) buffpos
			(sm last-char-read-size stream) lcrs)
		  (return (values count t)))
		 (t
		  (setf (char string posn) char))))))))

(def-ef-macro (sc read-chars :ef mapped) (ef simple-streams +ss-ef-max+
					     +ss-ef-sin-mmapped+)
  `(lambda (stream string search start end blocking)
     (with-stream-class (simple-stream stream)
       (do ((buffer (sm buffer stream))
	    (buffpos (sm buffpos stream))
	    (buf-len (sm buf-len stream))
	    (lcrs 0)
	    (ctrl (sm control-in stream))
	    (posn start (1+ posn))
	    (count 0 (1+ count)))
	   ((>= posn end)
	    (setf (sm buffpos stream) buffpos
		  (sm last-char-read-size stream) lcrs)
	    (values count nil))
	 (declare (type lisp::index buffpos buffer-ptr posn count))
	 (let* ((cnt 0)
		(char (octets-to-char ,ef (sm oc-state stream) cnt
			  (prog2
			      (when (>= buffpos buf-len)
				(return (values count :eof)))
			      (bref buffer buffpos)
			    (incf buffpos))
			  (lambda (n) (decf buffpos n))))
		(code (char-code char)))
	   (setq lcrs cnt)
	   (when (and (< code 32) ctrl (svref ctrl code))
	     (setq char (funcall (the (or symbol function) (svref ctrl code))
				 stream char)))
	   (cond ((null char)
		  (setf (sm buffpos stream) buffpos
			(sm last-char-read-size stream) lcrs)
		  (return (values count :eof)))
		 ((and search (char= char search))
		  (setf (sm buffpos stream) buffpos
			(sm last-char-read-size stream) lcrs)
		  (return (values count t)))
		 (t
		  (setf (char string posn) char))))))))

(def-ef-macro (sc unread-char :ef) (ef simple-streams +ss-ef-max+
				       +ss-ef-unread+)
  `(lambda (stream relaxed)
     (declare (ignore relaxed))
     (with-stream-class (simple-stream stream)
       (let ((unread (sm last-char-read-size stream)))
	 (if (>= (sm buffpos stream) unread)
	     (decf (sm buffpos stream) unread)
	     (error "This shouldn't happen."))))))

(def-ef-macro (sc write-char :ef) (ef simple-streams +ss-ef-max+
				      +ss-ef-out-sc+)
  `(lambda (character stream)
     (with-stream-class (single-channel-simple-stream stream)
       (when character
	 (let ((code (char-code character))
	       (ctrl (sm control-out stream)))
	   (unless (and (< code 32) ctrl (svref ctrl code)
			(funcall (the (or symbol function) (svref ctrl code))
				 stream character))
	     (char-to-octets ,ef character (sm co-state stream)
		(lambda (byte)
		  (when (>= (sm buffpos stream) (sm buffer-ptr stream))
		    (setf (sm buffpos stream) (flush-buffer stream t)))
		  (setf (bref (sm buffer stream) (sm buffpos stream)) byte)
		  (incf (sm buffpos stream))))
	     (sc-set-dirty stream)
	     (when (sm charpos stream)
	       (incf (sm charpos stream))))))
       character)))

(def-ef-macro (sc write-chars :ef) (ef simple-streams +ss-ef-max+
				       +ss-ef-sout-sc+)
  `(lambda (string stream start end)
     (with-stream-class (single-channel-simple-stream stream)
       (do ((buffer (sm buffer stream))
	    (buffpos (sm buffpos stream))
	    (buf-len (sm buf-len stream))
	    (ef (sm external-format stream))
	    (ctrl (sm control-out stream))
	    (posn start (1+ posn))
	    (count 0 (1+ count)))
	   ((>= posn end) (setf (sm buffpos stream) buffpos) count)
	 (declare (type fixnum buffpos buf-len posn count))
	 (let* ((char (char string posn))
		(code (char-code char)))
	   (unless (and (< code 32) ctrl (svref ctrl code)
			(funcall (the (or symbol function) (svref ctrl code))
				 stream char))
	     (char-to-octets ,ef char (sm co-state stream)
		(lambda (byte)
		  (when (>= buffpos buf-len)
		    (setf (sm buffpos stream) buffpos)
		    (setq buffpos (flush-buffer stream t)))
		  (setf (bref buffer buffpos) byte)
		  (incf buffpos)))
	     (setf (sm buffpos stream) buffpos)
	     (when (sm charpos stream)
	       (incf (sm charpos stream)))
	     (sc-set-dirty stream)))))))


;;;; Dual-Channel-Simple-Stream strategy functions

;; single-channel read-side functions work for dual-channel streams too

(def-ef-macro (dc write-char :ef) (ef simple-streams +ss-ef-max+
				      +ss-ef-out-dc+)
  `(lambda (character stream)
     (with-stream-class (dual-channel-simple-stream stream)
       (when character
	 (let ((code (char-code character))
	       (ctrl (sm control-out stream)))
	   (unless (and (< code 32) ctrl (svref ctrl code)
			(funcall (the (or symbol function) (svref ctrl code))
				 stream character))
	     (char-to-octets ,ef character (sm co-state stream)
		(lambda (byte)
		  (when (>= (sm outpos stream) (sm max-out-pos stream))
		    (setf (sm outpos stream) (flush-out-buffer stream t)))
		  (setf (bref (sm out-buffer stream) (sm outpos stream)) byte)
		  (incf (sm outpos stream))))
	     (when (sm charpos stream)
	       (incf (sm charpos stream))))))
       character)))

(def-ef-macro (dc write-chars :ef) (ef simple-streams +ss-ef-max+
				       +ss-ef-sout-dc+)
  `(lambda (string stream start end)
     (with-stream-class (dual-channel-simple-stream stream)
       (do ((buffer (sm out-buffer stream))
	    (outpos (sm outpos stream))
	    (max-out-pos (sm max-out-pos stream))
	    (ef (sm external-format stream))
	    (ctrl (sm control-out stream))
	    (posn start (1+ posn))
	    (count 0 (1+ count)))
	   ((>= posn end) (setf (sm outpos stream) outpos) count)
	 (declare (type fixnum outpos max-out-pos posn count))
	 (let* ((char (char string posn))
		(code (char-code char)))
	   (unless (and (< code 32) ctrl (svref ctrl code)
			(funcall (the (or symbol function) (svref ctrl code))
				 stream char))
	     (char-to-octets ,ef char (sm co-state stream)
		(lambda (byte)
		  (when (>= outpos max-out-pos)
		    (setf (sm outpos stream) outpos)
		    (setq outpos (flush-out-buffer stream t)))
		  (setf (bref buffer outpos) byte)
		  (incf outpos)))
	     (setf (sm outpos stream) outpos)
	     (when (sm charpos stream)
	       (incf (sm charpos stream)))))))))


;;;; String-Simple-Stream strategy functions

(declaim (ftype j-read-char-fn (str read-char)))
(defun (str read-char) (stream eof-error-p eof-value blocking)
  (declare (type string-input-simple-stream stream) (ignore blocking)
           #|(optimize (speed 3) (space 2) (safety 0))|#)
  (with-stream-class (string-input-simple-stream stream)
    (when (any-stream-instance-flags stream :eof)
      (return-from read-char
	(lisp::eof-or-lose stream eof-error-p eof-value)))
    (let* ((ptr (sm buffpos stream))
           (char (if (< ptr (sm buffer-ptr stream))
                     (schar (sm buffer stream) ptr)
                     nil)))
      (if (null char)
          (lisp::eof-or-lose stream eof-error-p eof-value)
          (progn
            (setf (sm last-char-read-size stream) 1)
            ;; do string-streams do control-in processing?
            #|(let ((column (sm charpos stream)))
              (declare (type (or null fixnum) column))
              (when column
                (setf (sm charpos stream) (1+ column))))|#
            char)))))


;;;; Functions to install the strategy functions in the appropriate slots

(defun melding-stream (stream)
  (with-stream-class (simple-stream)
    (do ((stm stream (sm melded-stream stm)))
	((eq (sm melded-stream stm) stream) stm))))

(defun meld (stream encap)
  (with-stream-class (simple-stream)
    (setf (sm melding-base encap) (sm melding-base stream))
    (setf (sm melded-stream encap) (sm melded-stream stream))
    (setf (sm melded-stream stream) encap)
    (rotatef (sm j-listen encap) (sm j-listen stream))
    (rotatef (sm j-read-char encap) (sm j-read-char stream))
    (rotatef (sm j-read-chars encap) (sm j-read-chars stream))
    (rotatef (sm j-unread-char encap) (sm j-unread-char stream))
    (rotatef (sm j-write-char encap) (sm j-write-char stream))
    (rotatef (sm j-write-chars encap) (sm j-write-chars stream))))

(defun unmeld (stream)
  (with-stream-class (simple-stream)
    (let ((encap (sm melded-stream stream)))
      (unless (eq encap (sm melding-base stream))
	(setf (sm melding-base encap) encap)
	(setf (sm melded-stream stream) (sm melded-stream encap))
	(setf (sm melded-stream encap) encap)
	(rotatef (sm j-listen stream) (sm j-listen encap))
	(rotatef (sm j-read-char encap) (sm j-read-char stream))
	(rotatef (sm j-read-chars stream) (sm j-read-chars encap))
	(rotatef (sm j-unread-char stream) (sm j-unread-char encap))
	(rotatef (sm j-write-char stream) (sm j-write-char encap))
	(rotatef (sm j-write-chars stream) (sm j-write-chars encap))))))

(defun %sf (kind name format &optional access)
  (or (ignore-errors (fdefinition (list kind name format access)))
      (ignore-errors (fdefinition (list kind name format)))
      (ignore-errors (funcall (fdefinition (list kind name :ef access))
			      format))
      (funcall (fdefinition (list kind name :ef)) format)))

(defun install-single-channel-character-strategy (stream external-format
                                                         access)
  (let ((format (find-external-format external-format)))
    ;; ACCESS is usually NIL
    ;; May be "undocumented" values: stream::buffer, stream::mapped
    ;;   to install strategies suitable for direct buffer streams
    ;;   (i.e., ones that call DEVICE-EXTEND instead of DEVICE-READ)
    ;; (Avoids checking "mode" flags by installing special strategy)
    (with-stream-class (simple-stream stream)
      (setf (sm j-listen stream)
	  (%sf 'sc 'listen (ef-name format) access)
	    (sm j-read-char stream)
	  (%sf 'sc 'read-char (ef-name format) access)
	    (sm j-read-chars stream)
	  (%sf 'sc 'read-chars (ef-name format) access)
	    (sm j-unread-char stream)
	  (%sf 'sc 'unread-char (ef-name format) access)
	    (sm j-write-char stream)
	  (%sf 'sc 'write-char (ef-name format) access)
	    (sm j-write-chars stream)
	  (%sf 'sc 'write-chars (ef-name format) access))))
  stream)

(defun install-dual-channel-character-strategy (stream external-format)
  (let ((format (find-external-format external-format)))
    (with-stream-class (simple-stream stream)
      (setf (sm j-listen stream)
	  (%sf 'sc 'listen (ef-name format))
	    (sm j-read-char stream)
	  (%sf 'sc 'read-char (ef-name format))
	    (sm j-read-chars stream)
	  (%sf 'sc 'read-chars (ef-name format))
	    (sm j-unread-char stream)
	  (%sf 'sc 'unread-char (ef-name format))
	    (sm j-write-char stream)
	  (%sf 'dc 'write-char (ef-name format))
	    (sm j-write-chars stream)
	  (%sf 'dc 'write-chars (ef-name format)))))
  stream)

;; Deprecated -- use install-string-{input,output}-character-strategy instead!
(defun install-string-character-strategy (stream)
  (when (any-stream-instance-flags stream :input)
    (install-string-input-character-strategy stream))
  (when (any-stream-instance-flags stream :output)
    (install-string-output-character-strategy stream))
  stream)

(defun install-string-input-character-strategy (stream)
  #| implement me |#
  (with-stream-class (simple-stream stream)
    (setf (sm j-read-char stream) #'(str read-char)))
  stream)

(defun install-string-output-character-strategy (stream)
  #| implement me |#)

#+(or)
(defun install-composing-format-character-strategy (stream composing-format)
  (let ((format composing-format))
    (with-stream-class (simple-stream stream)
      (case format
	(:e-crlf (setf (sm j-read-char stream) #'(str read-char :e-crlf)
		       (sm j-unread-char stream) #'(str unread-char :e-crlf)))))
    #| implement me |#)
  stream)

(defun compose-encapsulating-streams (stream external-format)
  (when (consp external-format)
    (with-stream-class (simple-stream)
      (let ((encap (if (eq (sm melded-stream stream) stream)
		       nil
		       (sm melded-stream stream))))
	(when (null encap)
	  (setq encap (make-instance 'composing-stream))
	  (meld stream encap))
	(setf (stream-external-format encap) (car (last external-format)))
	(setf (sm external-format stream) external-format)
	(install-composing-format-character-strategy stream
						     (butlast external-format))
	))))


(defmethod (setf stream-external-format) (ef (stream simple-stream))
  (with-stream-class (simple-stream stream)
    (setf (sm external-format stream) (find-external-format ef)))
  ef)

#+(or) ;; this is duplicated in classes.lisp
(defmethod (setf stream-external-format) :after
    (ef (stream single-channel-simple-stream))
  (with-stream-class (single-channel-simple-stream stream)
    (compose-encapsulating-streams stream ef)
    (install-single-channel-character-strategy (melding-stream stream)
					       ef nil)))

#+(or) ;; this is duplicated in classes.lisp
(defmethod (setf stream-external-format) :after
    (ef (stream dual-channel-simple-stream))
  (with-stream-class (dual-channel-simple-stream stream)
    (compose-encapsulating-streams stream ef)
    (install-dual-channel-character-strategy (melding-stream stream) ef)))
