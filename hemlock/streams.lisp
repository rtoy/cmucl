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
;;;    This file contains definitions of various types of streams used
;;; in Hemlock.  They are implementation dependant, but should be
;;; portable to all implementations based on Spice Lisp with little
;;; difficulty.
;;;
;;; Written by Skef Wholey and Rob MacLachlan.
;;;

(in-package "HEMLOCK-INTERNALS")

(export '(make-hemlock-output-stream
	  hemlock-region-stream hemlock-region-stream-p
	  hemlock-output-stream make-hemlock-region-stream
	  hemlock-output-stream-p make-kbdmac-stream
	  modify-kbdmac-stream))

(defstruct (hemlock-output-stream
	    (:include stream
		      (:misc #'hemlock-output-misc))
	    (:print-function %print-hemlock-output-stream)
	    (:constructor internal-make-hemlock-output-stream ()))
  ;;
  ;; The mark we insert at.
  mark)

(defun %print-hemlock-output-stream (s stream d)
  (declare (ignore d s))
  (write-string "#<Hemlock output stream>" stream))

(defun make-hemlock-output-stream (mark &optional (buffered :line))
  "Returns an output stream whose output will be inserted at the Mark.
  Buffered, which indicates to what extent the stream may be buffered
  is one of the following:
   :None  -- The screen is brought up to date after each stream operation.
   :Line  -- The screen is brought up to date when a newline is written.
   :Full  -- The screen is not updated except explicitly via Force-Output."
  (modify-hemlock-output-stream (internal-make-hemlock-output-stream) mark
                                buffered))


(defun modify-hemlock-output-stream (stream mark buffered)
  (unless (and (markp mark)
	       (memq (mark-kind mark) '(:right-inserting :left-inserting)))
    (error "~S is not a permanent mark." mark))
  (setf (hemlock-output-stream-mark stream) mark)
  (case buffered
    (:none
     (setf (lisp::stream-out stream) #'hemlock-output-unbuffered-out
	   (lisp::stream-sout stream) #'hemlock-output-unbuffered-sout))
    (:line
     (setf (lisp::stream-out stream) #'hemlock-output-line-buffered-out
	   (lisp::stream-sout stream) #'hemlock-output-line-buffered-sout))
    (:full
     (setf (lisp::stream-out stream) #'hemlock-output-buffered-out
	   (lisp::stream-sout stream) #'hemlock-output-buffered-sout))
    (t
     (error "~S is a losing value for Buffered." buffered)))
  stream)

(defmacro with-left-inserting-mark ((var form) &body forms)
  (let ((change (gensym)))
    `(let* ((,var ,form)
	    (,change (eq (mark-kind ,var) :right-inserting)))
       (unwind-protect
	   (progn
	     (when ,change
	       (setf (mark-kind ,var) :left-inserting))
	     ,@forms)
	 (when ,change
	   (setf (mark-kind ,var) :right-inserting))))))

(defun hemlock-output-unbuffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)
    (redisplay-windows-from-mark mark)))

(defun hemlock-output-unbuffered-sout (stream string start end)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)
    (redisplay-windows-from-mark mark)))

(defun hemlock-output-buffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)))

(defun hemlock-output-buffered-sout (stream string start end)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)))

(defun hemlock-output-line-buffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)
    (when (char= character #\newline)
      (redisplay-windows-from-mark mark))))

(defun hemlock-output-line-buffered-sout (stream string start end)
  (declare (simple-string string))
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)
    (when (find #\newline string :start start :end end)
      (redisplay-windows-from-mark mark))))

(defun hemlock-output-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos (mark-charpos (hemlock-output-stream-mark stream)))
    (:line-length
     (let* ((buffer (line-buffer (mark-line (hemlock-output-stream-mark stream)))))
       (when buffer
	 (do ((w (buffer-windows buffer) (cdr w))
	      (min most-positive-fixnum (min (window-width (car w)) min)))
	     ((null w)
	      (if (/= min most-positive-fixnum) min))))))
    ((:finish-output :force-output)
     (redisplay-windows-from-mark (hemlock-output-stream-mark stream)))
    (:close (setf (hemlock-output-stream-mark stream) nil))
    (:element-type 'string-char)))

(defstruct (hemlock-region-stream
	    (:include stream
		      (:in #'region-in)
		      (:misc #'region-misc)
		      (:in-buffer (make-string lisp::in-buffer-length)))
	    (:print-function %print-region-stream)
	    (:constructor internal-make-hemlock-region-stream (region mark)))
  ;;
  ;; The region we read from.
  region
  ;;
  ;; The mark pointing to the next character to read.
  mark)

(defun %print-region-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<Hemlock region stream>" stream))

(defun make-hemlock-region-stream (region)
  "Returns an input stream that will return successive characters from the
  given Region when asked for input."
  (internal-make-hemlock-region-stream
   region (copy-mark (region-start region) :right-inserting)))

(defun modify-hemlock-region-stream (stream region)
  (setf (hemlock-region-stream-region stream) region
	(lisp::stream-in-index stream) lisp::in-buffer-length)
  (let* ((mark (hemlock-region-stream-mark stream))
	 (start (region-start region))
	 (start-line (mark-line start)))
    ;; Make sure it's dead.
    (delete-mark mark)
    (setf (mark-line mark) start-line  (mark-charpos mark) (mark-charpos start))
    (push mark (line-marks start-line)))
  stream)

(defun region-readline (stream eof-errorp eof-value)
  (close-line)
  (let ((mark (hemlock-region-stream-mark stream))
	(end (region-end (hemlock-region-stream-region stream))))
    (cond ((mark>= mark end)
	   (if eof-errorp
	       (error "~A hit end of file." stream)
	       (values eof-value nil)))
	  ((eq (mark-line mark) (mark-line end))
	   (let* ((limit (mark-charpos end))
		  (charpos (mark-charpos mark))
		  (dst-end (- limit charpos))
		  (result (make-string dst-end)))
	     (declare (simple-string result))
	     (%sp-byte-blt (line-chars (mark-line mark)) charpos
			   result 0 dst-end)
	     (setf (mark-charpos mark) limit)
	     (values result t)))
	  ((= (mark-charpos mark) 0)
	   (let* ((line (mark-line mark))
		  (next (line-next line)))
	     (always-change-line mark next)
	     (values (line-chars line) nil)))
	  (t
	   (let* ((line (mark-line mark))
		  (chars (line-chars line))
		  (next (line-next line))
		  (charpos (mark-charpos mark))
		  (dst-end (- (length chars) charpos))
		  (result (make-string dst-end)))
	     (declare (simple-string chars result))
	     (%sp-byte-blt chars charpos result 0 dst-end)
	     (setf (mark-charpos mark) 0)
	     (always-change-line mark next)
	     (values result nil))))))

(defun region-in (stream eof-errorp eof-value)
  (close-line)
  (let* ((mark (hemlock-region-stream-mark stream))
	 (charpos (mark-charpos mark))
	 (line (mark-line mark))
	 (chars (line-chars line))
	 (length (length chars))
	 (last (region-end (hemlock-region-stream-region stream)))
	 (last-line (mark-line last))
	 (buffer (lisp::stream-in-buffer stream)) start len)
    (declare (fixnum length charpos last-charpos start len)
	     (simple-string chars))
    (cond 
     ((eq line last-line)
      (let ((last-charpos (mark-charpos last)))
	(setq len (- last-charpos charpos))
	(cond
	 ((>= charpos last-charpos)
	  (if eof-errorp
	      (error "~A hit end of file." stream) 
	      (return-from region-in eof-value)))
	 ((> len lisp::in-buffer-length)       
	  (%sp-byte-blt chars charpos buffer 0 lisp::in-buffer-length)
	  (setq start 0  len lisp::in-buffer-length))
	 (t
	  (setq start (- lisp::in-buffer-length len))
	  (%sp-byte-blt chars charpos buffer start lisp::in-buffer-length)))))
     ((line> line last-line)
      (if eof-errorp
	  (error "~a hit end of file." stream) 
	  (return-from region-in eof-value)))
     (t
      (setq len (- length charpos))
      (cond
       ((< len lisp::in-buffer-length)
	(let ((end (1- lisp::in-buffer-length)))
	  (setq start (- lisp::in-buffer-length len 1))
	  (%sp-byte-blt chars charpos buffer start end)
	  (setf (schar buffer end) #\newline))
	(incf len))
       (t
	(%sp-byte-blt chars charpos buffer 0 lisp::in-buffer-length)
	(setq start 0  len lisp::in-buffer-length)))))
    (setf (lisp::stream-in-index stream) (1+ start))
    (character-offset mark len)
    (schar buffer start)))

(defun region-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:listen (mark< (hemlock-region-stream-mark stream)
		    (region-end (hemlock-region-stream-region stream))))
    (:read-line (region-readline stream arg1 arg2))
    (:clear-input (move-mark
                   (hemlock-region-stream-mark stream)
                   (region-end (hemlock-region-stream-region stream))))
    (:close
     (delete-mark (hemlock-region-stream-mark stream))
     (setf (hemlock-region-stream-region stream) nil))
    (:element-type 'string-char)))

;;;; Stuff to support keyboard macros.

(defstruct (kbdmac-stream
	    (:include editor-input
		      (:get #'kbdmac-get)
		      (:unget #'kbdmac-unget)
		      (:listen #'kbdmac-listen))
	    (:constructor make-kbdmac-stream ()))
  buffer    ; The simple-vector that holds the characters.
  index)    ; Index of the next character.

(defun kbdmac-get (stream ignore-abort-attempts-p)
  (declare (ignore ignore-abort-attempts-p))
  (let ((index (kbdmac-stream-index stream)))
    (setf (kbdmac-stream-index stream) (1+ index))
    (setq *last-key-event-typed*
	  (svref (kbdmac-stream-buffer stream) index))))

(defun kbdmac-unget (ignore stream)
  (declare (ignore ignore))
  (if (plusp (kbdmac-stream-index stream))
      (decf (kbdmac-stream-index stream))
      (error "Nothing to unread.")))

(defun kbdmac-listen (stream)
  (declare (ignore stream))
  t)

;;; MODIFY-KBDMAC-STREAM  --  Internal
;;;
;;;    Bash the kbdmac-stream Stream so that it will return the Input.
;;;
(defun modify-kbdmac-stream (stream input)
  (setf (kbdmac-stream-index stream) 0)
  (setf (kbdmac-stream-buffer stream) input)
  stream)
