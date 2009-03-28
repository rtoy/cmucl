;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/extfmts.lisp,v 1.2.4.3.2.9 2009/03/28 13:31:42 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Implementation of external-formats

(in-package "STREAM")

(export '(string-to-octets octets-to-string *default-external-format*
	  string-encode string-decode))

(defvar *default-external-format* :iso8859-1)

(defvar *external-formats* (make-hash-table :test 'equal))
(defvar *external-format-aliases* (make-hash-table))

(defconstant +ef-cin+ 2)
(defconstant +ef-cout+ 3)
(defconstant +ef-sin+ 4)
(defconstant +ef-sout+ 5)
(defconstant +ef-os+ 6)
(defconstant +ef-so+ 7)
(defconstant +ef-en+ 8)
(defconstant +ef-de+ 9)
(defconstant +ef-max+ 10)

(define-condition external-format-not-implemented (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "Attempting unimplemented external-format I/O."))))

(defun %efni (a b c d)
  (declare (ignore a b c d))
  (error 'external-format-not-implemented))

(defstruct efx
  (octets-to-code #'%efni :type function :read-only t)
  (code-to-octets #'%efni :type function :read-only t)
  (cache nil :type (or null simple-vector))
  (min 1 :type kernel:index :read-only t)
  (max 1 :type kernel:index :read-only t))

(defstruct (external-format
             (:conc-name ef-)
             (:print-function %print-external-format)
             (:constructor make-external-format (name efx composingp
						 &optional slots slotd)))
  (name (ext:required-argument) :type (or keyword cons) :read-only t)
  (efx (ext:required-argument) :type efx :read-only t)
  (composingp (ext:required-argument) :type boolean :read-only t)
  (slots #() :type simple-vector :read-only t)
  (slotd nil :type list :read-only t))

(defun %print-external-format (ef stream depth)
  (declare (ignore depth))
  (print-unreadable-object (ef stream :type t :identity t)
    (princ (ef-name ef) stream)))

(defun %intern-ef (ef)
  (setf (gethash (ef-name ef) *external-formats*) ef))

(declaim (inline ef-octets-to-code ef-code-to-octets ef-cache
		 ef-min-octets ef-max-octets))

(defun ef-octets-to-code (ef)
  (efx-octets-to-code (ef-efx ef)))

(defun ef-code-to-octets (ef)
  (efx-code-to-octets (ef-efx ef)))

(defun ef-cache (ef)
  (efx-cache (ef-efx ef)))

(defun ef-min-octets (ef)
  (efx-min (ef-efx ef)))

(defun ef-max-octets (ef)
  (efx-max (ef-efx ef)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %merge-slots (old new)
    (let* ((pos (length old))
	   (tmp (mapcar (lambda (x)
			  (let* ((name (if (consp x) (first x) x))
				 (init (if (consp x) (second x) nil))
				 (list (if (consp x) (nthcdr 2 x) nil))
				 (prev (assoc name old))
				 (posn (if prev (second prev) (1- (incf pos)))))
			    (list name posn init (getf list :type t))))
			new)))
      (delete-duplicates (stable-sort (append old tmp) #'< :key #'second)
			 :key #'second))))

;;; DEFINE-EXTERNAL-FORMAT  -- Public
;;;
;;; name (&key min max size) (&rest slots) octets-to-code code-to-octets
;;;   Define a new external format.  Min/Max/Size are the minimum and
;;;   maximum number of octets that make up a character (:size N is just
;;;   shorthand for :min N :max N).  Slots is a list of slot descriptions
;;;   similar to defstruct.
;;;
;;; name (base) (&rest slots)
;;;   Define an external format based on a previously-defined external
;;;   format, Base.  The slot names used in Slots must match those in Base.
;;;
;;; Note: external-formats work on code-points, not characters, so that
;;;   the entire 31 bit ISO-10646 range can be used internally regardless of
;;;   the size of a character recognized by Lisp and external formats
;;;   can be useful to people who want to process characters outside the
;;;   Lisp range (see CODEPOINT-TO-OCTETS, OCTETS-TO-CODEPOINT)
;;;
(defmacro define-external-format (name (&rest args) (&rest slots)
				       &optional octets-to-code code-to-octets)
  (when (and (oddp (length args)) (not (= (length args) 1)))
    (warn "Nonsensical argument (~S) to DEFINE-EXTERNAL-FORMAT." args))
  (let* ((tmp (gensym))
	 (min (if (evenp (length args))
		  (or (getf args :min) (getf args :size) 1)
		  1))
	 (max (if (evenp (length args))
		  (or (getf args :max) (getf args :size) 6)
		  6))
	 (base (if (= (length args) 1)
		   (find-external-format (first args))
		   nil))
	 (bslotd (if base (ef-slotd base) nil))
	 (slotd (%merge-slots bslotd slots))
	 (slotb (loop for slot in slotd
		  collect `(,(first slot)
			    `(the ,',(fourth slot)
			      ;; IDENTITY is here to protect against SETF
			      (identity (svref %slots% ,',(second slot))))))))
    `(macrolet ((octets-to-code ((state input unput &rest vars) body)
		  `(lambda (,state ,input ,unput)
		     (declare (ignorable ,state ,input ,unput)
			      (optimize (ext:inhibit-warnings 3)))
		     (let (,@',slotb
			   (,input `(the (or (unsigned-byte 8) null) ,,input))
			   ,@(loop for var in vars collect `(,var (gensym))))
		       ,body)))
		(code-to-octets ((code state output &rest vars) body)
		  `(lambda (,',tmp ,state ,output)
		     (declare (ignorable ,state ,output)
			      (optimize (ext:inhibit-warnings 3)))
		     (let (,@',slotb
			   (,code ',code)
			   ,@(loop for var in vars collect `(,var (gensym))))
		       `(let ((,',code (the (unsigned-byte 31) ,,',tmp)))
			  (declare (ignorable ,',code))
			  ,,body)))))
       (%intern-ef (make-external-format ,name
		    ,(if base
			 `(ef-efx (find-external-format ,(ef-name base)))
			 `(make-efx :octets-to-code ,octets-to-code
				    :code-to-octets ,code-to-octets
				    :cache (make-array +ef-max+
							  :initial-element nil)
				    :min ,(min min max) :max ,(max min max)))
		    nil
		    (vector ,@(mapcar #'third slotd))
		    ',slotd)))))

;;; DEFINE-COMPOSING-EXTERNAL-FORMAT  -- Public
;;;
;;; A composing-external-format differs from an (ordinary) external-format
;;; in that it translates characters (really codepoints, of course) into
;;; other characters, rather than translating between characters and binary
;;; octets.  They have to be composed with a non-composing external-format
;;; to be of any use.
;;;
(defmacro define-composing-external-format (name (&key min max size)
						 input output)
  (let ((tmp (gensym))
	(min (or min size 1))
	(max (or max size 1)))
    `(macrolet ((input ((state input unput &rest vars) body)
		  `(lambda (,state ,input ,unput)
		     (declare (ignorable ,state ,input ,unput)
			      (optimize (ext:inhibit-warnings 3)))
		     (let ((,input `(the (values (or (unsigned-byte 31) null)
						 kernel:index)
					 ,,input))
			   ,@(loop for var in vars collect `(,var (gensym))))
		       ,body)))
		(output ((code state output &rest vars) body)
		  `(lambda (,',tmp ,state ,output)
		     (declare (ignorable ,state ,output)
			      (optimize (ext:inhibit-warnings 3)))
		     (let ((,code ',code)
			   ,@(loop for var in vars collect `(,var (gensym))))
		       `(let ((,',code (the (unsigned-byte 31) ,,',tmp)))
			  (declare (ignorable ,',code))
			  ,,body)))))
       (%intern-ef (make-external-format ,name
		    (make-efx :octets-to-code ,input
			      :code-to-octets ,output
			      :min ,(min min max) :max ,(max min max))
		    t
		    #() '())))))

(defun load-external-format-aliases ()
  (let ((*package* (find-package "KEYWORD"))
	(unix::*filename-encoding* :iso8859-1))
    (with-open-file (stm "ext-formats:aliases" :if-does-not-exist nil)
      (when stm
	(do ((alias (read stm nil stm) (read stm nil stm))
	     (value (read stm nil stm) (read stm nil stm)))
	    ((or (eq alias stm) (eq value stm))
	     (unless (eq alias stm)
	       (warn "External-format aliases file ends early.")))
	  (if (and (keywordp alias) (keywordp value))
	      (setf (gethash alias *external-format-aliases*) value)
	      (warn "Bad entry in external-format aliases file: ~S => ~S."
		    alias value)))))))

(defun %find-external-format (name)
  ;; avoid loading files, etc., early in the boot sequence
  (when (or (eq name :iso8859-1)
	    (and (eq name :default) (eq *default-external-format* :iso8859-1)))
    (return-from %find-external-format
      (gethash :iso8859-1 *external-formats*)))

  (when (zerop (hash-table-count *external-format-aliases*))
    (setf (gethash :latin1 *external-format-aliases*) :iso8859-1)
    (setf (gethash :latin-1 *external-format-aliases*) :iso8859-1)
    (setf (gethash :iso-8859-1 *external-format-aliases*) :iso8859-1)
    (load-external-format-aliases))

  (do ((tmp (gethash name *external-format-aliases*)
            (gethash tmp *external-format-aliases*))
       (cnt 0 (1+ cnt)))
      ((or (null tmp) (= cnt 50))
       (unless (null tmp)
         (error "External-format aliasing depth exceeded.")))
    (setq name tmp))

  (or (gethash name *external-formats*)
      (and (let ((*package* (find-package "STREAM"))
		 (lisp::*enable-package-locked-errors* nil)
		 (*default-external-format* :iso8859-1)
		 (unix::*filename-encoding* :iso8859-1)
		 (s (open (format nil "ext-formats:~(~A~).lisp" name) :if-does-not-exist nil)))
	     (when s
	       (null (nth-value 1 (ext:compile-from-stream s)))))
           (gethash name *external-formats*))))

(defun %composed-ef-name (a b)
  (if (consp a) (append a (list b)) (list a b)))

(defun %compose-external-formats (a b)
  (when (ef-composingp a)
    (error "~S is a Composing-External-Format." (ef-name a)))
  (unless (ef-composingp b)
    (error "~S is not a Composing-External-Format." (ef-name b)))
  (make-external-format
   (%composed-ef-name (ef-name a) (ef-name b))
   (make-efx
    :octets-to-code (lambda (state input unput)
		      (funcall (ef-octets-to-code b) state
			       (funcall (ef-octets-to-code a)
					state input unput)
			       unput))
    :code-to-octets (lambda (code state output)
		      (funcall (ef-code-to-octets b) code state
			       `(lambda (x)
				 ,(funcall (ef-code-to-octets a)
					   'x state output))))
    :cache (make-array +ef-max+ :initial-element nil)
    :min (* (ef-min-octets a) (ef-min-octets b))
    :max (* (ef-max-octets a) (ef-max-octets b)))
   nil #() '()))

(defun find-external-format (name &optional (error-p t))
  (when (external-format-p name)
    (return-from find-external-format name))

  (or (if (consp name) (every #'keywordp name) (keywordp name))
      (error "~S is not a valid external format name." name))

  (when (eq name :default)
    (setq name *default-external-format*))

  (when (and (consp name) (not (cdr name)))
    (setq name (car name)))

  (flet ((not-found ()
	   (when (equal *default-external-format* name)
	     (setq *default-external-format* :iso8859-1))
	   (if error-p (error "External format ~S not found." name) nil)))
    (if (consp name)
	(let ((efs (mapcar #'%find-external-format name)))
	  (if (member nil efs)
	      (not-found)
	      (let ((name (reduce #'%composed-ef-name (mapcar #'ef-name efs))))
		(or (gethash name *external-formats*)
		    (%intern-ef (reduce #'%compose-external-formats efs))))))
	(or (%find-external-format name) (not-found)))))

(defun flush-external-formats ()
  (maphash (lambda (name ef)
	     (declare (ignore name))
	     (fill (ef-cache ef) nil))
	   *external-formats*))

(defvar *.table-inverse.* (make-hash-table :test 'eq :size 7))

(defun invert-table (table)
  (declare (type (or (simple-array (unsigned-byte 31) *)
		     (simple-array (unsigned-byte 16) *))
		 table)
	   (optimize (speed 3) (space 0) (safety 0) (debug 0)
		     (ext:inhibit-warnings 3)))
  (or (gethash table *.table-inverse.*)
      (let* ((result (make-hash-table))
	     (width (array-dimension table 0))
	     (power (1- (array-rank table)))
	     (base (if (= width 94) 1 0)))
	(assert (and (< power 3) (<= width 256)))
	(dotimes (i (array-total-size table))
	  (declare (type (integer 0 (#.array-dimension-limit)) i))
	  (let ((tmp i) (val (row-major-aref table i)) (z 0))
	    (declare (type (integer 0 (#.array-dimension-limit)) tmp)
		     (type (unsigned-byte 32) z))
	    (unless (or (= val #xFFFE) (gethash val result))
	      (dotimes (j power)
		;; j is only ever 0 in reality, since no n^3 tables are
		;; defined; z was declared as 32-bit above, so that limits
		;; us to 0 <= j <= 2   (see the ASSERT)
		(declare (type (integer 0 2) j))
		(multiple-value-bind (x y) (floor tmp width)
		  (setq tmp x)
		  (setq z (logior z (ash (the (integer 0 255) (+ y base))
					 (the (integer 0 24)
					   (* 8 (- power j))))))))
	      (setf (gethash val result) (logior z (+ tmp base))))))
	(setf (gethash table *.table-inverse.*) result))))


(define-condition void-external-format (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "Attempting I/O through void external-format."))))

(define-external-format :void (:size 0) ()
  (octets-to-code (state input unput)
    `(error 'void-external-format))
  (code-to-octets (code state output)
    `(error 'void-external-format)))

(define-external-format :iso8859-1 (:size 1) ()
  (octets-to-code (state input unput)
    `(values ,input 1))
  (code-to-octets (code state output)
    `(,output (if (> ,code 255) #x3F ,code))))

;;; OCTETS-TO-CODEPOINT, CODEPOINT-TO-OCTETS  -- Semi-Public
;;;
;;; Normally you'd want to use OCTETS-TO-CHAR and CHAR-TO-OCTETS instead of
;;; these, but that limits you to Lisp's idea of a character - either Latin-1
;;; in 8 bit Lisp images, or the Unicode BMP in 16 bit images.  If you want
;;; to read or write texts containing characters not supported by your Lisp,
;;; these macros can be used instead.
(defmacro octets-to-codepoint (external-format state count input unput)
  (let ((tmp1 (gensym)) (tmp2 (gensym))
	(ef (find-external-format external-format)))
    `(multiple-value-bind (,tmp1 ,tmp2)
	 ,(funcall (ef-octets-to-code ef) state input unput)
       (setf ,count (the kernel:index ,tmp2))
       (the (or (unsigned-byte 31) null) ,tmp1))))

(defmacro codepoint-to-octets (external-format code state output)
  (let ((ef (find-external-format external-format)))
    (funcall (ef-code-to-octets ef) code state output)))



(defvar *ef-base* +ef-max+)
(defvar *ef-extensions* '())

(defun ensure-cache (ef id reqd)
  (let ((base (or (getf *ef-extensions* id)
		  (setf (getf *ef-extensions* id)
		      (prog1 *ef-base* (incf *ef-base* reqd))))))
    (when (< (length (ef-cache ef)) (+ base reqd))
      (setf (efx-cache (ef-efx ef))
	  (adjust-array (ef-cache ef) (+ base reqd) :initial-element nil)))
    base))

;;; DEF-EF-MACRO  -- Public
;;;
;;; 
(defmacro def-ef-macro (name (ef id reqd idx) body)
  (let ((tmp1 (gensym))
	(tmp2 (gensym))
	(%name (intern (format nil "%~A" name) (symbol-package name))))
    `(progn
       (defun ,%name (,ef)
	 (let* ((,tmp1 (find-external-format ,ef))
		(,tmp2 ,(if (eq id 'lisp::lisp)
			    idx
			    `(+ (ensure-cache ,tmp1 ',id ,reqd) ,idx))))
	   (funcall (or (aref (ef-cache ,tmp1) ,tmp2)
			(setf (aref (ef-cache ,tmp1) ,tmp2)
			    (let ((*compile-print* nil)
				  ;; Set default format when we compile so we
				  ;; can see compiler messages.  if we don't,
				  ;; we run into a problem that we might be
				  ;; changing the default format while we're
				  ;; compiling, and we don't know how to output
				  ;; the compiler messages.
				  (*default-external-format* :iso8859-1))
			      (compile nil `(lambda (%slots%)
					      (declare (ignorable %slots%))
					      ,,body)))))
		    (ef-slots ,tmp1))))
       (declaim (inline ,name))
       (defun ,name (,tmp1)
	 (let ((,tmp2 (load-time-value (cons nil nil))))
	   (when (eq ,tmp1 :default)
	     (setq ,tmp1 *default-external-format*))
	   (if (eq ,tmp1 (car ,tmp2))
	       (cdr ,tmp2)
	       (setf (car ,tmp2) ,tmp1
		     (cdr ,tmp2) (,%name ,tmp1))))))))



;;; OCTETS-TO-CHAR, CHAR-TO-OCTETS  -- Public
;;;
;;; Read and write one character through an external-format
;;;
(defmacro octets-to-char (external-format state count input unput)
  `(let ((code (octets-to-codepoint ,external-format
				    ,state ,count ,input ,unput)))
     (declare (type (unsigned-byte 31) code))
     (if (< code char-code-limit) (code-char code)
	 #-(and unicode (not unicode-bootstrap)) #\?
	 #+(and unicode (not unicode-bootstrap)) #\U+FFFD)))

(defmacro char-to-octets (external-format char state output)
  `(codepoint-to-octets ,external-format (char-code ,char) ,state ,output))


(def-ef-macro ef-string-to-octets (extfmt lisp::lisp +ef-max+ +ef-so+)
  `(lambda (string start end buffer &aux (ptr 0) (state nil))
     (declare #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#
	      (type simple-string string)
	      (type kernel:index start end ptr)
	      (type (simple-array (unsigned-byte 8) (*)) buffer)
	      (ignorable state))
     (dotimes (i (- end start) (values buffer ptr))
       (declare (type kernel:index i))
       (char-to-octets ,extfmt (schar string (+ start i)) state
		       (lambda (b)
			 (when (= ptr (length buffer))
			   (setq buffer (adjust-array buffer (* 2 ptr))))
			 (setf (aref buffer (1- (incf ptr))) b))))))

(defun string-to-octets (string &key (start 0) end (external-format :default)
				     (buffer nil bufferp))
  (declare (type string string)
	   (type kernel:index start)
	   (type (or kernel:index null) end)
	   (type (or (simple-array (unsigned-byte 8) (*)) null) buffer))
  (let* ((buffer (or buffer (make-array (length string)
					:element-type '(unsigned-byte 8)))))
    (multiple-value-bind (buffer ptr)
	(lisp::with-array-data ((string string) (start start) (end end))
	  (funcall (ef-string-to-octets external-format)
		   string start end buffer))
      (values (if bufferp buffer (lisp::shrink-vector buffer ptr)) ptr))))

(def-ef-macro ef-octets-to-string (extfmt lisp::lisp +ef-max+ +ef-os+)
  `(lambda (octets ptr end string &aux (pos -1) (count 0) (state nil))
     (declare #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#
	      (type (simple-array (unsigned-byte 8) (*)) octets)
	      (type kernel:index end count)
	      (type (integer -1 (#.array-dimension-limit)) ptr pos)
	      (type simple-string string)
	      (ignorable state))
     (loop until (>= ptr end)
	do (when (= pos (length string))
	     (setq string (adjust-array string (* 2 pos))))
	   (setf (schar string (incf pos))
	       (octets-to-char ,extfmt state count
			       (aref octets (incf ptr)) ;;@@ EOF??
			       (lambda (n) (decf ptr n))))
	finally (return (values string (1+ pos))))))

(defun octets-to-string (octets &key (start 0) end (external-format :default)
				     (string nil stringp))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
	   (type kernel:index start)
	   (type (or kernel:index null) end)
	   (type (or simple-string null) string))
  (multiple-value-bind (string pos)
      (funcall (ef-octets-to-string external-format)
	       octets (1- start) (1- (or end (length octets)))
	       (or string (make-string (length octets))))
    (values (if stringp string (lisp::shrink-vector string pos)) pos)))



(def-ef-macro ef-encode (extfmt lisp::lisp +ef-max+ +ef-en+)
  `(lambda (string start end result &aux (ptr 0) (state nil))
     (declare #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#
	      (type simple-string string)
	      (type kernel:index start end ptr)
	      (type simple-base-string result)
	      (ignorable state))
     (dotimes (i (- end start) (values result ptr))
       (declare (type kernel:index i))
       (char-to-octets ,extfmt (schar string (+ start i)) state
		       (lambda (b)
			 (when (= ptr (length result))
			   (setq result (adjust-array result (* 2 ptr))))
			 (setf (aref result (1- (incf ptr)))
			     (code-char b)))))))

(defun string-encode (string external-format &optional (start 0) end)
  (when (zerop (length string))
    (return-from string-encode string))
  (multiple-value-bind (result ptr)
      (lisp::with-array-data ((string string) (start start) (end end))
	(funcall (ef-encode external-format) string start end
		 (make-string (length string) :element-type 'base-char)))
    (lisp::shrink-vector result ptr)))

(def-ef-macro ef-decode (extfmt lisp::lisp +ef-max+ +ef-de+)
  `(lambda (string ptr end result &aux (pos -1) (count 0) (state nil))
     (declare #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#
	      (type simple-string string)
	      (type kernel:index end count)
	      (type (integer -1 (#.array-dimension-limit)) ptr pos)
	      (type simple-string result)
	      (ignorable state))
     (loop until (>= ptr end)
	;; increasing size of result shouldn't ever be necessary, unless
	;; someone implements an encoding smaller than the source string...
	do (setf (schar result (incf pos))
	       (octets-to-char ,extfmt state count
			       ;; note the need to return NIL for EOF
			       (if (= (1+ ptr) (length string))
				   nil
				   (char-code (char string (incf ptr))))
			       (lambda (n) (decf ptr n))))
	finally (return (values result (1+ pos))))))

(defun string-decode (string external-format &optional (start 0) end)
  (when (zerop (length string))
    (return-from string-decode string))
  (multiple-value-bind (result pos)
      (lisp::with-array-data ((string string) (start start) (end end))
	(funcall (ef-decode external-format)
		 string (1- start) (1- end) (make-string (length string))))
    (lisp::shrink-vector result pos)))
