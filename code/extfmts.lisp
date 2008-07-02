;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/extfmts.lisp,v 1.2.4.3.2.4 2008/07/02 14:53:44 rtoy Exp $")
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
  (let* ((tmp1 (gensym)) (tmp2 (gensym))
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
			    (the ,(fourth slot)
			      (identity (svref ,tmp1 ,(second slot))))))))
    `(macrolet ((octets-to-code ((state input unput &rest vars) body)
		  `(lambda (,',tmp1 ,state ,input ,unput)
		     (declare (type simple-vector ,',tmp1)
			      (ignorable ,state ,input ,unput)
			      (optimize (ext:inhibit-warnings 3)))
		     (let (,@',slotb
			   (,input `(the (or (unsigned-byte 8) null) ,,input))
			   ,@(loop for var in vars collect `(,var (gensym))))
		       ,body)))
		(code-to-octets ((code state output &rest vars) body)
		  `(lambda (,',tmp1 ,',tmp2 ,state ,output)
		     (declare (type simple-vector ,',tmp1)
			      (ignorable ,state ,output)
			      (optimize (ext:inhibit-warnings 3)))
		     (let (,@',slotb
			   (,code ',code)
			   ,@(loop for var in vars collect `(,var (gensym))))
		       `(let ((,',code (the (unsigned-byte 31) ,,',tmp2)))
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
  (let ((tmp1 (gensym)) (tmp2 (gensym))
	(min (or min size 1))
	(max (or max size 1)))
    `(macrolet ((input ((state input unput &rest vars) body)
		  `(lambda (,',tmp1 ,state ,input ,unput)
		     (declare (ignore ,',tmp1)
			      (ignorable ,state ,input ,unput)
			      (optimize (ext:inhibit-warnings 3)))
		     (let ((,input `(the (values (or (unsigned-byte 31) null)
						 kernel:index)
					 ,,input))
			   ,@(loop for var in vars collect `(,var (gensym))))
		       ,body)))
		(output ((code state output &rest vars) body)
		  `(lambda (,',tmp1 ,',tmp2 ,state ,output)
		     (declare (ignore ,',tmp1)
			      (ignorable ,state ,output)
			      (optimize (ext:inhibit-warnings 3)))
		     (let ((,code ',code)
			   ,@(loop for var in vars collect `(,var (gensym))))
		       `(let ((,',code (the (unsigned-byte 31) ,,',tmp2)))
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
  (when (ext:search-list-defined-p "ext-formats:")
    (when (zerop (hash-table-count *external-format-aliases*))
      (setf (gethash :latin1 *external-format-aliases*) :iso8859-1)
      (setf (gethash :latin-1 *external-format-aliases*) :iso8859-1)
      (setf (gethash :iso-8859-1 *external-format-aliases*) :iso8859-1)
      (load-external-format-aliases)))

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
		 (unix::*filename-encoding* :iso8859-1))
             (load (format nil "ext-formats:~(~A~)" name)
		   :if-does-not-exist nil))
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
    :octets-to-code (lambda (tmp state input unput)
		      (declare (ignore tmp))
		      (funcall (ef-octets-to-code b) (ef-slots b)
			       state
			       (funcall (ef-octets-to-code a) (ef-slots a)
					state
					input
					unput)
			       unput))
    :code-to-octets (lambda (tmp code state output)
		      (declare (ignore tmp))
		      (funcall (ef-code-to-octets b) (ef-slots b)
			       code
			       state
			       `(lambda (x)
				 ,(funcall (ef-code-to-octets a)
					   (ef-slots a)
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

  (if (consp name)
      (let ((efs (mapcar #'%find-external-format name)))
	(if (member nil efs)
	    (if error-p (error "External format ~S not found." name) nil)
	    (let ((name (reduce #'%composed-ef-name (mapcar #'ef-name efs))))
	      (or (gethash name *external-formats*)
		  (%intern-ef (reduce #'%compose-external-formats efs))))))
      (or (%find-external-format name)
	  (if error-p (error "External format ~S not found." name) nil))))

(defun flush-external-formats ()
  (maphash (lambda (name ef)
	     (declare (ignore name))
	     (fill (ef-cache ef) nil))
	   *external-formats*))

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
  (let ((tmp1 (gensym)) (tmp2 (gensym)))
    `(let ((body (funcall (ef-octets-to-code ,external-format)
			  (ef-slots ,external-format)
			  ',state ',input ',unput)))
       `(multiple-value-bind (,',tmp1 ,',tmp2) ,body
	  (setf ,',count (the kernel:index ,',tmp2))
	  (the (or (unsigned-byte 31) null) ,',tmp1)))))

(defmacro codepoint-to-octets (external-format code state output)
  `(funcall (ef-code-to-octets ,external-format) (ef-slots ,external-format)
	    ',code ',state ',output))



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
  (let ((tmp (gensym)))
    `(defun ,name (,ef)
       (let ((,tmp ,(if (eq id 'lisp::lisp)
			idx
			`(+ (ensure-cache ,ef ',id ,reqd) ,idx))))
	 (or (aref (ef-cache ,ef) ,tmp)
	     (setf (aref (ef-cache ,ef) ,tmp)
		 (let ((*compile-print* nil)) (compile nil ,body))))))))



;;; OCTETS-TO-CHAR, CHAR-TO-OCTETS  -- Public
;;;
;;; Read and write one character through an external-format
;;;
(defmacro octets-to-char (external-format state count input unput)
  `(let ((body (octets-to-codepoint ,external-format
				    ,state ,count ,input ,unput)))
     `(let ((code ,body))
        (declare (type (unsigned-byte 31) code))
        (if (< code char-code-limit) (code-char code)
	    #-(and unicode (not unicode-bootstrap)) #\?
	    #+(and unicode (not unicode-bootstrap)) #\U+FFFD))))

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
       ,(char-to-octets extfmt (schar string (+ start i)) state
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
	  (funcall (ef-string-to-octets (find-external-format external-format))
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
	       ,(octets-to-char extfmt state count
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
      (funcall (ef-octets-to-string (find-external-format external-format))
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
       ,(char-to-octets extfmt (schar string (+ start i)) state
			(lambda (b)
			  (when (= ptr (length result))
			    (setq result (adjust-array result (* 2 ptr))))
			  (setf (aref result (1- (incf ptr)))
			      (code-char b)))))))

(defun string-encode (string external-format &optional (start 0) end)
  (multiple-value-bind (result ptr)
      (lisp::with-array-data ((string string) (start start) (end end))
	(funcall (ef-encode (find-external-format external-format))
		 string start end
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
	       ,(octets-to-char extfmt state count
				;; note the need to return NIL for EOF
				(if (= (1+ ptr) (length string))
				    nil
				    (char-code (char string (incf ptr))))
				(lambda (n) (decf ptr n))))
	finally (return (values result (1+ pos))))))

(defun string-decode (string external-format &optional (start 0) end)
  (multiple-value-bind (result pos)
      (lisp::with-array-data ((string string) (start start) (end end))
	(funcall (ef-decode (find-external-format external-format))
		 string (1- start) (1- end) (make-string (length string))))
    (lisp::shrink-vector result pos)))
