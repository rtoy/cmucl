;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: src/code/extfmts.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Implementation of external-formats

(in-package "STREAM")

(intl:textdomain "cmucl")

(export '(string-to-octets octets-to-string *default-external-format*
	  string-encode string-decode set-system-external-format
	  +replacement-character-code+
	  list-all-external-formats
	  describe-external-format))

(defvar *default-external-format*
  :utf-8
  "The default external format to use if no other external format is
  specified.  This is unaffected by any locale settings or by
  SET-SYSTEM-EXTERNAL-FORMAT.")

(defvar *external-formats*
  (make-hash-table :test 'equal)
  "Hash table of all the external formats that have been loaded")

(defvar *external-format-aliases*
  (make-hash-table)
  "Hash table mapping an external format alias to the actual external
  format implementation")

;; Each time DEF-EF-MACRO is used to define a new external format
;; macro, a unique value must be used for the index.  The mapping
;; between the macro and the index is here.
(vm::defenum (:prefix "+EF-" :suffix "+" :start 1)
  str					; string length
  cin					; input a character
  cout					; output a character
  sin					; input string
  sout					; output string
  os					; octets to string
  so					; string to octets
  en					; encode
  de					; decode
  flush					; flush state
  copy-state				; copy state
  osc					; octets to string, counted
  max)

;; Unicode replacement character U+FFFD
(defconstant +replacement-character-code+ #xFFFD)

(define-condition external-format-not-implemented (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream (intl:gettext "Attempting unimplemented external-format I/O.")))))

(define-condition external-format-not-found (error)
  ((name :reader external-format-not-found-name
	:initarg :name))
  (:report
    (lambda (condition stream)
      (format stream (intl:gettext "External format ~S not found.")
	      (external-format-not-found-name condition)))))

(defun %efni (a b c d)
  (declare (ignore a b c d))
  (error 'external-format-not-implemented))

(defstruct efx
  ;;
  ;; Function to read a sequence of octets from a stream and convert
  ;; them a code point.
  (octets-to-code #'%efni :type function :read-only t)
  ;;
  ;; Function to convert a codepoint to a sequence of octets and write
  ;; them to an output stream.
  (code-to-octets #'%efni :type function :read-only t)
  ;;
  ;; Function (or NIL) to force any state in the external format to be
  ;; flushed to the output stream.  A NIL value means the external
  ;; format does not need to do anything special.
  (flush-state nil :type (or null function) :read-only t)
  ;;
  ;; Function to copy the state of the external-format.  If NIL, then
  ;; there is no state to be copied.
  (copy-state nil :type (or null function) :read-only t)
  (cache nil :type (or null simple-vector))
  ;;
  ;; Minimum number of octets needed to form a codepoint
  (min 1 :type kernel:index :read-only t)
  ;;
  ;; Maximum number of octets needed to form a codepoint.
  (max 1 :type kernel:index :read-only t)
  ;;
  ;; Documentation for this external format
  #+nil(documentation nil :type (or null string) :read-only t))

(defstruct (external-format
             (:conc-name ef-)
             (:print-function %print-external-format)
             (:constructor make-external-format (name efx composingp documentation
						 &optional slots slotd)))
  (name (ext:required-argument) :type (or keyword cons) :read-only t)
  (efx (ext:required-argument) :type efx :read-only t)
  (composingp (ext:required-argument) :type boolean :read-only t)
  (slots #() :type simple-vector :read-only t)
  (slotd nil :type list :read-only t)
  (documentation nil :type (or null string) :read-only t))

(defun %print-external-format (ef stream depth)
  (declare (ignore depth))
  (print-unreadable-object (ef stream :type t :identity t)
    (princ (ef-name ef) stream)))

(defun %intern-ef (ef)
  (setf (gethash (ef-name ef) *external-formats*) ef))

(declaim (inline ef-octets-to-code ef-code-to-octets ef-flush-state ef-copy-state
		 ef-cache ef-min-octets ef-max-octets))

(defun ef-octets-to-code (ef)
  (efx-octets-to-code (ef-efx ef)))

(defun ef-code-to-octets (ef)
  (efx-code-to-octets (ef-efx ef)))

(defun ef-flush-state (ef)
  (efx-flush-state (ef-efx ef)))

(defun ef-copy-state (ef)
  (efx-copy-state (ef-efx ef)))

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
;;; name (&key base min max size documentation) (&rest slots) octets-to-code
;;;       code-to-octets flush-state copy-state
;;;
;;;   Define a new external format.  If base is specified, then an
;;;   external format is defined that is based on a previously defined
;;;   external format named Base.  The slot names used in Slots must
;;;   match those defined in the Base format.
;;;
;;;   If Base is not specified, a new external format is defined.
;;;   Min/Max/Size are the minimum and maximum number of octets that
;;;   make up a character (:size N is just shorthand for :min N :max
;;;   N).  Slots is a list of slot descriptions similar to defstruct.
;;;
;;;   In both cases, Documentation is a string that documents the
;;;   external format.
;;;
;;; octets-to-code (state input unput error &rest vars)
;;;   Defines a form to be used by the external format to convert
;;;   octets to a code point.  State is a form that can be used by the
;;;   body to access the state of the stream.  Input is a form that
;;;   can be used to read one octet from the input stream.  (It can be
;;;   called as many times as needed.)  Similarly, Unput is a form to
;;;   put back one octet to the input stream.  Error is an error
;;;   handler.  The default is NIL to indicate that the code should do
;;;   its default handling.  Otherwise, it should be a function or
;;;   symbol to indicate how errors are handled.  Vars is a list of
;;;   vars that need to be defined for any symbols used within the
;;;   form.
;;;
;;;   The error handler is a function of 3 arguments: a format message
;;;   string, the offending octet (or NIL) and the number of octets
;;;   read for this encoding.  If the function returns, it should
;;;   return the codepoint to be used in place of the erroneous
;;;   sequence.
;;;
;;;   This should return two values: the code and the number of octets
;;;   read to form the code.
;;;
;;; code-to-octets (code state output error &rest vars)
;;;   Defines a form to be used by the external format to convert a
;;;   code point to octets for output.  Code is the code point to be
;;;   converted.  State is a form to access the current value of the
;;;   stream's state variable.  Output is a form that writes one octet
;;;   to the output stream.  Error is the error handler.  A value of
;;;   NIL means the external format should use its default method.
;;;   Otherwise, it should be a symbol or function that will e called
;;;   to handle the error.
;;;
;;;   The error function takes 2 arguments: a format message string
;;;   and the offending codepoint.  If the function returns, it should
;;;   be the desired replacement codepoint.
;;;
;;; flush-state (state output error &rest vars)
;;;   Defines a form to be used by the external format to flush out
;;;   any state when an output stream is closed.  Similar to
;;;   CODE-TO-OCTETS, but there is no code.  Error is similar to the
;;;   error parameter for code-to-octets.
;;;
;;; copy-state (state &rest vars)
;;;   Defines a form to copy any state needed by the external format.
;;;   This should probably be a deep copy so that if the original
;;;   state is modified, the copy is not.
;;;
;;; Note: external-formats work on code-points, not
;;;   characters, so that the entire 31 bit ISO-10646 range can be
;;;   used internally regardless of the size of a character recognized
;;;   by Lisp and external formats can be useful to people who want to
;;;   process characters outside the Lisp range (see
;;;   CODEPOINT-TO-OCTETS, OCTETS-TO-CODEPOINT)
;;;
(defmacro define-external-format (name (&key base min max size (documentation ""))
				       (&rest slots)
				       &optional octets-to-code code-to-octets
				       flush-state copy-state)
  (let* ((tmp (gensym))
	 (min (or min size 1))
	 (max (or max size 6))
	 (base (when base
		 (find-external-format base)))
	 (bslotd (if base (ef-slotd base) nil))
	 (slotd (%merge-slots bslotd slots))
	 (slotb (loop for slot in slotd
		  collect `(,(first slot)
			    `(the ,',(fourth slot)
			      ;; IDENTITY is here to protect against SETF
			       (identity (svref %slots% ,',(second slot))))))))
    (when documentation
      (intl::note-translatable intl::*default-domain* documentation))
    `(macrolet ((octets-to-code ((state input unput error &rest vars) body)
		  `(lambda (,state ,input ,unput ,error)
		     (declare (ignorable ,state ,input ,unput ,error)
			      (optimize (ext:inhibit-warnings 3)))
		     (let (,@',slotb
			   (,input `(the (or (unsigned-byte 8) null) ,,input))
			   ,@(loop for var in vars collect `(,var (gensym))))
		       ,body)))
		(code-to-octets ((code state output error &rest vars) body)
		  `(lambda (,',tmp ,state ,output ,error)
		     (declare (ignorable ,state ,output ,error)
			      (optimize (ext:inhibit-warnings 3)))
		     (let (,@',slotb
			   (,code ',code)
			   ,@(loop for var in vars collect `(,var (gensym))))
		       `(let ((,',code (the lisp:codepoint ,,',tmp)))
			  (declare (ignorable ,',code))
			  ,,body))))
		(flush-state ((state output error &rest vars) body)
		  `(lambda (,state ,output ,error)
		     (declare (ignorable ,state ,output ,error))
		     (let (,@',slotb
			   ,@(loop for var in vars collect `(,var (gensym))))
		       ,body)))
		(copy-state ((state &rest vars) body)
		  `(lambda (,state)
		     (declare (ignorable ,state))
		     (let (,@',slotb
			   ,@(loop for var in vars collect `(,var (gensym))))
		       ,body))))
       (%intern-ef (make-external-format ,name
		    ,(if base
			 `(ef-efx (find-external-format ,(ef-name base)))
			 `(make-efx :octets-to-code ,octets-to-code
				    :code-to-octets ,code-to-octets
				    :flush-state ,flush-state
			            :copy-state ,copy-state
				    :cache (make-array +ef-max+
							  :initial-element nil)
				    :min ,(min min max)
				    :max ,(max min max)))
		    nil
		    ,documentation
		    (let* ,(loop for x in slotd
				 collect (list (first x) (third x)))
		      (vector ,@(mapcar #'first slotd)))
		    ',slotd)))))

;;; DEFINE-COMPOSING-EXTERNAL-FORMAT  -- Public
;;;
;;; A composing-external-format differs from an (ordinary) external-format
;;; in that it translates characters (really codepoints, of course) into
;;; other characters, rather than translating between characters and binary
;;; octets.  They have to be composed with a non-composing external-format
;;; to be of any use.
;;;
;;;
;;; name (&key min max size documentation) input output
;;;   Defines a new composing external format.  The parameters Min,
;;;   Max, Size, and Documentation are the same as for defining an
;;;   external format.  The parameters input and output are forms to
;;;   handle input and output.
;;;
;;; input (state input unput &rest vars)
;;;   Defines a form to be used by the composing external format when
;;;   reading input to transform a codepoint (or sequence of
;;;   codepoints) to another.  State is a form that can be used by the
;;;   body to access the state of the external format.  Input is a
;;;   form that can be used to read one code point from the input
;;;   stream.  (Input returns two values, the codepoint and the number
;;;   of octets read.)  It may be called as many times as needed.
;;;   This returns two values: the codepoint of the character (or NIL)
;;;   and the number of octets read.  Similarly, Unput is a form to
;;;   put back one octet to the input stream.  Vars is a list of vars
;;;   that need to be defined for any symbols used within the form.
;;;
;;;   This should return two values: the code and the number of octets
;;;   read to form the code.
;;;
;;; output (code state output &rest vars)
;;;   Defines a form to be used by the composing external format to
;;;   convert a code point to octets for output.  Code is the code
;;;   point to be converted.  State is a form to access the current
;;;   value of the stream's state variable.  Output is a form that
;;;   writes one octet to the output stream.

(defmacro define-composing-external-format (name (&key min max size documentation)
						 input output)
  (let ((tmp (gensym))
	(min (or min size 1))
	(max (or max size 1)))
    `(macrolet ((input ((state input unput &rest vars) body)
		  `(lambda (,state ,input ,unput)
		     (declare (ignorable ,state ,input ,unput)
			      (optimize (ext:inhibit-warnings 3)))
		     (let ((,input `(the (values (or lisp:codepoint null)
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
		       `(let ((,',code (the lisp:codepoint ,,',tmp)))
			  (declare (ignorable ,',code))
			  ,,body)))))
       (%intern-ef (make-external-format ,name
		    (make-efx :octets-to-code ,input
			      :code-to-octets ,output
			      :min ,(min min max) :max ,(max min max))
		    t
		    ,documentation
		    #() '())))))

(defun load-external-format-aliases ()
  ;; Set filename encoding to NIL to bypass any encoding; it's not
  ;; needed to open the aliases file.  NIL means the pathname string is passed as is where only the low 8 bits of the 
  (let ((*package* (find-package "KEYWORD"))
	(unix::*filename-encoding* :null))
    (with-open-file (stm "ext-formats:aliases" :if-does-not-exist nil
			 :external-format :iso8859-1)
      (when stm
	(do ((alias (read stm nil stm) (read stm nil stm))
	     (value (read stm nil stm) (read stm nil stm)))
	    ((or (eq alias stm) (eq value stm))
	     (unless (eq alias stm)
	       (warn (intl:gettext "External-format aliases file ends early."))))
	  (if (and (keywordp alias) (or (keywordp value)
					(and (consp value)
					     (every #'keywordp value))))
	      (setf (gethash alias *external-format-aliases*) value)
	      (warn (intl:gettext "Bad entry in external-format aliases file: ~S => ~S.")
		    alias value)))))))

(defun list-all-external-formats ()
  "List the available external formats.  A list is returned where each
  element is list of the external format and a list of aliases for the
  format.  No distinction is made between external formats and
  composing external formats."
  ;; Look for all lisp files in the ext-formats directory.  These are
  ;; the available formats.
  (let ((ef (make-hash-table))
	result)
    (map nil #'(lambda (p)
		 (setf (gethash (intern (string-upcase (pathname-name p)) :keyword) ef)
		       nil))
	 (directory "ext-formats:*.lisp"))

    ;; Look through aliases and update formats with a list of aliases.
    (load-external-format-aliases)
    (maphash #'(lambda (k v)
		 (push k (gethash v ef)))
	     *external-format-aliases*)

    (maphash #'(lambda (k v)
		 (push (if v
			   (list k v)
			   (list k))
		       result))
	     ef)
    (sort result #'string< :key #'first)))

(defun describe-external-format (external-format)
  "Print a description of the given External-Format.  This may cause
  the external format to be loaded (silently), if it is not already
  loaded."
  (when (zerop (hash-table-count
		*external-format-aliases*))
    (load-external-format-aliases))
  (let ((alias (gethash external-format *external-format-aliases*)))
    (cond (alias
	   (format t (intl:gettext "~&~S is an alias for the external format ~S.~2%")
		   external-format alias))
	  ((and (listp external-format)
		(> (length external-format) 1))
	   ;; Some kind of composed external format
	   (format t (intl:gettext "~&~S is a composed external format.~2%") external-format))
	  (t
	   (let ((ef (handler-case (let ((*compile-print* nil)
					 (ext:*compile-progress* nil)
					 (*compile-verbose* nil))
				     ;; Should we be this silent when
				     ;; loading the external format?
				     ;; We aren't when the normally
				     ;; loading the format.
				     (find-external-format external-format))
		       (external-format-not-found ()
			 (format *error-output*
				 (intl:gettext "~&Could not find external format ~S~%")
				 external-format)))))
	     (when ef
	       (let (aliases)
		 ;; Find any aliases for this external format.  Doesn't need to be efficient.
		 (maphash #'(lambda (k v)
			      (when (eq v external-format)
				(push k aliases)))
			  *external-format-aliases*)
		 (format t (intl:gettext "~S~:[~; - [Aliases: ~{~S~^, ~}~]]~%")
			 external-format aliases aliases))
	       (when (ef-composingp ef)
		 (format t (intl:gettext "~&~S is a composing external format.~2%")
			 external-format))
	       (format t "~&~A~%"
		       (intl:gettext (or (ef-documentation ef) "")))))))))

(defun %find-external-format (name)
  ;; avoid loading files, etc., early in the boot sequence
  (when (or (eq name :iso8859-1)
	    (and (eq name :default) (eq *default-external-format* :iso8859-1)))
    (return-from %find-external-format
      (gethash :iso8859-1 *external-formats*)))
  (when (eq name :utf-8)
    (return-from %find-external-format
      (gethash :utf-8 *external-formats*)))
  (when (eq name :ascii)
    (return-from %find-external-format
      (gethash :ascii *external-formats*)))

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
         (error (intl:gettext "External-format aliasing depth exceeded."))))
    (setq name tmp))

  (or (gethash name *external-formats*)
      (and (consp name) (find-external-format name))
      (and (with-standard-io-syntax
	     ;; Use standard IO syntax so that changes by the user
	     ;; don't mess up compiling the external format, but we
	     ;; don't need to print readably.  Also, set filename
	     ;; encoding to NIL because we don't need any special
	     ;; encoding to open the format files.
	     (let* ((*print-readably* nil)
		    (unix::*filename-encoding* :null)
		    (*package* (find-package "STREAM"))
		    (lisp::*enable-package-locked-errors* nil)
		    (s (open (format nil "ext-formats:~(~A~).lisp" name)
			     :if-does-not-exist nil :external-format :iso8859-1)))
	       (when s
		 (null (nth-value 1 (ext:compile-from-stream s))))))
           (gethash name *external-formats*))))

(defun %composed-ef-name (a b)
  (if (consp a) (append a (list b)) (list a b)))

(defun %compose-external-formats (a b)
  (when (ef-composingp a)
    (error (intl:gettext "~S is a Composing-External-Format.") (ef-name a)))
  (unless (ef-composingp b)
    (error (intl:gettext "~S is not a Composing-External-Format.") (ef-name b)))
  (make-external-format
   (%composed-ef-name (ef-name a) (ef-name b))
   (make-efx
    :octets-to-code (lambda (state input unput error)
		      (let ((nstate (gensym "STATE-")))
			`(let ((,nstate ,state))
			   (when (null ,nstate)
			     (setq ,nstate (setf ,state (cons nil nil))))
			   ,(funcall (ef-octets-to-code b) `(car ,nstate)
				     (funcall (ef-octets-to-code a)
					      `(cdr ,nstate) input unput error)
				     unput))))
    :code-to-octets (lambda (code state output error)
		      (let ((nstate (gensym "STATE-")))
			`(let ((,nstate ,state))
			   (when (null ,nstate)
			     (setq ,nstate (setf ,state (cons nil nil))))
			   ,(funcall (ef-code-to-octets b) code `(car ,nstate)
				     `(lambda (x)
				       ,(funcall (ef-code-to-octets a)
						 'x `(cdr ,nstate) output error))))))
    :cache (make-array +ef-max+ :initial-element nil)
    :min (* (ef-min-octets a) (ef-min-octets b))
    :max (* (ef-max-octets a) (ef-max-octets b)))
   nil
   nil #() '()))

(defun find-external-format (name &optional (error-p t))
  (when (external-format-p name)
    (return-from find-external-format name))

  (or (if (consp name) (every #'keywordp name) (keywordp name))
      (error (intl:gettext "~S is not a valid external format name.") name))

  (when (eq name :default)
    (setq name *default-external-format*))

  (when (and (consp name) (not (cdr name)))
    (setq name (car name)))

  (flet ((not-found ()
	   (when (equal *default-external-format* name)
	     (setq *default-external-format* :iso8859-1))
	   (if error-p (error 'external-format-not-found :name name) nil)))
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

;; Create a trie that is the inverse of the given table.  The table
;; can be an array of rank 1 or 2.  Each dimension is limited to a
;; maximum of 254.
;;
;; Let table have rank 1 and let (aref table k) = uuuu.  Then
;; INVERT-TABLE creates a trie such that (get-inverse (invert-table
;; table) uuuu) = #xbb and (aref table (1- #xbb)) = uuuu.  If,
;; however, the table has no entry that matches uuuu, get-inverse
;; returns NIL.
;;
;; For a table of rank 2, we have (aref table m n) = uuuu, Then
;; INVERT-TABLE creates a trie such that (get-inverse (invert-table
;; table) uuuu) = #xaabb, where (aref table (1- #xbb) (1- #xaa)) =
;; uuuu.
(defun invert-table (table)
  (declare (type (or (simple-array (unsigned-byte 31) *)
		     (simple-array (unsigned-byte 16) *))
		 table)
	   (optimize (speed 3) (space 0) (safety 0) (debug 0)
		     (ext:inhibit-warnings 3)))
  (or (gethash table *.table-inverse.*)
      (let* ((mbits (if (= (array-total-size table) 128) 7 8))
	     (lbits (cond ((> (array-total-size table) 256) 3)
			  ((< (array-total-size table) 100) 6)
			  (t 5)))
	     (hvec (make-array (1+ (ash #x110000 (- 0 mbits lbits)))
			       :element-type '(unsigned-byte 16)
			       :initial-element #xFFFF))
	     (mvec (make-array 0 :element-type '(unsigned-byte 16)))
	     (lvec (make-array 0 :element-type '(unsigned-byte 16)))
	     (width (array-dimension table 0))
	     (power (1- (array-rank table)))
	     (base (if (= width 94) 1 0))
	     hx mx lx)
	(assert (and (< power 2) (<= width 16384)))
	(dotimes (i (array-total-size table))
	  (declare (type (integer 0 (#.array-dimension-limit)) i))
	  (let ((tmp i) (val (row-major-aref table i)) (z 0))
	    (declare (type (integer 0 (#.array-dimension-limit)) tmp)
		     (type (unsigned-byte 16) z))
	    (unless (= val #xFFFE)
	      (when (plusp power)
		(multiple-value-bind (x y) (floor tmp width)
		  (setq tmp x)
		  (setq z (logior z (ash (the (integer 0 255) (+ y base))
					 (the (integer 0 24)
					   (* 8 power)))))))
	      (setq hx (ash val (- 0 mbits lbits)))
	      (when (= (aref hvec hx) #xFFFF)
		(setf (aref hvec hx) (length mvec))
		(let ((tmp (make-array (+ (length mvec) (ash 1 mbits))
				       :element-type '(unsigned-byte 16)
				       :initial-element #xFFFF)))
		  (replace tmp mvec)
		  (setq mvec tmp)))
	      (setq mx (logand (ash val (- lbits)) (lognot (ash -1 mbits))))
	      (when (= (aref mvec (+ (aref hvec hx) mx)) #xFFFF)
		(setf (aref mvec (+ (aref hvec hx) mx)) (length lvec))
		(let ((tmp (make-array (+ (length lvec) (ash 1 lbits))
				       :element-type '(unsigned-byte 16)
				       :initial-element #xFFFF)))
		  (replace tmp lvec)
		  (setq lvec tmp)))
	      (setq lx (logand val (lognot (ash -1 lbits))))
	      (setf (aref lvec (+ (aref mvec (+ (aref hvec hx) mx)) lx))
		  (logior z (+ tmp base))))))
	(setf (gethash table *.table-inverse.*)
	    (lisp::make-ntrie16 :split (logior (ash (1- mbits) 4) (1- lbits))
				:hvec hvec :mvec mvec :lvec lvec)))))

(declaim (inline get-inverse))
(defun get-inverse (ntrie code)
  (declare (type lisp::ntrie16 ntrie) (type (integer 0 #x10FFFF) code))
  (let ((n (lisp::qref ntrie code)))
    (and n (let ((m (aref (lisp::ntrie16-lvec ntrie) n)))
	     (if (= m #xFFFF) nil m)))))


(define-condition void-external-format (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream (intl:gettext "Attempting I/O through void external-format.")))))

(define-external-format :void (:size 0 :documentation
"Void external format that signals an error on any input or output.")
 ()
  (octets-to-code (state input unput error)
    `(error 'void-external-format))
  (code-to-octets (code state output error)
    `(error 'void-external-format)))

(define-external-format :iso8859-1 (:size 1 :documentation
"ISO8859-1 is an 8-bit character encoding generally intended for
Western European languages including English, German, Italian,
Norwegian, Portuguese, Spanish, Swedish and many others.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ()
  (octets-to-code (state input unput error)
    `(values ,input 1))
  (code-to-octets (code state output error)
    `(,output (if (> ,code 255)
		  (if ,error
		      (locally
			  ;; No warnings about fdefinition
			  (declare (optimize (ext:inhibit-warnings 3)))
			(funcall ,error
				 (intl:gettext "Cannot output codepoint #x~X to ISO8859-1 stream")
				 ,code 1))
		      #x3F)
		  ,code))))

;;; OCTETS-TO-CODEPOINT, CODEPOINT-TO-OCTETS  -- Semi-Public
;;;
;;; Normally you'd want to use OCTETS-TO-CHAR and CHAR-TO-OCTETS instead of
;;; these, but that limits you to Lisp's idea of a character - either Latin-1
;;; in 8 bit Lisp images, or the Unicode BMP in 16 bit images.  If you want
;;; to read or write texts containing characters not supported by your Lisp,
;;; these macros can be used instead.
(defmacro octets-to-codepoint (external-format state count input unput &optional error)
  (let ((tmp1 (gensym)) (tmp2 (gensym))
	(ef (find-external-format external-format)))
    `(multiple-value-bind (,tmp1 ,tmp2)
	 ,(funcall (ef-octets-to-code ef) state input unput error)
       (setf ,count (the kernel:index ,tmp2))
       (the (or lisp:codepoint null) ,tmp1))))

(defmacro codepoint-to-octets (external-format code state output &optional error)
  (let ((ef (find-external-format external-format)))
    (funcall (ef-code-to-octets ef) code state output error)))



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
;;; Create an ef-macro (external-format macro).  This creates a
;;; function named Name that will process an external format in the
;;; desired way.
;;;
;;; Paul Foley says:
;;;   All the existing ef-macros are provided with the implementation,
;;;   so they all use lisp::lisp as the id; it's intended for people
;;;   who want to write their own macros~there are some number of
;;;   slots (+ef-max+) used by the implementation; the idea is that
;;;   you can write something like (def-ef-macro foo (ef my-tag 4 1)
;;;   ...) to implement 1 of a total of 4 new macros in your own
;;;   "namespace", without having to know how many are implemented by
;;;   others (e.g., the 10 used by the base implementation...which
;;;   could change with the next release -- and if several libraries
;;;   each add their own, the total number, and the position of each
;;;   one's slots within that total, may change depending on load
;;;   order, etc.)  When you write the above, it allocates 4 new
;;;   places and associates the base index with "my-tag", then the
;;;   "idx" value is relative to that base.  The id lisp:lisp always
;;;   has its base at 0, so it doesn't need to go through ensure-cache
;;;   to find that out.
(defmacro def-ef-macro (name (ef id reqd idx) body)
  (let* ((tmp1 (gensym))
	 (tmp2 (gensym))
	 (blknm (nth-value 1 (lisp::valid-function-name-p name)))
	 (%name (intern (format nil "%~A" name) #|(symbol-package blknm)|#)))
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
				    ;; can see compiler messages.  If we don't,
				    ;; we run into a problem that we might be
				    ;; changing the default format while we're
				    ;; compiling, and we don't know how to output
				    ;; the compiler messages.
				    #|(*default-external-format* :iso8859-1)|#)
				(compile nil `(lambda (%slots%)
						(declare (ignorable %slots%))
						(block ,',blknm
						  ,,body))))))
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
(defmacro octets-to-char (external-format state count input unput &optional error)
  (let ((nstate (gensym)))
    `(let ((,nstate ,state))
       (when (null ,nstate) (setq ,nstate (setf ,state (cons nil nil))))
       (if (car ,nstate)
	   ;; Return the trailing surrgate.  Must set count to 0 to
	   ;; tell the stream code we didn't consume any octets!
	   (prog1 (the character (car ,nstate))
	     (setf (car ,nstate) nil ,count 0))
	   (let ((code (octets-to-codepoint ,external-format
					    (cdr ,nstate) ,count ,input ,unput ,error)))
	     (declare (type lisp:codepoint code))
	     ;;@@ on non-Unicode builds, limit to 8-bit chars
	     ;;@@ if unicode-bootstrap, can't use #\u+fffd
	     (cond ((or (lisp::surrogatep code) (>= code lisp:codepoint-limit))
		    ;; Surrogate characters (that weren't combined
		    ;; into a codepoint by octets-to-codepoint) are
		    ;; illegal.  So are codepoints that are too large.
		    (if ,error
			(if (lisp::surrogatep code)
			    (locally
				(declare (optimize (ext:inhibit-warnings 3)))
			      (funcall ,error
				       (format nil (intl:gettext "Surrogate codepoint #x~~4,'0X is illegal for ~A")
					       ,external-format)
				       code nil))
			    (locally
				(declare (optimize (ext:inhibit-warnings 3)))
			      (funcall ,error (intl:gettext "Illegal codepoint on input: #x~X") code nil)))
			#-(and unicode (not unicode-bootstrap)) #\?
			#+(and unicode (not unicode-bootstrap)) #\U+FFFD))
		   #+unicode
		   ((> code #xFFFF)
		    (multiple-value-bind (hi lo) (surrogates code)
		      (setf (car ,nstate) lo)
		      hi))
		   (t (code-char code))))))))

(defmacro char-to-octets (external-format char state output &optional error)
  (let ((nchar (gensym))
	(nstate (gensym))
	(wryte (gensym))
	(ch (gensym)))
    `(let ((,nchar ,char)
	   (,nstate ,state))
       (when (null ,nstate) (setq ,nstate (setf ,state (cons nil nil))))
       (if (lisp::surrogatep (char-code ,nchar) :high)
	   (setf (car ,nstate) ,nchar)
	   (flet ((,wryte (,ch)
		    (codepoint-to-octets ,external-format ,ch (cdr ,nstate)
					 ,output ,error)))
	     (declare (dynamic-extent #',wryte))
	     (if (car ,nstate)
		 (prog1
		     ;; Invalid surrogate sequences get replaced with
		     ;; the replacement character.
		     (,wryte (if (lisp::surrogatep (char-code ,nchar) :low)
				 (surrogates-to-codepoint (car ,nstate) ,nchar)
				 (if ,error
				     (locally
					 (declare (optimize (ext:inhibit-warnings 3)))
				       (funcall ,error
						(intl:gettext "Cannot convert invalid surrogate #x~X to character")
						,nchar))
				     +replacement-character-code+)))
		   (setf (car ,nstate) nil))
		 ;; A lone trailing (low) surrogate gets replaced with
		 ;; the replacement character.
		 (,wryte (if (lisp::surrogatep (char-code ,nchar) :low)
			     (if ,error
				 (locally
				     (declare (optimize (ext:inhibit-warnings 3)))
				   (funcall ,error
					    (intl:gettext "Cannot convert lone trailing surrogate #x~X to character")
					    ,nchar))
				 +replacement-character-code+)
			     (char-code ,nchar)))))))))

(defmacro flush-state (external-format state output &optional error)
  (let* ((ef (find-external-format external-format))
	 (f (ef-flush-state ef)))
    (when f
      (funcall f state output error))))

(defmacro copy-state (external-format state)
  (let* ((ef (find-external-format external-format))
	 (f (ef-copy-state ef)))
    (when f
      (funcall f state))))

(def-ef-macro ef-string-to-octets (extfmt lisp::lisp +ef-max+ +ef-so+)
  `(lambda (string start end buffer buffer-start buffer-end error bufferp
	    &aux (ptr buffer-start) (state nil) (last-octet buffer-start))
     (declare #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#
	      (type simple-string string)
	      (type kernel:index start end ptr)
	      (type (simple-array (unsigned-byte 8) (*)) buffer)
	      (ignorable state))
     (if bufferp
	 (block ef-string-done
	   (dotimes (i (- end start) (values buffer ptr i))
	     (declare (type kernel:index i))
	     (char-to-octets ,extfmt (schar string (+ start i)) state
			     (lambda (b)
			       (when (= ptr buffer-end)
				 (return-from ef-string-done
				   (values buffer last-octet i)))
			       (setf (aref buffer (1- (incf ptr))) b))
			     error)
	     (setf last-octet ptr)))
	 (dotimes (i (- end start) (values buffer ptr i))
	   (declare (type kernel:index i))
	   (char-to-octets ,extfmt (schar string (+ start i)) state
			   (lambda (b)
			     (when (= ptr (length buffer))
			       (setq buffer (adjust-array buffer (* 2 ptr))))
			     (setf (aref buffer (1- (incf ptr))) b))
			   error)))))

(defun string-to-octets (string &key (start 0) end (external-format :default)
				     (buffer nil bufferp)
				     (buffer-start 0)
			             error)
  "Convert String to octets using the specified External-format.  The
  string is bounded by Start (defaulting to 0) and End (defaulting to
  the end of the string.  If Buffer is given, the octets are stored
  there.  If not, a new buffer is created.  Buffer-start specifies
  where in the buffer the first octet will be placed.

  Three values are returned: The buffer, the number of valid octets
  written, and the number of characters converted.  Note that the
  actual number of octets written may be greater than the returned
  value, These represent the partial octets of the next character to
  be converted, but there was not enough room to hold the complete set
  of octets."
  (declare (type string string)
	   (type kernel:index start)
	   (type (or kernel:index null) end)
	   (type (or (array (unsigned-byte 8) (*)) null) buffer))
  (let* ((buffer (or buffer (make-array (length string)
					:element-type '(unsigned-byte 8)))))
    (lisp::with-array-data ((b buffer) (b-start)
			    (b-end))
      (multiple-value-bind (result ptr octets)
	  (lisp::with-array-data ((string string) (start start) (end end))
	    (funcall (ef-string-to-octets external-format)
		     string start end b
		     (+ b-start buffer-start) b-end
		     error bufferp))
	(values (if bufferp buffer (lisp::shrink-vector result ptr))
		(- ptr b-start buffer-start) octets)))))

(def-ef-macro ef-octets-to-string (extfmt lisp::lisp +ef-max+ +ef-os+)
  `(lambda (octets ptr end state string s-start s-end error
	    &aux (pos s-start) (count 0) (last-octet 0))
     (declare (optimize (speed 3) (safety 0) #|(space 0) (debug 0)|#)
	      (type (simple-array (unsigned-byte 8) (*)) octets)
	      (type kernel:index pos end count last-octet s-start s-end)
	      (type (integer -1 (#.array-dimension-limit)) ptr)
	      (type simple-string string)
	      (ignorable state))
     (catch 'end-of-octets
       (loop while (< pos s-end)
	  do (setf (schar string pos)
		   (octets-to-char ,extfmt state count
				   (if (>= ptr end)
				       (throw 'end-of-octets nil)
				       (aref octets (incf ptr)))
				   (lambda (n) (decf ptr n))
				   error))
	  (incf pos)
	  (incf last-octet count)))
     (values string pos last-octet state)))

(defun octets-to-string (octets &key (start 0) end (external-format :default)
				     (string nil stringp)
			             (s-start 0) (s-end nil s-end-p)
			             (state nil)
			             error)
  "Octets-to-string converts an array of octets in Octets to a string
  according to the specified External-format.  The array of octets is
  bounded by Start (defaulting ot 0) and End (defaulting to the end of
  the array.  If String is not given, a new string is created.  If
  String is given, the converted octets are stored in String, starting
  at S-Start (defaulting to the 0) and ending at S-End (defaulting to
  the length of String).  If the string is not large enough to hold
  all of characters, then some octets will not be converted.  A State
  may also be specified; this is used as the state of the external
  format.  An error method may also be specified by Error, which
  defaults to NIL to mean the default handling of conversion errors is
  done.

  Four values are returned: the string, the position of where the next
  character would be read into the string, the number of octets
  actually consumed and the new state of the external format."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
	   (type kernel:index start s-start)
	   (type (or kernel:index null) end)
	   (type (or simple-string null) string))
  (let ((s-end (if s-end-p
		   s-end
		   (if stringp
		       (length string)
		       (length octets)))))
    (multiple-value-bind (string pos last-octet new-state)
	(funcall (ef-octets-to-string external-format)
		 octets (1- start) (1- (or end (length octets)))
		 state
		 (or string (make-string (length octets)))
		 s-start s-end
		 error)
      (values (if stringp string (lisp::shrink-vector string pos)) pos last-octet new-state))))


(def-ef-macro ef-octets-to-string-counted (extfmt lisp::lisp +ef-max+ +ef-osc+)
  `(lambda (octets ptr end state ocount string s-start s-end error
	    &aux (pos s-start) (last-octet 0))
     (declare (optimize (speed 3) (safety 0) #|(space 0) (debug 0)|#)
	      (type (simple-array (unsigned-byte 8) (*)) octets ocount)
	      (type kernel:index pos end last-octet s-start s-end)
	      (type (integer -1 (#.array-dimension-limit)) ptr)
	      (type simple-string string)
	      (ignorable state))
     (catch 'end-of-octets
       (loop for k of-type fixnum from 0 
	  while (< pos s-end)
	  do (setf (schar string pos)
		   (octets-to-char ,extfmt state (aref ocount k)
				   (if (>= ptr end)
				       (throw 'end-of-octets nil)
				       (aref octets (incf ptr)))
				   (lambda (n) (decf ptr n))
				   error))
	  (incf pos)
	  (incf last-octet (aref ocount k))))
     (values string pos last-octet state)))

;; Like OCTETS-TO-STRING, but we take an extra argument which is an
;; array which will contain the number of octets read for each
;; character placed in the output string.
(defun octets-to-string-counted (octets ocount
				 &key (start 0) end (external-format :default)
				 (string nil stringp)
				 (s-start 0) (s-end nil s-end-p)
				 (state nil)
				 error)
  "Octets-to-string converts an array of octets in Octets to a string
  according to the specified External-format.  The array of octets is
  bounded by Start (defaulting ot 0) and End (defaulting to the end of
  the array.  If String is not given, a new string is created.  If
  String is given, the converted octets are stored in String, starting
  at S-Start (defaulting to the 0) and ending at S-End (defaulting to
  the length of String).  If the string is not large enough to hold
  all of characters, then some octets will not be converted.  A State
  may also be specified; this is used as the state of the external
  format.

  In Ocount, the number of octets read for each character in the
  string is saved

  Four values are returned: the string, the number of characters read,
  the number of octets actually consumed and the new state of the
  external format."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets ocount)
	   (type kernel:index start s-start)
	   (type (or kernel:index null) end)
	   (type (or simple-string null) string))
  (let ((s-end (if s-end-p
		   s-end
		   (if stringp
		       (length string)
		       (length octets)))))
    (multiple-value-bind (string pos last-octet new-state)
	(funcall (ef-octets-to-string-counted external-format)
		 octets (1- start) (1- (or end (length octets)))
		 state
		 ocount
		 (or string (make-string (length octets)))
		 s-start s-end
		 error)
      (values (if stringp string (lisp::shrink-vector string pos)) (- pos s-start) last-octet new-state))))



(def-ef-macro ef-encode (extfmt lisp::lisp +ef-max+ +ef-en+)
  `(lambda (string start end result error  &aux (ptr 0) (state nil))
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
			       (code-char b)))
		       error))))

(defun string-encode (string external-format &optional (start 0) end error)
  "Encode the given String using External-Format and return a new
  string.  The characters of the new string are the octets of the
  encoded result, with each octet converted to a character via
  code-char.  This is the inverse to String-Decode"
  (when (zerop (length string))
    (return-from string-encode string))
  (multiple-value-bind (result ptr)
      (lisp::with-array-data ((string string) (start start) (end end))
	(funcall (ef-encode external-format) string start end
		 (make-string (length string) :element-type 'base-char)
		 error))
    (lisp::shrink-vector result ptr)))

(def-ef-macro ef-decode (extfmt lisp::lisp +ef-max+ +ef-de+)
  `(lambda (string ptr end result error &aux (pos -1) (count 0) (state nil))
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
			       (lambda (n) (decf ptr n))
			       error))
	finally (return (values result (1+ pos))))))

(defun string-decode (string external-format &optional (start 0) end error)
  "Decode String using the given External-Format and return the new
  string.  The input string is treated as if it were an array of
  octets, where the char-code of each character is the octet.  This is
  the inverse of String-Encode."
  (when (zerop (length string))
    (return-from string-decode string))
  (multiple-value-bind (result pos)
      (lisp::with-array-data ((string string) (start start) (end end))
	(funcall (ef-decode external-format)
		 string (1- start) (1- end) (make-string (length string))
		 error))
    (lisp::shrink-vector result pos)))


(defun set-system-external-format (terminal &optional filenames)
  "Change the external format of the standard streams to Terminal.
  The standard streams are sys::*stdin*, sys::*stdout*, and
  sys::*stderr*, which are normally the input and/or output streams
  for *standard-input* and *standard-output*.  Also sets sys::*tty*
  (normally *terminal-io* to the given external format.  The value of
  *default-external-format* is not changed.

  If the optional argument Filenames is given, then the filename
  encoding is set to the specified format, if it has not already been
  specified previously."
  (unless (find-external-format terminal)
    (error (intl:gettext "Can't find external-format ~S.") terminal))
  (setf (stream-external-format sys:*stdin*) terminal
	(stream-external-format sys:*stdout*) terminal
	(stream-external-format sys:*stderr*) terminal)
  (when (lisp::fd-stream-p sys:*tty*)
    (setf (stream-external-format sys:*tty*) terminal))
  (when filenames
    (unless (find-external-format filenames)
      (error (intl:gettext "Can't find external-format ~S.") filenames))
    (setq filenames (ef-name (find-external-format filenames)))
    (when (and (not (eq unix::*filename-encoding* :null))
	       (not (eq unix::*filename-encoding* filenames)))
      (cerror (intl:gettext "Change it anyway.")
	      (intl:gettext "The external-format for encoding filenames is already set.")))
    (setq unix::*filename-encoding* filenames))
  t)


;; Despite its name, this doesn't actually compile anything at all.  What it
;; does is expand into a lambda expression that can be compiled by the file
;; compiler.
(defmacro precompile-ef-slot (ef slot)
  (let* ((ef (find-external-format ef)))
    ;; if there's no lambda expression available, flush it and regenerate
    (unless (and (aref (ef-cache ef) slot)
		 (function-lambda-expression (aref (ef-cache ef) slot)))
      (setf (aref (ef-cache ef) slot) nil)
      (ecase slot
	(#.+ef-cin+ (lisp::%ef-cin ef))
	(#.+ef-cout+ (lisp::%ef-cout ef))
	(#.+ef-sout+ (lisp::%ef-sout ef))
	(#.+ef-os+ (%ef-octets-to-string ef))
	(#.+ef-so+ (%ef-string-to-octets ef))
	(#.+ef-en+ (%ef-encode ef))
	(#.+ef-de+ (%ef-decode ef))
	(#.+ef-osc+ (%ef-octets-to-string-counted ef))))
    `(setf (aref (ef-cache (find-external-format ,(ef-name ef))) ,slot)
	 ,(subst (ef-name ef) ef
		 (function-lambda-expression (aref (ef-cache ef) slot))))))

;;; Builtin external formats.

;; A safe UTF-8 external format.  Any illegal UTF-8 sequences on input
;; are replaced with the Unicode REPLACEMENT CHARACTER (U+FFFD), or
;; signals an error as appropriate.
;;
;; See Table 3-7, Ch 3.9 in the Unicode book.

(define-external-format :utf-8 (:min 1 :max 4 :documentation 
"UTF-8 is a variable-length character encoding for Unicode.  By
default, illegal input sequences are replaced by the Unicode
replacement character.")

  ()
  (octets-to-code (state input unput error c i j n)
    `(labels ((utf8 (,c ,i)
		(declare (type (unsigned-byte 8) ,c)
			 (type (integer 1 5) ,i))
		(let ((,n (ash (ldb (byte (- 6 ,i) 0) ,c)
			       (* 6 ,i))))
		  (declare (type (unsigned-byte 31) ,n))
		  (dotimes (,j ,i (check ,n ,i))
		    (let ((,c ,input))
		      ;; Following bytes must all have the form
		      ;; #b10xxxxxx.  If not, put back the octet we
		      ;; just read and return the replacement character
		      ;; for the bad sequence.
		      (if (< (logxor ,c #x80) #x40)
			  (setf (ldb (byte 6 (* 6 (- ,i ,j 1))) ,n)
				(ldb (byte 6 0) ,c))
			  (progn
			    (,unput 1)
			    (return
			      (values
			       (locally
				   ;; No warnings about fdefinition
				   (declare (optimize (ext:inhibit-warnings 3)))
				 (if ,error
				     (funcall ,error "Invalid utf8 octet #x~X at offset ~D"
					      ,c (1+ ,j))
				     +replacement-character-code+))
			       (1+ ,j)))))))))
	      (check (,n ,i)
		(declare (type (unsigned-byte 31) ,n)
			 (type (integer 1 5) ,i))
		;; We check for overlong sequences (sequences that
		;; encode to codepoints that don't need that long of a
		;; sequence) and any surrogate values and any code
		;; outside the 21-bit Unicode range.
		(if (or (>= ,n lisp:codepoint-limit)
			(<= ,n (the (member 127 2047 65535)
				 (svref #(127 2047 65535) (1- ,i)))) ; overlong
			(lisp::surrogatep ,n)) ; surrogate
		    (progn
		      ;; Replace the entire sequence with the
		      ;; replacment character
		      (values (if ,error
				  (cond
				    ((>= ,n lisp:codepoint-limit)
				     (locally
					 ;; No warnings about fdefinition
					 (declare (optimize (ext:inhibit-warnings 3)))
				       (funcall ,error "Invalid codepoint #x~X of ~D octets"
						,n (1+ ,i))))
				    ((lisp::surrogatep ,n)
				     (locally
					 ;; No warnings about fdefinition
					 (declare (optimize (ext:inhibit-warnings 3)))
				       (funcall ,error "Invalid surrogate code #x~X" ,n (1+ ,i))))
				    (t
				     (locally
					 ;; No warnings about fdefinition
					 (declare (optimize (ext:inhibit-warnings 3)))
				       (funcall ,error "Overlong utf8 sequence of ~*~D octets" nil (1+ ,i)))))
				  +replacement-character-code+)
			      (1+ ,i)))
		    (values ,n (1+ ,i)))))
      (let ((,c ,input))
	(declare (optimize (ext:inhibit-warnings 3)))
	(cond ((null ,c) (values nil 0))
	      ((< ,c #b10000000) (values ,c 1))
	      ((< ,c #b11000010)
	       (values
		(locally
		    ;; No warnings about fdefinition
		    (declare (optimize (ext:inhibit-warnings 3)))
		  (if ,error
		      (funcall ,error "Invalid initial utf8 octet: #x~X" ,c 1)
		      +replacement-character-code+))
		       1))
	      ((< ,c #b11100000) (utf8 ,c 1))
	      ((< ,c #b11110000) (utf8 ,c 2))
	      ((< ,c #b11111000) (utf8 ,c 3))
	      (t
	       (values
		(locally
		    ;; No warnings about fdefinition
		    (declare (optimize (ext:inhibit-warnings 3)))
		  (if ,error
		      (funcall ,error "Invalid initial utf8 octet: #x~X" ,c 1)
		      +replacement-character-code+))
		1))))))
  (code-to-octets (code state output error i j n p init)
    `(flet ((utf8 (,n ,i)
          (let* ((,j (- 6 ,i))
             (,p (* 6 ,i))
             (,init (logand #xFF (ash #b01111110 ,j))))
        (,output (logior ,init (ldb (byte ,j ,p) ,n)))
        (dotimes (,i ,i)
          (decf ,p 6)
          (,output (logior 128 (ldb (byte 6 ,p) ,n)))))))
       (declare (optimize (ext:inhibit-warnings 3)))
       (cond ((< ,code #x80) (,output ,code))
         ((< ,code #x800) (utf8 ,code 1))
         ((< ,code #x10000) (utf8 ,code 2))
         ((< ,code #x110000) (utf8 ,code 3))
         (t (error "How did this happen?  Codepoint U+~X is illegal" ,code))))))

(define-external-format :ascii (:size 1 :documentation
"US ASCII 7-bit encoding.  Illegal input sequences are replaced with
the Unicode replacment character.  Illegal output characters are
replaced with a question mark.")
  ()
  (octets-to-code (state input unput error c)
    `(let ((,c ,input))
       (values (if (< ,c #x80)
		   ,c
		   (if ,error
		       (locally
			   ;; No warnings about fdefinition
			   (declare (optimize (ext:inhibit-warnings 3)))
			 (funcall ,error "Invalid octet #x~4,'0X for ASCII" ,c 1))
		       +replacement-character-code+))
	       1)))
  (code-to-octets (code state output error)
    `(,output (if (> ,code #x7F)
		  (if ,error
		      (locally
			  ;; No warnings about fdefinition
			  (declare (optimize (ext:inhibit-warnings 3)))
			(funcall ,error "Cannot output codepoint #x~X to ASCII stream" ,code))
		      #x3F)
		  ,code))))
