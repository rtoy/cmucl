;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/extfmts.lisp,v 1.3 2008/06/19 01:41:34 rtoy Exp $")
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

(defconstant +ef-os+ 2)
(defconstant +ef-so+ 3)
(defconstant +ef-en+ 4)
(defconstant +ef-de+ 5)
(defconstant +ef-max+ 6)

(define-condition external-format-not-implemented (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "Attempting unimplemented external-format I/O."))))

(defun %efni (a b c d)
  (declare (ignore a b c d))
  (error 'external-format-not-implemented))

(defstruct (external-format
             (:conc-name ef-)
             (:print-function %print-external-format)
             (:constructor make-external-format (name composingp
						 &optional slots slotd
							   octets-to-code
							   code-to-octets)))
  (name (ext:required-argument) :type (or keyword cons) :read-only t)
  (composingp (ext:required-argument) :type boolean :read-only t)
  (slots #() :type simple-vector :read-only t)
  (slotd nil :type list :read-only t)
  (octets-to-code #'%efni :type function :read-only t)
  (code-to-octets #'%efni :type function :read-only t)
  (cache (make-array +ef-max+ :initial-element nil)))

(defun %print-external-format (ef stream depth)
  (declare (ignore depth))
  (print-unreadable-object (ef stream :type t :identity t)
    (princ (ef-name ef) stream)))

(defun %whatsit (ef)
  (setf (gethash (ef-name ef) *external-formats*) ef))

(defmacro define-external-format (name octets-to-code code-to-octets)
  (let ((tmp1 (gensym)) (tmp2 (gensym)))
    `(macrolet ((octets-to-code ((state input unput &rest vars) body)
		  `(lambda (,',tmp1 ,state ,input ,unput)
		     (declare (ignore ,',tmp1)
			      (ignorable ,state ,input ,unput)
			      (optimize (ext:inhibit-warnings 3)))
		     (let ((,input `(the (or (unsigned-byte 8) null) ,,input))
			   ,@(loop for var in vars collect `(,var (gensym))))
		       ,body)))
		(code-to-octets ((code state output &rest vars) body)
		  `(lambda (,',tmp1 ,',tmp2 ,state ,output)
		     (declare (ignore ,',tmp1)
			      (ignorable ,state ,output)
			      (optimize (ext:inhibit-warnings 3)))
		     (let ((,code ',code)
			   ,@(loop for var in vars collect `(,var (gensym))))
		       `(let ((,',code (the (unsigned-byte 31) ,,',tmp2)))
			  (declare (ignorable ,',code))
			  ,,body)))))
       (%whatsit (make-external-format ,name nil #() '()
				       ,octets-to-code ,code-to-octets)))))

(defmacro define-composing-external-format (name input output)
  (let ((tmp1 (gensym)) (tmp2 (gensym)))
    `(macrolet ((input ((state input unput &rest vars) body)
		  `(lambda (,',tmp1 ,state ,input ,unput)
		     (declare (ignore ,',tmp1)
			      (ignorable ,state ,input ,unput)
			      (optimize (ext:inhibit-warnings 3)))
		     (let ((,input `(the (values (or (unsigned-byte 31) null)
						 lisp::index)
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
       (%whatsit (make-external-format ,name t #() '()
				       ,input ,output)))))

(defun load-external-format-aliases ()
  (let ((*package* (find-package "KEYWORD")))
    (with-open-file (stm "library:ext-formats/aliases" :if-does-not-exist nil)
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
  #+(or)
  (unless (ext:search-list-defined-p "ef:")
    (setf (ext:search-list "ef:") '("library:ef/")))

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
		 (lisp::*enable-package-locked-errors* nil))
             (load (format nil "library:ext-formats/~(~A~)" name)
		   :if-does-not-exist nil))
           (gethash name *external-formats*))))

(defun %composed-ef-name (a b)
  (if (consp a) (append a (list b)) (list a b)))

(defun %compose-external-formats (a b &optional name)
  (when (ef-composingp a)
    (error "~S is a Composing-External-Format." (ef-name a)))
  (unless (ef-composingp b)
    (error "~S is not a Composing-External-Format." (ef-name b)))
  (make-external-format (or name (%composed-ef-name (ef-name a) (ef-name b)))
			nil #() '()
			(lambda (tmp state input unput)
			  (declare (ignore tmp))
			  (funcall (ef-octets-to-code b) b
				   state
				   (funcall (ef-octets-to-code a) a
					    state
					    input
					    unput)
				   unput))
			(lambda (tmp code state output)
			  (declare (ignore tmp))
			  (funcall (ef-code-to-octets b) b
				   code
				   state
				   `(lambda (x)
				     ,(funcall (ef-code-to-octets a) a
					       'x state output))))))

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
		  (%whatsit (reduce #'%compose-external-formats efs))))))
      (or (%find-external-format name)
	  (if error-p (error "External format ~S not found." name) nil))))

(define-condition void-external-format (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "Attempting I/O through void external-format."))))

(define-external-format :void
  (octets-to-code (state input unput)
    `(error 'void-external-format))
  (code-to-octets (code state output)
    `(error 'void-external-format)))

(define-external-format :iso8859-1
  (octets-to-code (state input unput)
    `(values ,input 1))
  (code-to-octets (code state output)
    `(,output (if (> ,code 255) #x3F ,code))))

(defmacro octets-to-codepoint (external-format state count input unput)
  (let ((tmp1 (gensym)) (tmp2 (gensym)))
    `(let ((body (funcall (ef-octets-to-code ,external-format) ,external-format
			  ',state ',input ',unput)))
       `(multiple-value-bind (,',tmp1 ,',tmp2) ,body
	  (setf ,',count (the lisp::index ,',tmp2))
	  (the (or (unsigned-byte 31) null) ,',tmp1)))))

(defmacro codepoint-to-octets (external-format code state output)
  `(funcall (ef-code-to-octets ,external-format) ,external-format
	    ',code ',state ',output))



(defvar *ef-base* +ef-max+)
(defvar *ef-extensions* '())

(defun ensure-cache (ef id reqd)
  (let ((base (or (getf *ef-extensions* id)
		  (setf (getf *ef-extensions* id)
		      (prog1 *ef-base* (incf *ef-base* reqd))))))
    (when (< (length (ef-cache ef)) (+ base reqd))
      (setf (ef-cache ef)
	  (adjust-array (ef-cache ef) (+ base reqd) :initial-element nil)))
    base))

(defmacro def-ef-macro (name (ef id reqd idx) body)
  (let ((tmp (gensym)))
    `(defun ,name (,ef)
       (let ((,tmp ,(if (eq id 'lisp::lisp)
			idx
			`(+ (ensure-cache ,ef ',id ,reqd) ,idx))))
	 (or (aref (ef-cache ,ef) ,tmp)
	     (setf (aref (ef-cache ,ef) ,tmp)
		 (let ((*compile-print* nil)) (compile nil ,body))))))))



(defmacro octets-to-char (external-format state count input unput)
  `(let ((body (octets-to-codepoint ,external-format
				    ,state ,count ,input ,unput)))
     `(let ((code ,body))
        (declare (type (unsigned-byte 31) code))
        (if (< code #x100) (code-char code) #\?))))

(defmacro char-to-octets (external-format char state output)
  `(codepoint-to-octets ,external-format (char-code ,char) ,state ,output))

(def-ef-macro ef-string-to-octets (extfmt lisp::lisp +ef-max+ +ef-so+)
  `(lambda (string start end buffer &aux (ptr 0) (state nil))
     (declare #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#
	      (type simple-string string)
	      (type lisp::index start end ptr)
	      (type (simple-array (unsigned-byte 8) (*)) buffer)
	      (ignorable state))
     (dotimes (i (- end start) (values buffer ptr))
       (declare (type lisp::index i))
       ,(char-to-octets extfmt (schar string (+ start i)) state
			(lambda (b)
			  (when (= ptr (length buffer))
			    (setq buffer (adjust-array buffer (* 2 ptr))))
			  (setf (aref buffer (1- (incf ptr))) b))))))

(defun string-to-octets (string &key (start 0) end (external-format :default)
				     (buffer nil bufferp))
  (declare (type string string)
	   (type lisp::index start)
	   (type (or lisp::index null) end)
	   (type (or (simple-array (unsigned-byte 8) (*)) null) buffer))
  (multiple-value-bind (buffer ptr)
      (lisp::with-array-data ((string string) (start start) (end end))
	(funcall (ef-string-to-octets (find-external-format external-format))
		 string start end
		 (or buffer (make-array (length string)
					:element-type '(unsigned-byte 8)))))
    (values (if bufferp buffer (lisp::shrink-vector buffer ptr)) ptr)))

(def-ef-macro ef-octets-to-string (extfmt lisp::lisp +ef-max+ +ef-os+)
  `(lambda (octets ptr end string &aux (pos -1) (count 0) (state nil))
     (declare #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#
	      (type (simple-array (unsigned-byte 8) (*)) octets)
	      (type lisp::index end count)
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
	finally (return (values string pos)))))

(defun octets-to-string (octets &key (start 0) end (external-format :default)
				     (string nil stringp))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
	   (type lisp::index start)
	   (type (or lisp::index null) end)
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
	      (type lisp::index start end ptr)
	      (type simple-base-string result)
	      (ignorable state))
     (dotimes (i (- end start) (values result ptr))
       (declare (type lisp::index i))
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
	      (type lisp::index end count)
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
	finally (return (values result pos)))))

(defun string-decode (string external-format &optional (start 0) end)
  (multiple-value-bind (result pos)
      (lisp::with-array-data ((string string) (start start) (end end))
	(funcall (ef-decode (find-external-format external-format))
		 string (1- start) (1- end) (make-string (length string))))
    (lisp::shrink-vector result (1+ pos))))
