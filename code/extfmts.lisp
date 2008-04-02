;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/extfmts.lisp,v 1.2 2007/10/31 14:37:38 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Implementation of external-formats

(in-package "STREAM")

(export '(string-to-octets octets-to-string *default-external-format*
	  encode-string decode-string))

(defvar *default-external-format* :iso8859-1)

(defvar *external-formats* (make-hash-table))
(defvar *external-format-aliases* (make-hash-table))

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
             (:constructor make-external-format (name
						 &optional slots slotd
							   octets-to-code
							   code-to-octets)))
  (name (ext:required-argument) :type keyword :read-only t)
  (slots #() :type simple-vector :read-only t)
  (slotd nil :type list :read-only t)
  (octets-to-code #'%efni :type function :read-only t)
  (code-to-octets #'%efni :type function :read-only t))

(defun %print-external-format (ef stream depth)
  (declare (ignore depth))
  (print-unreadable-object (ef stream :type t :identity t)
    (princ (ef-name ef) stream)))

(defmacro define-external-format (name octets-to-code code-to-octets)
  (let ((tmp (gensym)))
    `(macrolet ((octets-to-code ((state input unput) &body body)
		  `(lambda (,',tmp ,state ,input ,unput)
		     (declare (type (function () (unsigned-byte 8)) ,input)
			      (type (function (lisp::index) t) ,unput)
			      (ignore ,',tmp)
			      (ignorable ,state ,unput)
			      (values (unsigned-byte 31) lisp::index t))
		     ,@body))
		(code-to-octets ((code state output) &body body)
		  `(lambda (,',tmp ,code ,state ,output)
		     (declare (type (unsigned-byte 31) ,code)
			      (type (function ((unsigned-byte 8)) t) ,output)
			      (ignore ,',tmp)
			      (ignorable ,state ,output)
			      (values t))
		     ,@body)))
       (setf (gethash ,name *external-formats*)
	     (make-external-format ,name #() '()
				   ,octets-to-code ,code-to-octets)))))

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

(defun find-external-format (name &optional (error-p t))
  (when (external-format-p name)
    (return-from find-external-format name))

  (when (eq name :default)
    (setq name *default-external-format*))

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
           (gethash name *external-formats*))
      (if error-p (error "External format ~S not found." name) nil)))


(define-condition void-external-format (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "Attempting I/O through void external-format."))))

(define-external-format :void
  (octets-to-code (state input unput)
    (declare (ignore input))
    (error 'void-external-format))
  (code-to-octets (code state output)
    (declare (ignore code))
    (error 'void-external-format)))

(define-external-format :iso8859-1
  (octets-to-code (state input unput)
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    (values (funcall input) 1 nil))
  (code-to-octets (code state output)
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    (funcall output (if (> code 255) #x3F code))
    nil))


(defmacro octets-to-codepoint (external-format state count input unput)
  (let ((tmp1 (gensym)) (tmp2 (gensym)) (tmp3 (gensym)))
    `(multiple-value-bind (,tmp1 ,tmp2 ,tmp3)
         (funcall (ef-octets-to-code ,external-format) ,external-format
		  ,state ,input ,unput)
       (declare (type (unsigned-byte 31) ,tmp1) (type lisp::index ,tmp2))
       (setf ,state ,tmp3 ,count ,tmp2)
       ,tmp1)))

(defmacro codepoint-to-octets (external-format code state output)
  `(progn
     (setf ,state (funcall (ef-code-to-octets ,external-format)
			   ,external-format ,code ,state ,output))
     nil))



(defmacro octets-to-char (external-format state count input unput)
  `(let ((code (octets-to-codepoint ,external-format
				    ,state ,count ,input ,unput)))
     (declare (type (unsigned-byte 31) code))
     (if (< code #x100) (code-char code) #\?)))

(defmacro char-to-octets (external-format char state output)
  `(codepoint-to-octets ,external-format (char-code ,char) ,state ,output))

(defun string-to-octets (string &key (start 0) end (external-format :default)
				     (buffer nil bufferp))
  (declare (type string string)
           (type lisp::index start)
           (type (or null lisp::index) end)
	   (type (or null (simple-array (unsigned-byte 8) (*))) buffer)
	   #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#)
  (let ((ef (find-external-format external-format))
        (buffer (or buffer (make-array (length string)
				       :element-type '(unsigned-byte 8))))
        (ptr 0)
        (state nil))
    (declare (type external-format ef)
	     (type (simple-array (unsigned-byte 8) (*)) buffer)
	     (type lisp::index ptr))
    (flet ((out (b)
	     (when (= ptr (length buffer))
               (setq buffer (adjust-array buffer (* 2 ptr))))
             (setf (aref buffer (1- (incf ptr))) b)))
      (dotimes (i (- (or end (length string)) start))
        (declare (type lisp::index i))
        (char-to-octets ef (char string (+ start i)) state #'out))
      (values (if bufferp buffer (lisp::shrink-vector buffer ptr)) ptr))))

(defun octets-to-string (octets &key (start 0) end (external-format :default)
				     (string nil stringp))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (type lisp::index start)
           (type (or null lisp::index) end)
	   (type (or null simple-string string))
	   #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#)
  (let ((ef (find-external-format external-format))
        (end (1- (or end (length octets))))
        (string (or string (make-string (length octets))))
        (ptr (1- start))
        (pos 0)
        (count 0)
        (state nil))
    (declare (type external-format ef)
	     (type lisp::index end count)
	     (type (integer -1 (#.array-dimension-limit)) pos ptr)
	     (type simple-string string))
    (flet ((input ()
             (aref octets (incf ptr)))
           (unput (n)
             (decf ptr (the lisp::index n))))
      (loop until (>= ptr end)
            do (when (= pos (length string))
		 (setq string (adjust-array string (* 2 pos))))
	       (setf (schar string (1- (incf pos)))
		   (octets-to-char ef state count #'input #'unput))))
    (values (if stringp string (lisp::shrink-vector string pos)) pos)))



(defun encode-string (string external-format &optional (start 0) end)
  (declare (type string string)
           (type lisp::index start)
           (type (or null lisp::index) end)
	   #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#)
  (let ((ef (find-external-format external-format))
        (result (make-string (length string) :element-type 'base-char))
        (ptr 0)
        (state nil))
    (declare (type external-format ef)
	     (type simple-base-string result)
	     (type lisp::index ptr))
    (flet ((out (b)
	     (when (= ptr (length result))
               (setq result (adjust-array result (* 2 ptr))))
             (setf (char result (1- (incf ptr))) (code-char b))))
      (dotimes (i (- (or end (length string)) start))
        (declare (type lisp::index i))
	(char-to-octets ef (char string (+ start i)) state #'out))
      (lisp::shrink-vector result ptr))))

(defun decode-string (string external-format &optional (start 0) end)
  (declare (type string string)
	   (type lisp::index start)
           (type (or null lisp::index) end)
	   #|(optimize (speed 3) (safety 0) (space 0) (debug 0))|#)
  (let ((ef (find-external-format external-format))
        (end (1- (or end (length string))))
        (result (make-string (length string)))
        (ptr (1- start))
        (pos -1)
        (count 0)
        (state nil))
    (declare (type external-format ef)
	     (type lisp::index end count)
	     (type (integer -1 (#.array-dimension-limit)) pos ptr)
	     (type simple-string result))
    (flet ((input ()
             (char-code (char string (incf ptr))))
           (unput (n)
             (decf ptr (the lisp::index n))))
      (loop until (>= ptr end)
	    ;; increasing size of result shouldn't ever be necessary, unless
	    ;; someone implements an encoding smaller than the source string...
            do (setf (schar result (incf pos))
		   (octets-to-char ef state count #'input #'unput))))
    (lisp::shrink-vector result (1+ pos))))
