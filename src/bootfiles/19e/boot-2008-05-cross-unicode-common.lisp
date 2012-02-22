;;; Common part for cross-compiling 16-bit strings for Unicode.
;;; This part is independent of the architecture.

(load "target:bootfiles/19f/boot-2009-07")

(pushnew :unicode *features*)
(pushnew :unicode-bootstrap *features*)
(in-package "C")

(handler-bind ((error #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'kernel::continue))))
;; Update so we create the correct size of arrays for characters.
(defconstant array-info
  '((base-char #\NULL 16 old-vm:simple-string-type)
    (single-float 0.0f0 32 old-vm:simple-array-single-float-type)
    (double-float 0.0d0 64 old-vm:simple-array-double-float-type)
    #+long-float (long-float 0.0l0 #+x86 96 #+sparc 128
		  old-vm:simple-array-long-float-type)
    #+double-double
    (double-double-float 0w0 128
		  old-vm::simple-array-double-double-float-type)
    (bit 0 1 old-vm:simple-bit-vector-type)
    ((unsigned-byte 2) 0 2 old-vm:simple-array-unsigned-byte-2-type)
    ((unsigned-byte 4) 0 4 old-vm:simple-array-unsigned-byte-4-type)
    ((unsigned-byte 8) 0 8 old-vm:simple-array-unsigned-byte-8-type)
    ((unsigned-byte 16) 0 16 old-vm:simple-array-unsigned-byte-16-type)
    ((unsigned-byte 32) 0 32 old-vm:simple-array-unsigned-byte-32-type)
    ((signed-byte 8) 0 8 old-vm:simple-array-signed-byte-8-type)
    ((signed-byte 16) 0 16 old-vm:simple-array-signed-byte-16-type)
    ((signed-byte 30) 0 32 old-vm:simple-array-signed-byte-30-type)
    ((signed-byte 32) 0 32 old-vm:simple-array-signed-byte-32-type)
    ((complex single-float) #C(0.0f0 0.0f0) 64
     old-vm:simple-array-complex-single-float-type)
    ((complex double-float) #C(0.0d0 0.0d0) 128
     old-vm:simple-array-complex-double-float-type)
    #+long-float
    ((complex long-float) #C(0.0l0 0.0l0) #+x86 192 #+sparc 256
     old-vm:simple-array-complex-long-float-type)
    #+double-double
    ((complex double-double-float) #C(0.0w0 0.0w0) 256
     old-vm::simple-array-complex-double-double-float-type)
    (t 0 32 old-vm:simple-vector-type)))
)

(handler-bind ((error #'(lambda (c)
			  (declare (ignore c))
			  (invoke-restart 'kernel::clobber-it))))
(defstruct (fd-stream
	    (:print-function %print-fd-stream)
	    (:constructor %make-fd-stream)
	    (:include file-stream
		      (misc #'fd-stream-misc-routine)))

  (name nil)		      ; The name of this stream
  (file nil)		      ; The file this stream is for
  ;;
  ;; The backup file namestring for the old file, for :if-exists :rename or
  ;; :rename-and-delete.
  (original nil :type (or simple-string null))
  (delete-original nil)	      ; for :if-exists :rename-and-delete
  ;;
  ;;; Number of bytes per element.
  (element-size 1 :type index)
  (element-type 'base-char)   ; The type of element being transfered.
  (fd -1 :type fixnum)	      ; The file descriptor
  ;;
  ;; Controls when the output buffer is flushed.
  (buffering :full :type (member :full :line :none))
  ;;
  ;; Character position if known.
  (char-pos nil :type (or index null))
  ;;
  ;; T if input is waiting on FD.  :EOF if we hit EOF.
  (listen nil :type (member nil t :eof))
  ;;
  ;; The input buffer.
  (unread nil)
  (ibuf-sap nil :type (or system-area-pointer null))
  (ibuf-length nil :type (or index null))
  (ibuf-head 0 :type index)
  (ibuf-tail 0 :type index)

  ;; The output buffer.
  (obuf-sap nil :type (or system-area-pointer null))
  (obuf-length nil :type (or index null))
  (obuf-tail 0 :type index)

  ;; Output flushed, but not written due to non-blocking io.
  (output-later nil)
  (handler nil)
  ;;
  ;; Timeout specified for this stream, or NIL if none.
  (timeout nil :type (or index null))
  ;;
  ;; Pathname of the file this stream is opened to (returned by PATHNAME.)
  (pathname nil :type (or pathname null))
  ;;
  ;; External formats
  ;; @@ I want to use :default here, but keyword pkg isn't set up yet at boot
  ;; so initialize to NIL and fix it in SET-ROUTINES
  #+unicode
  (external-format nil :type (or null keyword cons))
  #+unicode
  (oc-state nil)
  #+unicode
  (co-state nil)
  #+unicode
  (last-char-read-size 0 :type index)))

(defun %print-fd-stream (fd-stream stream depth)
  (declare (ignore depth) (stream stream))
  (format stream "#<Stream for ~A>"
	  (fd-stream-name fd-stream)))

;; Dump a character of a string to a fasl file in the byte correct
;; order.
(defun dump-string-char (code file)
  ;; Do we want *native-backend* or *target-backend*?  Use
  ;; *native-backend* because we're assuming we're cross-compiling
  ;; from the same arch as the desired arch.
  (ecase (c::backend-byte-order c::*native-backend*)
    (:little-endian
     (dump-byte (ldb (byte 8 0) code) file)
     (dump-byte (ldb (byte 8 8) code) file))
    (:big-endian
     (dump-byte (ldb (byte 8 8) code) file)
     (dump-byte (ldb (byte 8 0) code) file))))

;; Dump a string one character at a time because in the
;; cross-compiler, we're still using 8-bit strings, but want 16-bit
;; strings in the resulting fasl file.
(defun dump-simple-string (s file)
  (declare (type simple-base-string s))
  (let ((length (length s)))
    (dump-fop* length lisp::fop-small-string lisp::fop-string file)
    (dotimes (k length)
      (dump-string-char (char-code (aref s k)) file)))
  (undefined-value))

;; Like dump-simple-string
(defun dump-symbol (s file)
  (let* ((pname (symbol-name s))
	 (pname-length (length pname))
	 (pkg (symbol-package s)))

    (cond ((null pkg)
	   (dump-fop* pname-length lisp::fop-uninterned-small-symbol-save
		      lisp::fop-uninterned-symbol-save file))
	  ;; Why do we do this?  It causes weird things to happen if
	  ;; you're in, say, the KERNEL package when you compile-file
	  ;; something and load the fasl back in when you're in a
	  ;; different package.
	  #-(and)
	  ((eq pkg *package*)
	   (dump-fop* pname-length lisp::fop-small-symbol-save
		      lisp::fop-symbol-save file))
	  ((eq pkg ext:*lisp-package*)
	   (dump-fop* pname-length lisp::fop-lisp-small-symbol-save
		      lisp::fop-lisp-symbol-save file))
	  ((eq pkg ext:*keyword-package*)
	   (dump-fop* pname-length lisp::fop-keyword-small-symbol-save
		      lisp::fop-keyword-symbol-save file))
	  ((< pname-length 256)
	   (dump-fop* (dump-package pkg file)
		      lisp::fop-small-symbol-in-byte-package-save
		      lisp::fop-small-symbol-in-package-save file)
	   (dump-byte pname-length file))
	  (t
	   (dump-fop* (dump-package pkg file)
		      lisp::fop-symbol-in-byte-package-save
		      lisp::fop-symbol-in-package-save file)
	   (dump-unsigned-32 pname-length file)))

    (dotimes (k pname-length)
      (dump-string-char (char-code (aref pname k)) file))

    (unless *cold-load-dump*
      (setf (gethash s (fasl-file-eq-table file)) (fasl-file-table-free file)))

    (incf (fasl-file-table-free file)))

  (undefined-value))

;; We always dump characters in little endian order, which seems to be
;; the way most fops are done.
(defun dump-character (ch file)
  (dump-fop 'lisp::fop-short-character file)
  (let ((code (char-code ch)))
    (dump-byte (ldb (byte 8 0) code) file)
    (dump-byte (ldb (byte 8 8) code) file)))

(defun dump-fixups (fixups file)
  (declare (list fixups) (type fasl-file file))
  (dolist (info fixups)
    (let* ((kind (first info))
	   (fixup (second info))
	   (name (fixup-name fixup))
	   (flavor (fixup-flavor fixup))
	   (offset (third info)))
      (dump-fop 'lisp::fop-normal-load file)
      (let ((*cold-load-dump* t))
	(dump-object kind file))
      (dump-fop 'lisp::fop-maybe-cold-load file)
      (ecase flavor
	(:assembly-routine
	 (assert (symbolp name))
	 (dump-fop 'lisp::fop-normal-load file)
	 (let ((*cold-load-dump* t))
	   (dump-object name file))
	 (dump-fop 'lisp::fop-maybe-cold-load file)
	 (dump-fop 'lisp::fop-assembler-fixup file))
	((:foreign :foreign-data)
	 (assert (stringp name))
	 (if (eq flavor :foreign)
	     (dump-fop 'lisp::fop-foreign-fixup file)
	     (dump-fop 'lisp::fop-foreign-data-fixup file))
	 (let ((len (length name)))
	   (assert (< len 256))
	   (dump-byte len file)
	   (dotimes (i len)
	     (dump-string-char (char-code (schar name i)) file))))
	(:code-object
	 (dump-fop 'lisp::fop-code-object-fixup file)))
      (dump-unsigned-32 offset file)))
  (undefined-value))

(in-package "LISP")

;; See print.lisp.
(defconstant othercase-attribute        (ash 1 9))

(handler-bind ((error #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'kernel::continue))))
  (defconstant attribute-names
    `((number . number-attribute) (lowercase . lowercase-attribute)
      (uppercase . uppercase-attribute) (letter . letter-attribute)
      (sign . sign-attribute) (extension . extension-attribute)
      (dot . dot-attribute) (slash . slash-attribute)
      (other . other-attribute) (funny . funny-attribute)
      (othercase . othercase-attribute))))

;; Opposite of dump-string-char.
(defmacro load-string-char ()
  (ecase (c::backend-byte-order c::*native-backend*)
    (:little-endian
     `(code-char (+ (read-arg 1)
		    (ash (read-arg 1) 8))))
    (:big-endian
     `(code-char (+ (ash (read-arg 1) 8)
		    (read-arg 1))))))
  
;; Needed to read in 16-bit strings.
(clone-fop (fop-string 37)
	   (fop-small-string 38)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (dotimes (k arg)
      (setf (aref res k) (load-string-char)))
    res))

;; Read in characters.  They're always dumped in little-endian order.
(define-fop (fop-short-character 69)
  (code-char (+ (read-arg 1)
		(ash (read-arg 1) 8))))

;; Needed to read in 16-bit strings to create the symbols.
(macrolet ((frob (name code name-size package)
	     (let ((n-package (gensym "PACKAGE-"))
		   (n-size (gensym "SIZE-"))
		   (n-buffer (gensym "BUFFER-"))
		   (k (gensym "IDX-")))
	       `(define-fop (,name ,code)
		  (prepare-for-fast-read-byte *fasl-file*
		    (let ((,n-package ,package)
			  (,n-size (fast-read-u-integer ,name-size)))
		      (when (> ,n-size *load-symbol-buffer-size*)
			(setq *load-symbol-buffer*
			      (make-string (setq *load-symbol-buffer-size*
						 (* ,n-size 2)))))
		      (done-with-fast-read-byte)
		      (let ((,n-buffer *load-symbol-buffer*))
			(dotimes (,k ,n-size)
			  (setf (aref ,n-buffer ,k) (load-string-char)))
			(push-table (intern* ,n-buffer ,n-size ,n-package)))))))))
  (frob fop-symbol-save 6 4 *package*)
  (frob fop-small-symbol-save 7 1 *package*)
  (frob fop-lisp-symbol-save 75 4 *lisp-package*)
  (frob fop-lisp-small-symbol-save 76 1 *lisp-package*)
  (frob fop-keyword-symbol-save 77 4 *keyword-package*)
  (frob fop-keyword-small-symbol-save 78 1 *keyword-package*)

  (frob fop-symbol-in-package-save 8 4
    (svref *current-fop-table* (fast-read-u-integer 4)))
  (frob fop-small-symbol-in-package-save 9 1
    (svref *current-fop-table* (fast-read-u-integer 4)))
  (frob fop-symbol-in-byte-package-save 10 4
    (svref *current-fop-table* (fast-read-u-integer 1)))
  (frob fop-small-symbol-in-byte-package-save 11 1
    (svref *current-fop-table* (fast-read-u-integer 1))))

(clone-fop (fop-uninterned-symbol-save 12)
	   (fop-uninterned-small-symbol-save 13)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (dotimes (k arg)
      (setf (aref res k) (load-string-char)))
    (push-table (make-symbol res))))

(define-fop (fop-foreign-fixup 147)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (dotimes (k len)
      (setf (aref sym k) (load-string-char)))
    (old-vm:fixup-code-object code-object (read-arg 4)
			      (foreign-symbol-address-aux sym :code)
			      kind)
    code-object))

(define-fop (fop-foreign-data-fixup 150)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (dotimes (k len)
      (setf (aref sym k) (load-string-char)))
    (old-vm:fixup-code-object code-object (read-arg 4)
			      (foreign-symbol-address-aux sym :data)
			      kind)
    code-object))

;; Kill the any deftransforms.  They get in the way because they
;; assume 8-bit strings.
(in-package "C")
(dolist (f '(concatenate subseq replace copy-seq))
  (setf (c::function-info-transforms (c::function-info-or-lose f)) nil))

(in-package "C-CALL")

(def-alien-type-method (c-string :deport-gen) (type value)
  (declare (ignore type))
  (let ((s (gensym "C-STRING-"))
	(len (gensym "LEN-"))
	(k (gensym "IDX-")))
    `(etypecase ,value
       (null (int-sap 0))
       ((alien (* char)) (alien-sap ,value))
       (simple-base-string
	(let* ((,len (length ,value))
	       (,s (make-array (1+ ,len) :element-type '(unsigned-byte 8))))
	  (dotimes (,k ,len)
	    (setf (aref ,s ,k) (logand #xff (char-code (aref ,value ,k)))))
	  (setf (aref ,s ,len) 0)
	  (vector-sap ,s))))))

;;; Might as well update the FASL file version too, since we have to
;;; do a cross-compile anyway, and the Unicode fasl's aren't even
;;; close to being compatible with previous versions.

(in-package :c)

(setf lisp::*enable-package-locked-errors* nil)

;;;
;;; Note that BYTE-FASL-FILE-VERSION is a constant.
;;;
;;; (Be sure to change BYTE-FASL-FILE-VERSION in
;;; compiler/byte-comp.lisp to the correct value too!)
;;;
(setf (symbol-value 'byte-fasl-file-version)       #x20a)
(setf (backend-fasl-file-version *target-backend*) #x20a)

;;;
;;; Don't check fasl versions in the compiling Lisp because we'll
;;; load files compiled with the new version numbers.
;;;
(setq lisp::*skip-fasl-file-version-check* t)

;;;
;;; This is here because BYTE-FASL-FILE-VERSION is constant-folded in
;;; OPEN-FASL-FILE.  To make the new version number take effect, we
;;; have to redefine the function.
;;;
(defun open-fasl-file (name where &optional byte-p)
  (declare (type pathname name))
  (let* ((stream (open name :direction :output
		       :if-exists :new-version
		       :element-type '(unsigned-byte 8)))
	 (res (make-fasl-file :stream stream)))
    (multiple-value-bind
	(version f-vers f-imp)
	(if byte-p
	    (values "Byte code"
		    byte-fasl-file-version
		    (backend-byte-fasl-file-implementation *backend*))
	    (values (backend-version *backend*)
		    (backend-fasl-file-version *backend*)
		    (backend-fasl-file-implementation *backend*)))
      (format stream
	      "FASL FILE output from ~A.~@
	       Compiled ~A on ~A~@
	       Compiler ~A, Lisp ~A~@
	       Targeted for ~A, FASL version ~X~%"
	      where
	      (ext:format-universal-time nil (get-universal-time))
	      (machine-instance) compiler-version
	      (lisp-implementation-version)
	      version f-vers)
      ;;
      ;; Terminate header.
      (dump-byte 255 res)
      ;;
      ;; Specify code format.
      (dump-fop 'lisp::fop-long-code-format res)
      (dump-byte f-imp res)
      (dump-unsigned-32 f-vers res))
    res))
