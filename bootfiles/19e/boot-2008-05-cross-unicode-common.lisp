;;; Common part for cross-compiling 16-bit strings for Unicode.
;;; This part is independent of the architecture.

(pushnew :unicode *features*)
(in-package "C")

;; Write a char-code to a fasl file in the correct order
(defun dump-char-code (code file)
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

;; Not sure why dump-bytes doesn't work. So we explicitly dump the
;; bytes ourselves.
(defun dump-simple-string (s file)
  (declare (type simple-base-string s))
  (let ((length (length s)))
    (dump-fop* length lisp::fop-small-string lisp::fop-string file)
    (dotimes (k length)
      (dump-char-code (char-code (aref s k)) file)))
  (undefined-value))

;; Like dump-simple-string, but dump the characters explicitly.
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
      (dump-char-code (char-code (aref pname k)) file))

    (unless *cold-load-dump*
      (setf (gethash s (fasl-file-eq-table file)) (fasl-file-table-free file)))

    (incf (fasl-file-table-free file)))

  (undefined-value))

(defun dump-character (ch file)
  (dump-fop 'lisp::fop-short-character file)
  (dump-char-code (char-code ch) file))

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
	     (dump-char-code (char-code (schar name i)) file))))
	(:code-object
	 (dump-fop 'lisp::fop-code-object-fixup file)))
      (dump-unsigned-32 offset file)))
  (undefined-value))

(in-package "LISP")

;; Opposite of dump-char-code
(defmacro load-char-code ()
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
      (setf (aref res k) (load-char-code)))
    res))

(define-fop (fop-short-character 69)
  (load-char-code))

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
			  (setf (aref ,n-buffer ,k) (load-char-code)))
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
      (setf (aref res k) (load-char-code)))
    (push-table (make-symbol res))))

(define-fop (fop-foreign-fixup 147)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (dotimes (k len)
      (setf (aref sym k) (load-char-code)))
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
      (setf (aref sym k) (load-char-code)))
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

