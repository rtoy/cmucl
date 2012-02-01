;; Common parts for cross-compiling from a little-ending machine to a
;; big-endian machine like sparc or ppc.  Basically, we need to adjust
;; the fops that deal with strings (like symbols and strings).  The
;; strings in the fasls are written in the target byte order, but the
;; compiling system (little-endian) needs to be able to read them back
;; in correctly to create kernel.core.

(in-package "VM")
;; Define char-bytes.  Don't know why this isn't defined for the new
;; backend on sparc and ppc.
(defconstant char-bytes #+unicode 2 #-unicode 1)
(export 'char-bytes)
(in-package "CL-USER")

(in-package "LISP")
;; We need the the fops if the cross-compiled fasl file is in
;; big-endian order.  When we read in a string, we need to
;; convert the big-endian string to little-endian for x86 so we can
;; process the symbols and such as expected.
#+unicode
(progn
(defun maybe-swap-string (f name &optional (len (length name)))
  (declare (ignorable f))
  (unless (eq (c:backend-byte-order c:*backend*)
	      (c:backend-byte-order c:*native-backend*))
    (dotimes (k len)
      (let ((code (char-code (aref name k))))
	(setf (aref name k)
	      (code-char (logior (ash (ldb (byte 8 0) code) 8)
				 (ldb (byte 8 8) code))))))
    ;;(format t "~S: new name = ~S~%" f (subseq name 0 len))
    name))

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
						 (* ,n-size vm:char-bytes)))))
		      (done-with-fast-read-byte)
		      (let ((,n-buffer *load-symbol-buffer*))
			(read-n-bytes *fasl-file* ,n-buffer 0
				      (* old-vm:char-bytes ,n-size))
			(maybe-swap-string ',name ,n-buffer ,n-size)
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

(define-fop (fop-package 14)
  (let ((name (pop-stack)))
    ;;(format t "xfop-package: ~{~X~^ ~}~%" (map 'list #'char-code name))
    (or (find-package name)
	(error (intl:gettext "The package ~S does not exist.") name))))

(clone-fop (fop-string 37)
	   (fop-small-string 38)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (read-n-bytes *fasl-file* res 0
		  (* old-vm:char-bytes arg))
    (maybe-swap-string 'fop-string res)
    res))

#+unicode
(defun cold-load-symbol (size package)
  (let ((string (make-string size)))
    (read-n-bytes *fasl-file* string 0 (* 2 size))
    ;;(format t "xpre swap cold-load-symbol: ~S to package ~S~%" string package)
    (maybe-swap-string 'cold-load-symbol string)
    ;;(format t "xpost swap cold-load-symbol: ~S to package ~S~%" string package)
    (cold-intern (intern string package) package)))
)

