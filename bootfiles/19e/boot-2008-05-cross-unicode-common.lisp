(pushnew :unicode *features*)
(in-package "C")

;; Not sure why dump-bytes doesn't work. So we explicitly dump the
;; bytes ourselves.
(defun dump-simple-string (s file)
  (declare (type simple-base-string s))
  (let ((length (length s)))
    (dump-fop* length lisp::fop-small-string lisp::fop-string file)
    #+nil
    (dump-bytes s (* 2 length) file)

    (dotimes (k length)
      (let ((code (char-code (aref s k))))
	(dump-byte (ldb (byte 8 8) code) file)
	(dump-byte (ldb (byte 8 0) code) file))))
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
      (let ((code (char-code (aref pname k))))
	(dump-byte (ldb (byte 8 8) code) file)
	(dump-byte (ldb (byte 8 0) code) file)))

    (unless *cold-load-dump*
      (setf (gethash s (fasl-file-eq-table file)) (fasl-file-table-free file)))

    (incf (fasl-file-table-free file)))

  (undefined-value))

(in-package "LISP")

;; Needed to read in 16-bit strings.
(clone-fop (fop-string 37)
	   (fop-small-string 38)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    #+nil
    (read-n-bytes *fasl-file* res 0 (* 2 arg))
    
    (dotimes (k arg)
      (let ((c-hi (read-arg 1))
	    (c-lo (read-arg 1)))
	(setf (aref res k) (code-char (+ c-lo
					 (ash c-hi 8))))))
    res))

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
			  (setf (aref ,n-buffer ,k)
				(code-char (+ (ash (read-arg 1) 8) (read-arg 1)))))
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
      (setf (aref res k)
	    (code-char (+ (ash (read-arg 1) 8) (read-arg 1)))))
    (push-table (make-symbol res))))

;; Kill the any deftransforms.  They get in the way because they
;; assume 8-bit strings.
(in-package "C")
(dolist (f '(concatenate subseq replace copy-seq))
  (setf (c::function-info-transforms (c::function-info-or-lose f)) nil))

(in-package "C-CALL")

;; For some reason (make-array n :element-type '(unsigned-byte 8))
;; returns a simple-vector when compiled in c-string deport-gen.  So
;; instead of that, call out to make-array-unsigned-byte-8 which seems
;; to do the right thing.
(defun make-array-unsigned-byte-8 (n)
  (declare (fixnum n)
	   (optimize (speed 3) (safety 0)))
  (make-array n :element-type '(unsigned-byte 8)))

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
	       (,s (make-array-unsigned-byte-8 (1+ ,len))))
	  #+nil
	  (progn
	    (lisp::%primitive lisp::print "deport string")
	    (lisp::%primitive lisp::print ,value)
	    (lisp::%primitive lisp::print ,s))
	  (dotimes (,k ,len)
	    (setf (aref ,s ,k) (logand #xff (char-code (aref ,value ,k)))))
	  (setf (aref ,s ,len) 0)
	  #+nil
	  (lisp::%primitive lisp::print ,s)
	  (vector-sap ,s))))))
