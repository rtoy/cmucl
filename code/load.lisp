;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Loader for Spice Lisp.
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package "LISP")
(export '(load *load-verbose*))

(in-package "SYSTEM")
(export 'resolve-loaded-assembler-references)

(in-package "EXTENSIONS")
(export '*load-if-source-newer*)

(in-package "LISP")


;;;; Random state variables:

(defvar *load-verbose* ()
  "The default for the :Verbose argument to Load.")
(defvar *load-print-stuff* ()
  "True if we're gonna mumble about what we're loading.")
(defvar *fasl-file* () "The fasl file we're reading from.")
(defvar *current-code-format* "The code format that we think we are loading.")

(defvar *in-cold-load* nil)	; True if we are in the cold loader.

(defvar *load-if-source-newer* :load-object
  "The value of *load-if-source-newer* determines what happens when the
  source file is newer than the object file.  The possible values are:
  :load-object - load object file (default), :load-source - load the source
  file, :compile - compile the source and then load the object file, or
  :query - ask the user if he wants to load the source or object file.")

(proclaim '(special cold-fop-functions))

;;;; The Fop-Table:
;;;
;;;    The table is implemented as a simple-vector indexed by the table
;;; offset.  We may need to have several, since load can be called recursively.

(defvar *free-fop-tables* (list (make-array 1000))
  "List of free fop tables for the fasloader.")

(defvar *current-fop-table* ()
  "The current fop table.")

(defvar *current-fop-table-size* ()
  "The length of the current fop table.")

(defvar *current-fop-table-index* ()
  "Index in the fop-table of the next entry to be used.")

(defun grow-fop-table ()
  (let* ((new-size (* *current-fop-table-size* 2))
	 (new-table (make-array new-size)))
    (declare (fixnum new-size) (simple-vector new-table))
    (replace new-table (the simple-vector *current-fop-table*))
    (setq *current-fop-table* new-table)
    (setq *current-fop-table-size* new-size)))

(defmacro push-table (thing)
  (let ((n-index (gensym)))
    `(let ((,n-index *current-fop-table-index*))
       (declare (fixnum ,n-index))
       (when (= ,n-index (the fixnum *current-fop-table-size*))
	 (grow-fop-table))
       (setq *current-fop-table-index* (1+ ,n-index))
       (setf (svref *current-fop-table* ,n-index) ,thing))))

;;;; The Fop-Stack:
;;;
;;;  The is also in a simple-vector, but it grows down, since it is somewhat 
;;; cheaper to test for overflow that way.
;;;
(defvar *fop-stack* (make-array 100)
  "The fop stack (we only need one!).")

(defvar *fop-stack-pointer* 100
  "The index of the most recently pushed item on the fop-stack.")

(defvar *fop-stack-pointer-on-entry* ()
  "The current index into the fop stack when we last recursively entered LOAD.")


(defun grow-fop-stack ()
  (let* ((size (length (the simple-vector *fop-stack*)))
	 (new-size (* size 2))
	 (new-stack (make-array new-size)))
    (declare (fixnum size new-size) (simple-vector new-stack))
    (replace new-stack (the simple-vector *fop-stack*) :start1 size)
    (incf *fop-stack-pointer-on-entry* size)
    (setq *fop-stack-pointer* size)
    (setq *fop-stack* new-stack)))

;;; With-Fop-Stack  --  Internal
;;;
;;;    Cache information about the fop-stack in local variables.  Define
;;; a local macro to pop from the stack.  Push the result of evaluation if
;;; specified.
;;;
(defmacro with-fop-stack (pushp &body forms)
  (let ((n-stack (gensym))
	(n-index (gensym))
	(n-res (gensym)))
    `(let ((,n-stack *fop-stack*)
	   (,n-index *fop-stack-pointer*))
       (declare (simple-vector ,n-stack) (fixnum ,n-index))
       (macrolet ((pop-stack ()
		    `(prog1
		      (svref ,',n-stack ,',n-index)
		      (setq ,',n-index (1+ ,',n-index))))
		  (call-with-popped-things (fun n)
		    (let ((n-start (gensym)))
		      `(let ((,n-start (+ ,',n-index ,n)))
			 (setq ,',n-index ,n-start)
			 (,fun ,@(make-list n :initial-element
					    `(svref ,',n-stack
						    (decf ,n-start))))))))
	 ,(if pushp
	      `(let ((,n-res (progn ,@forms)))
		 (when (zerop ,n-index)
		   (grow-fop-stack)
		   (setq ,n-index *fop-stack-pointer*
			 ,n-stack *fop-stack*))
		 (decf ,n-index)
		 (setq *fop-stack-pointer* ,n-index)
		 (setf (svref ,n-stack ,n-index) ,n-res))
	      `(prog1
		(progn ,@forms)
		(setq *fop-stack-pointer* ,n-index)))))))

;;; FOP database:

(defvar fop-codes (make-array 256)
  "Vector indexed by a FaslOP that yields the FOP's name.")

(defvar fop-functions
  (make-array 256 :initial-element #'(lambda () (error "Losing FOP!")))
  "Vector indexed by a FaslOP that yields a function of 0 arguments which
  will perform the operation.")


;;; Define-FOP  --  Internal
;;;
;;;    Defines Name as a fasl operation, with op-code op.  If pushp is :nope,
;;; the the body neither pushes or pops the fop stack.  If it is nil, then
;;; the body may pop, but the result is ignored.  If it is true, the the result
;;; is pushed on the stack.
;;;
(defmacro define-fop ((name op &optional (pushp t)) &rest forms)
  `(progn
    (defun ,name ()
      ,(if (eq pushp :nope)
	   `(progn ,@forms)
	   `(with-fop-stack ,pushp ,@forms)))
    (setf (svref fop-codes ,op) ',name)
    (setf (get ',name 'fop-code) ,op)
    (setf (svref fop-functions ,op) #',name)))

;;; Clone-Fop  --  Internal
;;;
;;;    Defines a pair of fops which are identical except in that one reads
;;; a four byte argument and the other reads a one byte argument.  The
;;; argument can be accessed by using the Clone-Arg macro.
;;;
(defmacro clone-fop ((name op &optional (pushp t))
		      (small-name small-op) &rest forms)
  `(progn
    (macrolet ((clone-arg () '(read-arg 4)))
      (define-fop (,name ,op ,pushp) ,@forms))
    (macrolet ((clone-arg () '(read-arg 1)))
      (define-fop (,small-name ,small-op ,pushp) ,@forms))))

;;;; Utilities for reading from the fasl file.

(proclaim '(inline read-byte))

;;; Fast-Read-U-Integer  --  Internal
;;;
;;;    Expands into code to read an N-byte unsigned integer using
;;; fast-read-byte.
;;;
(defmacro fast-read-u-integer (n)
  (do ((res '(fast-read-byte)
	    `(logior (fast-read-byte)
		     (ash ,res 8)))
       (cnt 1 (1+ cnt)))
      ((>= cnt n) res)))

;;; Fast-Read-Variable-U-Integer  --  Internal
;;;
;;;    Like Fast-Read-U-Integer, but the size may be determined at run time.
;;;
(defmacro fast-read-variable-u-integer (n)
  (let ((n-pos (gensym))
	(n-res (gensym))
	(n-cnt (gensym)))
    `(do ((,n-pos 8 (+ ,n-pos 8))
	  (,n-cnt (1- ,n) (1- ,n-cnt))
	  (,n-res
	   (fast-read-byte)
	   (dpb (fast-read-byte) (byte 8 ,n-pos) ,n-res)))
	 ((zerop ,n-cnt) ,n-res))))

;;; Fast-Read-S-Integer  --  Internal
;;;
;;;    Read a signed integer.
;;;
(defmacro fast-read-s-integer (n)
  (let ((n-last (gensym)))
    (do ((res `(let ((,n-last (fast-read-byte)))
		 (if (zerop (logand ,n-last #x80))
		     ,n-last
		     (logior ,n-last #x-100)))
	      `(logior (fast-read-byte)
		       (ash ,res 8)))
	 (cnt 1 (1+ cnt)))
	((>= cnt n) res))))

;;; Read-Arg  --  Internal
;;;
;;;    Read an N-byte unsigned integer from the *fasl-file*
;;;
(defmacro read-arg (n)
  (if (= n 1)
      `(read-byte *fasl-file*)
      `(prepare-for-fast-read-byte *fasl-file*
	 (prog1
	  (fast-read-u-integer ,n)
	  (done-with-fast-read-byte)))))

;;; Fasload:

(defun fasload (stream)
  (when *load-verbose*
    (format t "~&; Loading stuff from ~S.~%" stream))
  (let* ((*fasl-file* stream)
	 (*current-fop-table* (pop *free-fop-tables*))
	 (*current-fop-table-size* ())
	 (*current-fop-table-index* 0)
	 (*fop-stack-pointer-on-entry* *fop-stack-pointer*))
    (if (null *current-fop-table*)
	(setq *current-fop-table* (make-array 1000)))
    (setq *current-fop-table-size*
	  (length (the simple-vector *current-fop-table*)))
    (unwind-protect 
      (do ((loaded-group (load-group stream) (load-group stream)))
	  ((not loaded-group)))
      (setq *fop-stack-pointer* *fop-stack-pointer-on-entry*)
      ;;
      ;; Nil out the table, so we don't hold onto garbage.
      (let ((tab *current-fop-table*))
	(dotimes (i *current-fop-table-index*)
	  (declare (fixnum i))
	  (setf (svref tab i) nil))
	(push tab *free-fop-tables*))
      ;;
      ;; Ditto for the stack...
      (dotimes (i *fop-stack-pointer-on-entry*)
	(declare (fixnum i))
	(setf (svref *fop-stack* i) nil))))
  t)

#|

(defvar *fop-counts* (make-array 256 :initial-element 0))
(defvar *fop-times* (make-array 256 :initial-element 0))
(defvar *print-fops* nil)

(defun clear-counts ()
  (fill (the simple-vector *fop-counts*) 0)
  (fill (the simple-vector *fop-times*) 0)
  t)

(defun analyze-counts ()
  (let ((counts ())
	(total-count 0)
	(times ())
	(total-time 0))
    (macrolet ((breakdown (lvar tvar vec)
		 `(progn
		   (dotimes (i 255)
		     (declare (fixnum i))
		     (let ((n (svref ,vec i)))
		       (push (cons (svref fop-codes i) n) ,lvar)
		       (incf ,tvar n)))
		   (setq ,lvar (subseq (sort ,lvar #'(lambda (x y)
						       (> (cdr x) (cdr y))))
				       0 10)))))
		 
      (breakdown counts total-count *fop-counts*)
      (breakdown times total-time *fop-times*)
      (format t "Total fop count is ~D~%" total-count)
      (dolist (c counts)
	(format t "~30S: ~4D~%" (car c) (cdr c)))
      (format t "~%Total fop time is ~D~%" (/ (float total-time) 60.0))
      (dolist (m times)
	(format t "~30S: ~6,2F~%" (car m) (/ (float (cdr m)) 60.0))))))
|#

;;; Load-Group  --  Internal
;;;
;;; Load-Group returns t if it successfully loads a group from the file,
;;; or () if EOF was encountered while trying to read from the file.
;;; Dispatch to the right function for each fop.  Special-case fop-byte-push
;;; since it is real common.
;;;
(defun load-group (file)
  (when (check-header file)
    (catch 'group-end
      (let ((*current-code-format* 'uninitialized-code-format))
	(loop
	 (let ((byte (read-byte file)))
	   (if (eql byte 3)
	       (let ((index *fop-stack-pointer*))
		 (when (zerop index)
		   (grow-fop-stack)
		   (setq index *fop-stack-pointer*))
		 (decf index)
		 (setq *fop-stack-pointer* index)
		 (setf (svref *fop-stack* index)
		       (svref *current-fop-table* (read-byte file))))
	       (funcall (svref fop-functions byte)))))))))

;;; Check-Header returns t if t succesfully read a header from the file,
;;; or () if EOF was hit before anything was read.  An error is signaled
;;; if garbage is encountered.

(defun check-header (file)
  (let ((byte (read-byte file NIL '*eof*)))
    (cond ((eq byte '*eof*) ())
	  ((eq byte (char-int #\F))
	   (do ((byte (read-byte file) (read-byte file))
		(count 1 (1+ count)))
	       ((= byte 255) t)
	     (declare (fixnum byte))
	     (if (and (< count 9)
		      (not (eql byte (char-int (schar "FASL FILE" count)))))
		 (error "Bad FASL file format."))))
	  (t (error "Bad FASL file format.")))))


;;; Load-S-Integer loads a signed integer Length bytes long from the File.

(defun load-s-integer (length)  
  (declare (fixnum length))
  (do* ((index length (1- index))
	(byte 0 (read-byte *fasl-file*))
	(result 0 (+ result (ash byte bits)))
	(bits 0 (+ bits 8)))
       ((= index 0)
	(if (logbitp 7 byte)	; look at sign bit
	    (- result (ash 1 bits))
	    result))
    (declare (fixnum index byte bits))))

;;; Sloload:

;;; Something not EQ to anything read from a file:

(defconstant load-eof-value '(()))

;;; Sloload loads a text file into the given Load-Package.

(defun sloload (stream)
  (when *load-verbose*
    (format t "~&; Loading stuff from ~S.~%" stream))
  (do ((sexpr (read stream nil load-eof-value)
	      (read stream nil load-eof-value)))
      ((eq sexpr load-eof-value))
    (if *load-print-stuff*
	(format t "~&; ~S~%" (eval sexpr))
	(eval sexpr))))))

;;; Load:

(defun load (filename &key ((:verbose *load-verbose*) *load-verbose*)
		      ((:print *load-print-stuff*) *load-print-stuff*)
		      (if-does-not-exist :error))
  "Loads the file named by Filename into the Lisp environment.  See manual
   for details."
  (let ((*package* *package*))
    (if (streamp filename)
	(if (equal (stream-element-type filename) '(unsigned-byte 8))
	    (fasload filename)
	    (sloload filename))
	(let* ((pn (merge-pathnames (pathname filename)
				    *default-pathname-defaults*))
	       (tn (probe-file pn)))
	  (cond
	   (tn
	    (if (string-equal (pathname-type tn) "nfasl")
		(with-open-file (file tn
				      :direction :input
				      :element-type '(unsigned-byte 8))
		  (fasload file))
		(with-open-file (file tn :direction :input)
		  (sloload file)))
	    t)
	   ((pathname-type pn)
	    (let ((stream (open pn :direction :input
				:if-does-not-exist if-does-not-exist)))
	      (when stream
		(sloload stream)
		(close stream)
		t)))
	   (t
	    (let* ((srcn (make-pathname :type "lisp" :defaults pn))
		   (src (probe-file srcn))
		   (objn (make-pathname :type "nfasl" :defaults pn))
		   (obj (probe-file objn)))
	      (cond
	       (obj
		(cond ((and src (> (file-write-date src)
				   (file-write-date obj)))
		       (case *load-if-source-newer*
			 (:load-object
			  (warn "Loading object file ~A, which is~%  ~
			         older than the presumed source, ~A."
				(namestring obj)
				(namestring src))
			  (load obj))
			 (:load-source
			  (warn "Loading source file ~A, which is~%  ~
			         newer than the presumed object file, ~A."
				(namestring src)
				(namestring obj))
			  (load src))
			 (:compile
			  (compile-file (namestring src))
			  (load obj))
			 (:query
			  (if (y-or-n-p "Load source file ~A which is newer~%  ~
					 than presumed object file ~A? "
					(namestring src)
					(namestring obj))
			      (load src)
			      (load obj)))
			 (T (error "*Load-if-source-newer* contains ~A which is not one of:~%  ~
			            :load-object, :load-source, :compile, or :query."
				   *load-if-source-newer*))))
		      (T (load obj))))
	       (t
		(load srcn :if-does-not-exist if-does-not-exist))))))))))


;;;; Actual FOP definitions:

(define-fop (fop-nop 0 :nope))
(define-fop (fop-pop 1 nil) (push-table (pop-stack)))
(define-fop (fop-pop-for-effect 65 nil) (pop-stack))
(define-fop (fop-push 2) (svref *current-fop-table* (read-arg 4)))
(define-fop (fop-byte-push 3) (svref *current-fop-table* (read-arg 1)))

(define-fop (fop-empty-list 4) ())
(define-fop (fop-truth 5) t)
(define-fop (fop-misc-trap 66)
	    (%primitive make-immediate-type 0 lisp::%trap-type))

(define-fop (fop-character 68)
  (int-char (read-arg 3)))
(define-fop (fop-short-character 69)
  (code-char (read-arg 1)))

(define-fop (fop-structure 79)
  (%primitive set-vector-subtype (pop-stack) 1))

(define-fop (fop-end-group 64 :nope) (throw 'group-end t))
(define-fop (fop-end-header 255)
  (error "Fop-End-Header was executed???"))

(define-fop (fop-normal-load 81 :nope))
(define-fop (fop-maybe-cold-load 82 :nope)
  (when *in-cold-load*
    (setq fop-functions cold-fop-functions)))

(define-fop (fop-static-heap 60 :nope))
(define-fop (fop-dynamic-heap 61 :nope))
(define-fop (fop-read-only-heap 67 :nope))

(define-fop (fop-verify-table-size 62 :nope)
  (if (/= *current-fop-table-index* (read-arg 4))
      (error "Fasl table of improper size.  Bug!")))
(define-fop (fop-verify-empty-stack 63 :nope)
  (if (/= *fop-stack-pointer* *fop-stack-pointer-on-entry*)
      (error "Fasl stack not empty.  Bug!")))

;;;; Loading symbols:

(defvar *load-symbol-buffer* (make-string 100))
(defvar *load-symbol-buffer-size* 100)
   
(macrolet ((frob (name code name-size package)
	     (let ((n-package (gensym))
		   (n-size (gensym))
		   (n-buffer (gensym)))
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
			(read-n-bytes *fasl-file* ,n-buffer 0 ,n-size)
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
    (read-n-bytes *fasl-file* res 0 arg)
    (push-table (make-symbol res))))

(define-fop (fop-package 14)
  (let ((name (pop-stack)))
    (or (find-package name)
	(error "The package ~S does not exist." name))))

;;;; Loading numbers:

(clone-fop (fop-integer 33)
	   (fop-small-integer 34)
  (load-s-integer (clone-arg)))

(define-fop (fop-word-integer 35)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
     (fast-read-s-integer 4)
     (done-with-fast-read-byte))))
(define-fop (fop-byte-integer 36)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
     (fast-read-s-integer 1)
     (done-with-fast-read-byte))))

(define-fop (fop-ratio 70)
  (let ((den (pop-stack)))
    (%primitive make-ratio (pop-stack) den)))

(define-fop (fop-complex 71)
  (let ((im (pop-stack)))
    (%primitive make-complex (pop-stack) im)))

(define-fop (fop-float 45)
  (let* ((n (read-arg 1))
	 (exponent (load-s-integer (ceiling n 8)))
	 (m (read-arg 1))
	 (mantissa (load-s-integer (ceiling m 8)))
	 (number (cond ((or (> n 9) (> m 32))
			(coerce mantissa 'long-float))
		       ((> m 21)
			(coerce mantissa 'single-float))
		       (T (coerce mantissa 'short-float)))))
    (multiple-value-bind (f ex s) (decode-float number)
      (declare (ignore ex))
      (* s (scale-float f exponent)))))

;;;; Loading lists:

(define-fop (fop-list 15)
  (do ((res () (cons (pop-stack) res))
       (n (read-arg 1) (1- n)))
      ((zerop n) res)))

(define-fop (fop-list* 16)
  (do ((res (pop-stack) (cons (pop-stack) res))
       (n (read-arg 1) (1- n)))
      ((zerop n) res)))

(macrolet ((frob (name op fun n)
	     `(define-fop (,name ,op)
		(call-with-popped-things ,fun ,n))))

  (frob fop-list-1 17 list 1)
  (frob fop-list-2 18 list 2)
  (frob fop-list-3 19 list 3)
  (frob fop-list-4 20 list 4)
  (frob fop-list-5 21 list 5)
  (frob fop-list-6 22 list 6)
  (frob fop-list-7 23 list 7)
  (frob fop-list-8 24 list 8)

  (frob fop-list*-1 25 list* 2)
  (frob fop-list*-2 26 list* 3)
  (frob fop-list*-3 27 list* 4)
  (frob fop-list*-4 28 list* 5)
  (frob fop-list*-5 29 list* 6)
  (frob fop-list*-6 30 list* 7)
  (frob fop-list*-7 31 list* 8)
  (frob fop-list*-8 32 list* 9))


;;;; Loading arrays:
;;;

(clone-fop (fop-string 37)
	   (fop-small-string 38)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (read-n-bytes *fasl-file* res 0 arg)
    res))

(clone-fop (fop-vector 39)
	   (fop-small-vector 40)
  (let* ((size (clone-arg))
	 (res (make-array size)))
    (declare (fixnum size))
    (do ((n (1- size) (1- n)))
	((minusp n))
      (setf (svref res n) (pop-stack)))
    res))

(clone-fop (fop-uniform-vector 41)
	   (fop-small-uniform-vector 42)
  (make-array (clone-arg) :initial-element (pop-stack)))

(define-fop (fop-array 83)
  (let* ((rank (read-arg 4))
	 (vec (pop-stack))
	 (size (+ rank %array-first-dim-slot))
	 (length (length vec))
	 (res (%primitive alloc-array rank)))
    (declare (simple-array vec))
    (set-array-header res vec length length 0
		      (do ((i (1- size) (1- i))
			   (dimensions () (cons (pop-stack) dimensions)))
			  ((< i %array-first-dim-slot) dimensions))
		      nil)
    res))


;;; Fop-Int-Vector  --  Internal
;;;
;;;    Load an I-Vector using the Guy Steele memorial faslop.  If there
;;; is no space at the end of a group, then we can read it using
;;; Read-N-Bytes.  All vectors dumped by our compiler should be loadable
;;; in this way.  If the other cases don't work, we may never know...
;;;
(define-fop (fop-int-vector 43)
  (prepare-for-fast-read-byte *fasl-file*
    (let* ((n (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (count (fast-read-byte))
	   (res (make-array n :element-type `(unsigned-byte ,size))))
      (multiple-value-bind (ints-per-entry extra)
			   (truncate (* count 8) size)
	(cond ((and (zerop extra) (<= size 16))
	       (done-with-fast-read-byte)
	       (read-n-bytes *fasl-file* res 0
			     (* count (ceiling n ints-per-entry))))
	      ((= ints-per-entry 1)
	       (dotimes (i n)
		 (setf (aref res i) (fast-read-variable-u-integer count)))
	       (done-with-fast-read-byte))
	      (t
	       (let ((i 0))
		 (loop
		   (when (= i n) (return))
		   (let ((byte (fast-read-byte)))
		     (dotimes (j ints-per-entry)
		       (setf (aref res i) (ldb (byte size (* size j)) byte))
		       (incf i)
		       (when (= i n) (return))))))
	       (done-with-fast-read-byte))))
      res)))

(define-fop (fop-uniform-int-vector 44)
  (prepare-for-fast-read-byte *fasl-file*
    (let* ((n (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (value (fast-read-variable-u-integer (ceiling size 8))))
      (done-with-fast-read-byte)
      (make-array n :element-type `(unsigned-byte ,size)
		  :initial-element value))))

(define-fop (fop-alter 52 nil)
  (let ((index (read-arg 1))
	(newval (pop-stack))
	(object (pop-stack)))
    (declare (fixnum index))
    (typecase object
      (list (case index
	      (0 (rplaca object newval))
	      (1 (rplacd object newval))
	      (t (error "~S: Bad index for FaslOP Alter.  Bug!"))))
      (symbol (case index
		(0 (set object newval))
		(1 (setf (symbol-function object) newval))
		(2 (setf (symbol-plist object) newval))
		(t (error "~S: Bad index for FaslOP Alter.  Bug!"))))
      (array (setf (aref object index) newval))
      (t (error "~S: Bad object for FaslOP Alter.  Bug!")))))

(define-fop (fop-eval 53)
  (let ((result (eval (pop-stack))))
    (when *load-print-stuff*
      (format t "~&; ~S~%" result))
    result))

(define-fop (fop-eval-for-effect 54 nil)
  (let ((result (eval (pop-stack))))
    (when *load-print-stuff*
      (format t "~&; ~S~%" result))))

(define-fop (fop-funcall 55)
  (let ((arg (read-arg 1)))
    (if (zerop arg)
	(funcall (pop-stack))
	(do ((args () (cons (pop-stack) args))
	     (n arg (1- n)))
	    ((zerop n) (apply (pop-stack) args))))))

(define-fop (fop-funcall-for-effect 56 nil)
  (let ((arg (read-arg 1)))
    (if (zerop arg)
	(funcall (pop-stack))
	(do ((args () (cons (pop-stack) args))
	     (n arg (1- n)))
	    ((zerop n) (apply (pop-stack) args))))))

;;;; Fixing up circularities.
(define-fop (fop-rplaca 200 nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4))
	(val (pop-stack)))
    (setf (car (nthcdr idx obj)) val)))


(define-fop (fop-rplacd 201 nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4))
	(val (pop-stack)))
    (setf (cdr (nthcdr idx obj)) val)))

(define-fop (fop-svset 202 nil)
  (let* ((obi (read-arg 4))
	 (obj (svref *current-fop-table* obi))
	 (idx (read-arg 4))
	 (val (pop-stack)))
    (setf (svref obj idx) val)))

(define-fop (fop-nthcdr 203 t)
  (nthcdr (read-arg 4) (pop-stack)))

;;;; Loading functions:

(define-fop (fop-code-format 57 :nope)
  (setq *current-code-format* (read-arg 1)))


;;; Load-Code loads a code object.  NItems objects are popped off the stack for
;;; the boxed storage section, then Size bytes of code are read in.  This must
;;; be done WITHOUT-GCING, since GC only recognizes code object references that
;;; appear in a function object.  If a GC happened before we stored the code
;;; object, the code would disappear.
;;;
(defmacro load-code (nitems size)
  `(without-gcing
     (let ((box-num ,nitems)
	   (code-length ,size))
       (declare (fixnum box-num code-length))
       (let ((function (%primitive alloc-function box-num)))
	 (%primitive set-vector-subtype function %function-constants-subtype)
	 (do ((index (1- box-num) (1- index)))
	     ((minusp index))
	   (declare (fixnum index))
	   (%primitive header-set function index (pop-stack)))
	 (let ((code (%primitive alloc-code code-length)))
	   (read-n-bytes *fasl-file* code 0 code-length)
	   (%primitive header-set function %function-code-slot code))
	 (when *load-print-stuff*
	   (format t "~&; ~S~%" function))
	 function))))

(define-fop (fop-code 58)
  (if (eql *current-code-format* %fasl-code-format)
      (load-code (read-arg 4) (read-arg 4))
      (error "~A has an incompatible fasl file format.~@
               You must recompile the source code."
	     *fasl-file*)))


(define-fop (fop-small-code 59)
  (if (eql *current-code-format* %fasl-code-format)
      (load-code (read-arg 1) (read-arg 2))
      (error "~A has an incompatible fasl file format.~@
               You must recompile the source code."
	     *fasl-file*)))


;;; Now a NOOP except in cold load... 
(define-fop (fop-fset 74 nil)
  (pop-stack)
  (pop-stack))


;;; Modify a slot in a Constants object.
;;;
(clone-fop (fop-alter-code 140 nil) (fop-byte-alter-code 141)
  (let ((value (pop-stack))
	(code (pop-stack))
	(index (clone-arg)))
    (%primitive header-set code index value)))


;;; Kind of like Load-Code, except that we set the Code and Constants
;;; slots from the Constants object that is our first stack argument.  The
;;; subtype is set to the second stack argument.
;;;
(define-fop (fop-function-entry 142)
  (let* ((box-num (read-arg 1))
	 (function (%primitive alloc-function box-num)))
    ;;
    ;; Pop boxed things, storing them in the allocated entry object.
    (do ((index (1- box-num) (1- index)))
	((minusp index))
      (%primitive header-set function index (pop-stack)))
    ;;
    ;; Set the subtype of the entry object.
    (%primitive set-vector-subtype function (pop-stack))
    ;;
    ;; Set code and constants slots in the entry.
    (let* ((constants (pop-stack))
	   (code (%primitive header-ref constants %function-code-slot)))
      (%primitive header-set function %function-code-slot code)
      (%primitive header-set function %function-entry-constants-slot
		  constants))

    function))



(define-fop (fop-user-miscop-fixup 134)
  (let* ((miscop-name (pop-stack))
	 (function-object (pop-stack))
	 (code (%primitive header-ref function-object %function-code-slot))
	 (offset (read-arg 4))
	 (loaded-addr (get miscop-name '%loaded-address)))
    (unless loaded-addr
      (error "Miscop ~A is undefined." miscop-name))
    
    (let ((hi-addr (logior (ash clc::type-assembler-code
				clc::type-shift-16)
			   (logand (ash loaded-addr -16) #xFFFF))))
      (setf (aref code (+ offset 1)) (logand hi-addr #xFF))
      (setf (aref code (+ offset 2))
	    (logand (ash loaded-addr -8) #xFF))
      (setf (aref code (+ offset 3))
	    (logand loaded-addr #xFF)))

    function-object))


;;;; Loading assembler routines:
;;;

;;; Allocate-Assembler-Code  --  Internal
;;;
;;;    Allocate some stuff out of assembler code space.
;;;
(defun allocate-assembler-code (bytes)
  (let* ((idx (ash %assembler-code-type %alloc-ref-type-shift))
	 (free (alloc-ref idx))
	 (new (+ free bytes)))
    (prog1
      (%primitive make-immediate-type free %assembler-code-type)
      (%primitive 16bit-system-set alloctable-address idx (ash new -16))
      (%primitive 16bit-system-set alloctable-address (1+ idx)
		  (logand new #xFFFF)))))

(define-fop (fop-assembler-routine 130)
  (let* ((code-length (read-arg 4))
	 (buffer (make-array code-length :element-type '(unsigned-byte 8)))
	 (code (allocate-assembler-code code-length)))
    (declare (fixnum code-length))
    (read-n-bytes *fasl-file* buffer 0 code-length)
    (%primitive byte-blt buffer 0 code 0 code-length)
    code))

;;; A list of the miscop definitions which have been loaded but not
;;; resolved.  Each element is a cons (name . code-ptr).
;;;
(defvar *miscop-definitions* ())


;;; Recall that the format of a reference is (How Label Location),
;;; where How is one of JI, BI, BA, or L, Label is the label's name, and
;;; Location is the location of the reference.  These things are stored on
;;; the list *external-references* as (Name . References), where Name is
;;; the name of the referencing routine, and References is a list of references
;;; in the above format.
;;;
(defvar *external-references* ())
(defvar *user-defined-miscops* ())

(define-fop (fop-fixup-miscop-routine 131 nil)
  (let* ((external-references (pop-stack))
	 (external-labels (pop-stack))
	 (name (pop-stack))
	 (code (pop-stack))
	 (start (%primitive make-immediate-type code %+-fixnum-type)))
    (dolist (lab external-labels)
      (setf (get (car lab) '%loaded-address) (+ (ash (cdr lab) 1) start)))
    (push (cons name external-references) *external-references*)
    (push (cons name code) *miscop-definitions*)))

(define-fop (fop-fixup-user-miscop-routine 133 nil)
  (let* ((external-references (pop-stack))
	 (external-labels (pop-stack))
	 (name (pop-stack))
	 (code (pop-stack))
	 (start (%primitive make-immediate-type code %+-fixnum-type)))
    (dolist (lab external-labels)
      (setf (get (car lab) '%loaded-address) (+ (ash (cdr lab) 1) start)))
    (push (cons name external-references) *external-references*)
    (pushnew name *user-defined-miscops*)
    (setf (get name 'user-miscop) t)))

(define-fop (fop-fixup-assembler-routine 132 nil)
  (let* ((external-references (pop-stack))
	 (external-labels (pop-stack))
	 (name (pop-stack))
	 (code (pop-stack))
	 (start (%primitive make-immediate-type code %+-fixnum-type)))
    (dolist (lab external-labels)
      (setf (get (car lab) '%loaded-address) (+ (ash (cdr lab) 1) start)))
    (push (cons name external-references) *external-references*)))

;;; Resolving all the assembler routines' references.

;;; Patch-Instruction  --  Internal
;;;
;;;    Used to patch an assembler code object.  Hi-var and lo-var are
;;; bound to the values of the high and low halfwords in the instruction.
;;; The values may by changed by setting the variables.
;;;
(defmacro patch-instruction ((hi-var lo-var code offset) &body body)
  `(let ((,hi-var (%primitive 16bit-system-ref ,code ,offset))
	 (,lo-var (%primitive 16bit-system-ref ,code (1+ ,offset))))
     (multiple-value-prog1
      (progn ,@body)
      (%primitive 16bit-system-set ,code ,offset ,hi-var)
      (%primitive 16bit-system-set ,code (1+ ,offset) ,lo-var))))

;;; Resolve-Loaded-Assembler-References  --  Public
;;;
;;;    Fix up the recorded external references and define the miscops.
;;;
(defun resolve-loaded-assembler-references ()
  "This function resolves external label references in loaded assembler
  routines.  It should be called after assembler files have been loaded.
  Miscop definitions do not take effect until this function is called."
  (dolist (reflist *external-references*)
    (let* ((code-byte-offset (get (car reflist) '%loaded-address))
	   (code-halfword-offset (ash code-byte-offset -1))
	   (address (%primitive make-immediate-type code-byte-offset
				%assembler-code-type)))
      (dolist (refs (cdr reflist))
	(let ((how (car refs))
	      (label (get (cadr refs) '%loaded-address))
	      (location (caddr refs)))
	  (unless label
	    (error "~A references ~A, which has not been defined.~%"
		   (car reflist) (cadr refs)))
	  (let ((offset (- (- (ash label -1) code-halfword-offset) location)))
	    (ecase how
	      (clc::ji
	       (unless (<= #x-80 offset #x7F)
		 (error "Offset #X~X out of JI range for ~A to reference ~A.~%"
			offset (car reflist) (cadr refs)))
	       (patch-instruction (hi lo address location)
		 (setf (ldb (byte 8 0) hi) offset)))
	      (clc::bi
	       (unless (<= #x-80000 offset #x7FFFF)
		 (error "Offset #X~X out of BI range for ~A to reference ~A.~%"
			offset (car reflist) (cadr refs)))
	       (patch-instruction (hi lo address location)
		 (setf (ldb (byte 4 0) hi) (ash offset -16))
		 (setq lo (logand offset #xFFFF))))
	      (clc::ba (error "I can't resolve a BA reference yet.~%"))
	      (clc::l (error "I can't resolve an L reference yet.~%"))))))))
  (setq *external-references* ())

  (dolist (mo *miscop-definitions*)
    (let* ((name (intern (symbol-name (car mo)) (find-package "COMPILER")))
	   (index (get name 'clc::transfer-vector-index)))
      (if index
	  (%primitive write-control-stack
		      (%primitive make-immediate-type (ash index 2)
				  %assembler-code-type)
		      (cdr mo))
	  (pushnew name *user-defined-miscops*))))
  (setq *miscop-definitions* ()))

(proclaim '(notinline read-byte))
