;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/load.lisp,v 1.29 1991/04/06 12:10:07 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/load.lisp,v 1.29 1991/04/06 12:10:07 wlott Exp $
;;;
;;; Loader for Spice Lisp.
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package "LISP")
(export '(load *load-verbose*))

(in-package "EXTENSIONS")
(export '*load-if-source-newer*)

(in-package "LISP")


;;;; Random state variables:

(defvar *load-verbose* ()
  "The default for the :Verbose argument to Load.")
(defvar *load-print-stuff* ()
  "True if we're gonna mumble about what we're loading.")
(defvar *load-depth* 0
  "Count of the number of recursive loads.")
(defvar *fasl-file* ()
  "The fasl file we're reading from.")
(defvar *current-code-format*
  "The code format that we think we are loading.")

(defvar *load-if-source-newer* :load-object
  "The value of *load-if-source-newer* determines what happens when the
  source file is newer than the object file.  The possible values are:
  :load-object - load object file (default), :load-source - load the source
  file, :compile - compile the source and then load the object file, or
  :query - ask the user if he wants to load the source or object file.")

;;; LOAD-FRESH-LINE -- internal.
;;;
;;; Output the corrent number of semicolons after a fresh-line.
;;; 
(defconstant semicolons ";;;;;;;;;;;;;;;;")
;;;
(defun load-fresh-line ()
  (fresh-line)
  (do ((count *load-depth* (- count (length semicolons))))
      ((< count (length semicolons))
       (unless (zerop count)
	 (write-string semicolons *standard-output* :end count)))
    (write-string semicolons))
  (write-char #\space))


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
  (unless (listen stream)
    (error "Attempt to load an empty FASL FILE:~%  ~S" stream))
  (when *load-verbose*
    (load-fresh-line)
    (format t "Loading stuff from ~S.~%" stream))
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
      (let ((*current-code-format* nil))
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
	  ((eq byte (char-code #\F))
	   (do ((byte (read-byte file) (read-byte file))
		(count 1 (1+ count)))
	       ((= byte 255) t)
	     (declare (fixnum byte))
	     (if (and (< count 9)
		      (not (eql byte (char-code (schar "FASL FILE" count)))))
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
    (load-fresh-line)
    (format t "Loading stuff from ~S.~%" stream))
  (do ((sexpr (read stream nil load-eof-value)
	      (read stream nil load-eof-value)))
      ((eq sexpr load-eof-value))
    (if *load-print-stuff*
	(let ((results (multiple-value-list (eval sexpr))))
	  (load-fresh-line)
	  (format t "~{~S~^, ~}~%" results))
	(eval sexpr)))
  t)

;;; Load:

(defun load (filename &key ((:verbose *load-verbose*) *load-verbose*)
		      ((:print *load-print-stuff*) *load-print-stuff*)
		      (if-does-not-exist :error) contents)
  "Loads the file named by Filename into the Lisp environment.  See manual
   for details."
  (declare (type (or null (member :source :binary)) contents))
  (let ((*package* *package*)
	(*load-depth* (1+ *load-depth*)))
    (if (streamp filename)
	(if (or (eq contents :binary)
		(and (null contents)
		     (equal (stream-element-type filename)
			    '(unsigned-byte 8))))
	    (fasload filename)
	    (sloload filename))
	(let ((pn (merge-pathnames (pathname filename)
				   *default-pathname-defaults*)))
	  (internal-load pn (probe-file pn) if-does-not-exist contents)))))

(defun internal-load (pathname truename if-does-not-exist contents)
  (cond
   (truename
    (case contents
      (:source
       (with-open-file (file truename :direction :input)
	 (sloload file)))
      (:binary
       (with-open-file (file truename
			     :direction :input
			     :element-type '(unsigned-byte 8))
	 (fasload file)))
      (t
       (let ((first-line (with-open-file (file truename :direction :input)
			   (read-line file nil))))
	 (if (and first-line
		  (>= (length first-line) 9)
		  (string= first-line "FASL FILE" :end1 9))
	     (internal-load pathname truename if-does-not-exist :binary)
	     (internal-load pathname truename if-does-not-exist :source))))))
   ((pathname-type pathname)
    (with-open-file (stream pathname :direction :input
			    :if-does-not-exist if-does-not-exist)
      (sloload stream)))
   (t
    (let* ((srcn (make-pathname :type "lisp" :defaults pathname))
	   (src (probe-file srcn))
	   (obj (or (probe-file (make-pathname
				 :type #.(c:backend-fasl-file-type c:*backend*)
				 :defaults pathname))
		    (probe-file (make-pathname :type "fasl"
					       :defaults pathname)))))
      (cond
       (obj
	(cond
	 ((and src (> (file-write-date src) (file-write-date obj)))
	  (case *load-if-source-newer*
	    (:load-object
	     (warn "Loading object file ~A, which is~%  ~
	            older than the presumed source:~% ~A."
		   (namestring obj)
		   (namestring src))
	     (internal-load obj obj if-does-not-exist :binary))
	    (:load-source
	     (warn "Loading source file ~A, which is~%  ~
	            newer than the presumed object file, ~A."
		   (namestring src)
		   (namestring obj))
	     (internal-load obj obj if-does-not-exist :source))
	    (:compile
	     (compile-file (namestring src) :output-file obj)
	     (internal-load obj obj if-does-not-exist :binary))
	    (:query
	     (if (y-or-n-p "Load source file ~A which is newer~%  ~
	                    than presumed object file ~A? "
			   (namestring src)
			   (namestring obj))
		 (internal-load obj obj if-does-not-exist :source)
		 (internal-load obj obj if-does-not-exist :binary)))
	    (T
	     (error
	      "*Load-if-source-newer* contains ~A which is not one of:~%  ~
	       :load-object, :load-source, :compile, or :query."
	      *load-if-source-newer*))))
	 (t
	  (internal-load obj obj if-does-not-exist :binary))))
       (t
	(internal-load srcn srcn if-does-not-exist :source)))))))


;;;; Actual FOP definitions:

(define-fop (fop-nop 0 :nope))
(define-fop (fop-pop 1 nil) (push-table (pop-stack)))
(define-fop (fop-pop-for-effect 65 nil) (pop-stack))
(define-fop (fop-push 2) (svref *current-fop-table* (read-arg 4)))
(define-fop (fop-byte-push 3) (svref *current-fop-table* (read-arg 1)))

(define-fop (fop-empty-list 4) ())
(define-fop (fop-truth 5) t)
(define-fop (fop-misc-trap 66)
	    (%primitive make-other-immediate-type 0 vm:unbound-marker-type))

(define-fop (fop-character 68)
  (code-char (read-arg 3)))
(define-fop (fop-short-character 69)
  (code-char (read-arg 1)))

(clone-fop (fop-struct 48)
	   (fop-small-struct 49)
  (let* ((size (clone-arg))
	 (res (make-structure size)))
    (declare (type index size))
    (do ((n (1- size) (1- n)))
	((minusp n))
      (declare (type (integer -1 #.most-positive-fixnum) n))
      (setf (structure-ref res n) (pop-stack)))
    res))

(define-fop (fop-end-group 64 :nope) (throw 'group-end t))
(define-fop (fop-end-header 255)
  (error "Fop-End-Header was executed???"))

;;; In the normal loader, we just ignore these.  Genesis overwrites
;;; fop-maybe-cold-load with something that knows when to revert to
;;; cold-loading or not.
;;; 
(define-fop (fop-normal-load 81 :nope))
(define-fop (fop-maybe-cold-load 82 :nope))

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
    (%make-ratio (pop-stack) den)))

(define-fop (fop-complex 71)
  (let ((im (pop-stack)))
    (%make-complex (pop-stack) im)))

(define-fop (fop-single-float 46)
  (make-single-float (load-s-integer 4)))

(define-fop (fop-double-float 47)
  (let ((lo (ldb (byte 32 0) (load-s-integer 4))))
    (make-double-float (load-s-integer 4) lo)))


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
	 (length (length vec))
	 (res (make-array-header vm:simple-array-type rank)))
    (declare (simple-array vec)
	     (type (unsigned-byte 24) rank))
    (set-array-header res vec length length 0
		      (do ((i rank (1- i))
			   (dimensions () (cons (pop-stack) dimensions)))
			  ((zerop i) dimensions))
		      nil)
    res))


;;; FOP-INT-VECTOR  --  Internal
;;;
;;; *** NOT *** the FOP-INT-VECTOR as currently documented in rtguts.  Size
;;; must be a directly supported I-vector element size, with no extra bits.
;;; This must be packed according to the local byte-ordering, allowing us to
;;; directly read the bits.
;;;
(define-fop (fop-int-vector 43)
  (prepare-for-fast-read-byte *fasl-file*
    (let* ((len (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (res (case size
		  (1 (make-array len :element-type 'bit))
		  (2 (make-array len :element-type '(unsigned-byte 2)))
		  (4 (make-array len :element-type '(unsigned-byte 4)))
		  (8 (make-array len :element-type '(unsigned-byte 8)))
		  (16 (make-array len :element-type '(unsigned-byte 16)))
		  (32 (make-array len :element-type '(unsigned-byte 32)))
		  (t (error "Losing i-vector element size: ~S" size)))))
      (declare (type index len))
      (done-with-fast-read-byte)
      (read-n-bytes *fasl-file* res 0 (ceiling (* size len) vm:byte-bits))
      res)))


(define-fop (fop-uniform-int-vector 44)
  (prepare-for-fast-read-byte *fasl-file*
    (let* ((n (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (value (fast-read-variable-u-integer (ceiling size 8))))
      (done-with-fast-read-byte)
      (make-array n :element-type `(unsigned-byte ,size)
		  :initial-element value))))

(define-fop (fop-eval 53)
  (let ((result (eval (pop-stack))))
    (when *load-print-stuff*
      (load-fresh-line)
      (prin1 result)
      (terpri))
    result))

(define-fop (fop-eval-for-effect 54 nil)
  (let ((result (eval (pop-stack))))
    (when *load-print-stuff*
      (load-fresh-line)
      (prin1 result)
      (terpri))))

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
    (if (structurep obj)
	(setf (c::structure-ref obj idx) val)
	(setf (svref obj idx) val))))

(define-fop (fop-structset 204 nil)
  (setf (c::structure-ref (svref *current-fop-table* (read-arg 4))
			  (read-arg 4))
	(pop-stack)))

(define-fop (fop-nthcdr 203 t)
  (nthcdr (read-arg 4) (pop-stack)))

;;;; Loading functions:

(define-fop (fop-code-format 57 :nope)
  (setf *current-code-format*
	(cons (read-arg 1) (read-arg 1))))

;;; Load-Code loads a code object.  NItems objects are popped off the stack for
;;; the boxed storage section, then Size bytes of code are read in.
;;;
(defmacro load-code (nitems size)
  `(if *current-code-format*
       (let ((implementation (car *current-code-format*))
	     (version (cdr *current-code-format*)))
	 (unless (= implementation
		    #.(c:backend-fasl-file-implementation c:*backend*))
	   (error "~A was compiled for a ~A, but this is a ~A"
		  *Fasl-file*
		  (or (elt c:fasl-file-implementations implementation)
		      "unknown machine")
		  (or (elt c:fasl-file-implementations
			   #.(c:backend-fasl-file-implementation c:*backend*))
		      "unknown machine")))
	 (unless (= version #.(c:backend-fasl-file-version c:*backend*))
	   (error "~A was compiled for fasl-file version ~A, ~
	           but this is version ~A"
	    *Fasl-file* version #.(c:backend-fasl-file-version c:*backend*)))
	 (let ((box-num ,nitems)
	       (code-length ,size))
	   (declare (fixnum box-num code-length))
	   (let ((code (%primitive allocate-code-object box-num code-length))
		 (index (+ vm:code-trace-table-offset-slot box-num)))
	     (setf (code-header-ref code vm:code-debug-info-slot) (pop-stack))
	     (dotimes (i box-num)
	       (declare (fixnum i))
	       (setf (code-header-ref code (decf index)) (pop-stack)))
	     (system:without-gcing
	      (read-n-bytes *fasl-file* (code-instructions code) 0
			    code-length))
	     code)))
       (error
	"Code Format not set?  Can't load code until after FOP-CODE-FORMAT.")))

(define-fop (fop-code 58)
  (load-code (read-arg 4) (read-arg 4)))

(define-fop (fop-small-code 59)
  (load-code (read-arg 1) (read-arg 2)))


;;; Now a NOOP except in cold load... 
(define-fop (fop-fset 74 nil)
  (pop-stack)
  (pop-stack))


;;; Modify a slot in a Constants object.
;;;
(clone-fop (fop-alter-code 140 nil) (fop-byte-alter-code 141)
  (let ((value (pop-stack))
	(code (pop-stack)))
    (setf (code-header-ref code (clone-arg)) value)
    (undefined-value)))

(define-fop (fop-function-entry 142)
  (let ((type (pop-stack))
	(arglist (pop-stack))
	(name (pop-stack))
	(code-object (pop-stack))
	(offset (read-arg 4)))
    (declare (type index offset))
    (unless (zerop (logand offset vm:lowtag-mask))
      (error "Unaligned function object, offset = #x~X." offset))
    (let ((fun (%primitive compute-function code-object offset)))
      (%primitive set-function-self fun fun)
      (%primitive set-function-next fun
		  (%primitive code-entry-points code-object))
      (%primitive set-code-entry-points code-object fun)
      (%primitive set-function-name fun name)
      (%primitive set-function-arglist fun arglist)
      (%primitive set-function-type fun type)
      (when *load-print-stuff*
	(load-fresh-line)
	(format t "~S defined~%" fun))
      fun)))


;;;; Linkage fixups.

;;; These two variables are initially filled in by Genesis.

(defvar *initial-assembler-routines*)
(defvar *initial-foreign-symbols*)

(defvar *assembler-routines* (make-hash-table :test #'eq))
(defvar *foreign-symbols* (make-hash-table :test #'equal))

(defun loader-init ()
  (dolist (routine *initial-assembler-routines*)
    (setf (gethash (car routine) *assembler-routines*) (cdr routine)))
  (dolist (symbol *initial-foreign-symbols*)
    (setf (gethash (car symbol) *foreign-symbols*) (cdr symbol)))
  (makunbound '*initial-assembler-routines*)
  (makunbound '*initial-foreign-symbols*))


(define-fop (fop-foreign-fixup 147)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (read-n-bytes *fasl-file* sym 0 len)
    (multiple-value-bind
	(value found)
	(gethash sym *foreign-symbols* 0)
      (unless found
	(error "Unknown foreign symbol: ~S" sym))
      (vm:fixup-code-object code-object (read-arg 4) value kind))
    code-object))

(define-fop (fop-assembler-code 144)
  (error "Cannot load assembler code."))

(define-fop (fop-assembler-routine 145)
  (error "Cannot load assembler code."))

(define-fop (fop-assembler-fixup 148)
  (let ((routine (pop-stack))
	(kind (pop-stack))
	(code-object (pop-stack)))
    (multiple-value-bind
	(value found)
	(gethash routine *assembler-routines*)
      (unless found
	(error "Undefined assembler routine: ~S" routine))
      (vm:fixup-code-object code-object (read-arg 4) value kind))
    code-object))



(proclaim '(maybe-inline read-byte))
