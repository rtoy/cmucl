;;; -*- Log: clc.log; Package: Compiler -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Assembler for the Common Lisp Compiler.  This file deals with turning
;;; LAP code into binary code and dumping the results in the right places.
;;; There also ulilities for dealing with the various output files.
;;; 

;;; Written by many hands: Joe Ginder, Scott Fahlman, Dave Dill,
;;; Walter van Roggen, and Skef Wholey.

;;; Currently maintained by Scott Fahlman.

(in-package 'compiler :use '(system))

(import '(lisp::%fasl-code-format))

;;; Version number.

(defparameter assembler-version "2.0")
(defparameter target-fasl-code-format 3)

(proclaim '(special compiler-version target-machine target-system
		    *compile-to-lisp* *lisp-package* *keyword-package*))



(defvar function-name nil
  "Holds a symbol that names the function currently being compiled,
  or nil if between functions.")

;;; Output streams:
;;; If a stream is NIL, don't produce that kind of output.
(defvar *clc-fasl-stream* nil)
(defvar *clc-lap-stream* nil)
(defvar *clc-err-stream* nil)

;;; Stuff we keep track of for the error log:

(defvar functions-with-errors nil
  "A list of all functions that did not compile properly due to errors in
  the code.")

(defvar error-count 0
  "The number of errors generated during this compilation.")

(defvar warning-count 0
  "The number of warnings generated during this compilation.")

(defvar unknown-functions nil
  "List of functions called but not yet seen and not built-in.  The user
  is informed of any function still on this list at the end of a file
  compilation.")

(defvar unknown-free-vars nil
  "A list of all variables referenced free in the compilation, but not
  bound or declared special anywhere.  These are assumed to be special
  variables, but are listed in a warning message at the end of the
  compilation.")

(defvar *verbose* t
  "If nil, only true error messages and warnings go to the error stream.
  If non-null, prints a message as each function is compiled.")

(defvar *compile-to-lisp* nil
  "If non-null, stuff compiled definitions into the compiler's own Lisp
  environment.")

(defvar *clc-input-stream* nil)
(defvar *input-filename* nil "Truename of file being compiled.")
(defvar *compiler-is-reading* nil
  "This is true only if we are actually doing a read from *clc-input-stream*.
  #, (in the reader) looks at this.")

  
;;; The Line-Length of the lap stream...
(defvar *lap-line-length*)

;;; The defined-from string for functions defined in the current source file:
(defvar *current-defined-from*)


;;;; Error Reporting:

;;; CLC-MUMBLE is just a format print to the error stream.

(defun clc-mumble (string &rest args)
  (when *clc-err-stream*
    (apply #'format *clc-err-stream* string args)))


;;; A COMMENT is something the user might like to know, but that will
;;; probably not affect the correctness of his code.

(defun clc-comment (string &rest args)
  (when *clc-err-stream*
    (if (or (not function-name) (eq function-name 'lisp::top-level-form))
	(format *clc-err-stream* "Comment between functions:~%  ")
	(format *clc-err-stream* "Comment in ~S:~%  " function-name))
    (apply #'format *clc-err-stream* string args)
    (terpri *clc-err-stream*)))


;;; A WARNING is something suspicious in the user's code that probably
;;; signals some form of lossage, but that may be ignored if the user
;;; knows what he is doing.

(defun clc-warning (string &rest args)
  (when *clc-err-stream*
    (incf warning-count)
    (if (or (not function-name) (eq function-name 'lisp::top-level-form))
	(format *clc-err-stream* "Warning between functions:~%  ")
	(format *clc-err-stream* "Warning in ~S:~%  " function-name))
    (apply #'format *clc-err-stream* string args)
    (terpri *clc-err-stream*)))


;;; An ERROR is a problem in the user's code that will definitely cause some
;;; lossage.  The compiler attempts to go on with the compilation so that
;;; as many errors as possible can be caught per compilation.

(defun clc-error (string &rest args)
  (when *clc-err-stream*
    (incf error-count)
    (cond ((or (not function-name) (eq function-name 'lisp::top-level-form))
	   (format *clc-err-stream* "Error between functions:~%  ")
	   (pushnew function-name functions-with-errors))
	  (t
	   (format *clc-err-stream* "Error in ~S:~%  " function-name)))
    (apply #'format *clc-err-stream* string args)
    (terpri *clc-err-stream*)))


;;; Keep the internal real and run times in these vars so that we can report
;;; the elapsed time.
(defvar *start-real-time*)
(defvar *start-run-time*)

;;; Start-Assembly  --  Internal
;;;
;;;    This function is called before assembling each batch of stuff.  It
;;; writes out the fasl file header and does other random stuff.  It is
;;; assumed that all the streams are initialized at this point.
;;;
(defun start-assembly ()
  (let* ((host (machine-instance))
	 (now (get-universal-time))
	 (now-string (universal-time-to-string now))
	 (in-string (if *input-filename* (namestring *input-filename*)))
	 (then (if *input-filename*
		   (file-write-date *input-filename*)))
	 (then-string (if then (universal-time-to-string then)))
	 (where (cond ((not *clc-input-stream*)
		       (format nil "Lisp on ~A, machine ~A" now-string host))
		      (in-string
		       (format nil "~A ~A" in-string now-string))
		      ((not then)
		       (format nil "~S on ~A, machine ~A" *clc-input-stream*
			       now-string host))
		      (t
		       (format nil "~A ~A" in-string then-string)))))
    
    ;; Set the defined-from string:
    (setq *current-defined-from* (format nil "~A ~D" where (or then now)))

    (when *input-filename*
      (setq *start-real-time* (get-internal-real-time))
      (setq *start-run-time* (get-internal-run-time))
      (clc-mumble "Error output from ~A.~@
		  Compiled on ~A by CLC version ~A.~2%"
		  where now-string compiler-version))

    (when *clc-lap-stream*
      (setq *lap-line-length* (or (lisp::line-length *clc-lap-stream*) 80))
      (format *clc-lap-stream*
	      "~:[Unreadble~;Readable~] LAP output from ~A.~@
	      Compiled on ~A by CLC version ~A.~%"
	      *print-readable-lap* where now-string compiler-version))
  
    (when *clc-fasl-stream*
      (format *clc-fasl-stream* "FASL FILE output from ~A~@
	      Compiled ~A on ~A~@
	      Compiler ~A, Assembler ~A, Lisp ~A~@
	      Targeted for ~A/~A, FASL code format ~D~%"
	      where now-string host compiler-version assembler-version
	      (lisp-implementation-version) target-machine target-system
	      c::target-fasl-code-format)
      (start-fasl-file))))

;;; Also the ten-dozenth place this is defined...
(defun universal-time-to-string (ut)
  (multiple-value-bind (sec min hour day month year)
		       (decode-universal-time ut)
    (format nil "~D-~A-~2,'0D ~D:~2,'0D:~2,'0D"
	    day (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
			  "Sep" "Oct" "Nov" "Dec")
		       (1- month))
	    (rem year 100)
	    hour min sec)))

(defun elapsed-time-to-string (it)
  (let ((tsec (truncate it internal-time-units-per-second)))
    (multiple-value-bind (tmin sec)
			 (truncate tsec 60)
      (multiple-value-bind (thr min)
			   (truncate tmin 60)
	(format nil "~D:~2,'0D:~2,'0D" thr min sec)))))


;;; Finish-Assembly  --  Internal
;;;
;;;
(defun finish-assembly ()
  (when *clc-fasl-stream* (terminate-fasl-file))
  
  ;; All done.  Let the post-mortems begin.
  (when *input-filename*
    (clc-mumble "~%Finished compilation of file ~S.~%"
		(namestring *input-filename*))
    (clc-mumble "~S Errors, ~S Warnings.~%" error-count warning-count)
    (clc-mumble "Elapsed time ~A, run time ~A.~2%"
		(elapsed-time-to-string (- (get-internal-real-time)
					   *start-real-time*))
		(elapsed-time-to-string (- (get-internal-run-time)
					   *start-run-time*))))
    
  (when functions-with-errors
    (clc-mumble "Errors were detected in the following functions:~% ~S~%"
		(nreverse functions-with-errors)))
  (when unknown-functions
    (clc-mumble
     "These symbols were called as functions but not declared or defined:~% ~S~%"
     (nreverse unknown-functions)))
  (do* ((p NIL)
	(l unknown-free-vars (cdr l))
	(a (car l) (car l)))
       ((null l))
    (if (get a 'globally-special-in-compiler)
	(cond (p (rplacd p (cdr l))
		 (setq l p))
	      (T (setq unknown-free-vars (cdr l))
		 (setq p NIL)))
	(setq p l)))
  (when unknown-free-vars
    (clc-mumble
     "The following variables, assumed to be special, are referenced~@
     but never declared:~% ~S~%"
     (nreverse unknown-free-vars))))

;;;; Fasl dumping stuff:

;;; Next slot to be filled in the fasload table.  Reset at the start of
;;; each new FASL file.
(defvar fop-table-counter 0)

;;; For speed, we keep the table index for each symbol in a property
;;; under that symbol, rather than in an A-list.  All the symbols with
;;; FOP-TABLE-INDEX properties are kept on this list, so that we can
;;; clean up when a new FASL file is started.
(defvar fop-table-symbol-list nil)

;;; FOP-TABLE-PACKAGE-LIST is an a-list mapping packages to their fop-table
;;; indices.  Each entry is (package . index).
(defvar fop-table-package-list nil)

;;; When we dump lists and strings, we look for them in this hashtable.
;;; If we find what we are looking for, we just push the thing from the
;;; table.  If it isn't there, we dump the object and then enter it.
(defvar *table-table* (make-hash-table :test #'equal))

;;; If true, then we must dump stuff so that it neither adds to nor refers
;;; the table.  This is used by the forms which need to be available
;;; at cold load time.
(defvar *hands-off-table* nil)

;;; Dump a single byte to the *CLC-FASL-STREAM* file.  We buffer these until
;;; we collect 512 of them, and then write-string them to the FASL stream.
(defvar *dump-byte-buffer* (make-array 512 :element-type '(mod 256)))
(defvar *dump-byte-index*)


(defun dump-dump-byte-buffer ()
  (write-string *dump-byte-buffer* *clc-fasl-stream*
		:start 0 :end *dump-byte-index*)
  (setq *dump-byte-index* 0))

(defmacro dump-byte (b)
  `(progn
    (if (= *dump-byte-index* 512)
	(dump-dump-byte-buffer))
    (setf (aref *dump-byte-buffer* *dump-byte-index*) (logand ,b #x+FF))
    (incf *dump-byte-index*)))


;;; Put out the code for one FASL-format operator.

(defun dump-fop (fs)
  (let ((val (get fs 'lisp::fop-code)))
    (if (null val)
	(error "Compiler bug: ~S not a legal fasload operator." fs)
	(dump-byte val))))


(defmacro dump-fop* (n byte-fop word-fop)
  `(cond ((< ,n 256)
	  (dump-fop ',byte-fop)
	  (dump-byte ,n))
	 (t
	  (dump-fop ',word-fop)
	  (quick-dump-number ,n 4))))

;;; Dump out number NUM as BYTES bytes.

(defun quick-dump-number (num bytes)
  (do ((n num (ash n -8))
       (i bytes (1- i)))
      ((= i 0))
    (dump-byte (logand n #o377))))

;;; Start-Fasl-File  --  Internal
;;;
;;;    Set up fasdumper state and finish off the header.  The "FASL FILE"
;;; header should already be written.  Called by Start-Assembly.
;;;
(defun start-fasl-file ()
  ;; We now have a virgin FOP-TABLE, so clean up any old stuff.
  (setq fop-table-counter 0)
  ;; Just in case this didn't get cleaned up after an earlier compile.
  (cond (fop-table-symbol-list
	 (do ((sl fop-table-symbol-list (cdr sl)))
	     ((null sl) (setq fop-table-symbol-list nil))
	   (remprop (car sl) 'lisp::fop-table-index))))
  ;; Clear the package alist.
  (setq fop-table-package-list nil)
  ;; And the table hashtable.
  (clrhash *table-table*)
  ;; Reset the dump-byte buffer
  (setq *dump-byte-index* 0)
  ;; Print header stuff.
  (dump-byte 255)
  ;; Perq code format.
  (dump-fop 'lisp::fop-code-format)
  (dump-byte c::target-fasl-code-format))

;;; Terminate-Fasl-File  --  Internal
;;;
;;;    Finish off the current fasl group and clean up.  Called from 
;;; Finish-Assembly.
;;;
(defun terminate-fasl-file ()
  (dump-fop 'lisp::fop-verify-empty-stack)
  (dump-fop 'lisp::fop-verify-table-size)
  (quick-dump-number fop-table-counter 4)
  (dump-fop 'lisp::fop-end-group)
  (dump-dump-byte-buffer)
  (do ((sl fop-table-symbol-list (cdr sl)))
      ((null sl) (setq fop-table-symbol-list nil))
    (remprop (car sl) 'lisp::fop-table-index)))
  
;;; Fasl-Dump-Cold-Load-Form  --  Internal
;;;
;;;    Similar to fasl-dump-form, except that the form is to be evaluated
;;; at cold load time when in cold load.  This is used to dump package
;;; frobbing forms.
;;;
(defun fasl-dump-cold-load-form (form)
  (dump-fop 'lisp::fop-normal-load)
  (let ((*hands-off-table* t))
    (dump-object form))
  (dump-fop 'lisp::fop-eval-for-effect)
  (dump-fop 'lisp::fop-maybe-cold-load))


;;; Dump-Object  -- Internal
;;;
;;;    Dump an object of any type.  This function dispatches to the correct
;;; type-specific dumping function.  Table entry and lookup for non-immediate
;;; objects other than lists and symbols is done here.
;;;
(defun dump-object (x)
  (cond
   ((listp x)
    (cond ((null x)
	   (dump-fop 'lisp::fop-empty-list))
	  ((eq (car x) '%eval-at-load-time)
	   (load-time-eval x))
	  (t
	   (dump-list x))))
   ((symbolp x)
    (if (eq x t) 
	(dump-fop 'lisp::fop-truth)
	(dump-symbol x)))
   ((fixnump x) (dump-integer x))
   ((characterp x) (dump-character x))
   ((typep x 'short-float) (dump-short-float x))
   (t
    ;;
    ;; Look for it in the table; if it is there, push it, otherwise
    ;; dump it.
    (let ((index (gethash x *table-table*)))
      (cond
       ((and index (not *hands-off-table*))
	(dump-fop* index lisp::fop-byte-push lisp::fop-push))
       (t
	(typecase x
	  (vector
	   (cond ((stringp x) (dump-string x))
		 ((subtypep (array-element-type x) '(unsigned-byte 16))
		  (dump-i-vector x))
		 (t
		  (dump-vector x))))
	  (array (dump-array x))
	  (number
	   (etypecase x
	     (ratio (dump-ratio x))
	     (complex (dump-complex x))
;	     (single-float (dump-single-float x))
	     (long-float (dump-long-float x))
	     (integer (dump-integer x))))
	  (compiled-function (dump-function x))
	  (t
	   (clc-error "This object cannot be dumped into a fasl file:~%  ~S" x)
	   (dump-object nil)))
	;;
	;; If wasn't in the table, put it there...
	(unless *hands-off-table*
	  (dump-fop 'lisp::fop-pop)
	  (dump-fop* fop-table-counter lisp::fop-byte-push lisp::fop-push)
	  (setf (gethash x *table-table*) fop-table-counter)
	  (incf fop-table-counter))))))))

;;;; Number Dumping:

;;; Dump a ratio

(defun dump-ratio (x)
  (dump-object (numerator x))
  (dump-object (denominator x))
  (dump-fop 'lisp::fop-ratio))

;;; Or a complex...

(defun dump-complex (x)
  (dump-object (realpart x))
  (dump-object (imagpart x))
  (dump-fop 'lisp::fop-complex))

;;; Dump an integer.

(defun dump-integer (n)
  (let* ((bytes (compute-bytes n)))
    (cond ((= bytes 1)
	   (dump-fop 'lisp::fop-byte-integer)
	   (dump-byte n))
	  ((< bytes 5)
	   (dump-fop 'lisp::fop-word-integer)
	   (quick-dump-number n 4))
	  ((< bytes 256)
	   (dump-fop 'lisp::fop-small-integer)
	   (dump-byte bytes)
	   (quick-dump-number n bytes))
	  (t (dump-fop 'lisp::fop-integer)
	     (quick-dump-number bytes 4)
	     (quick-dump-number n bytes)))))

;;; Compute how many bytes it will take to represent signed integer N.

(defun compute-bytes (n)
  (truncate (+ (integer-length n) 8) 8))

;;;
;;; These two are almost exactly alike, and could easily be the same function.

(defun dump-short-float (x)
  (multiple-value-bind (f exponent sign) (decode-float x)
    (let ((mantissa (truncate (scale-float (* f sign) (float-precision f)))))
      (dump-fop 'lisp::fop-float)
      (dump-byte (1+ (integer-length exponent)))
      (quick-dump-number exponent (compute-bytes exponent))
      (dump-byte (1+ (integer-length mantissa)))
      (quick-dump-number mantissa (compute-bytes mantissa)))))

#|
(defun dump-single-float (x)
  (multiple-value-bind (f exponent sign) (decode-float x)
    (let ((mantissa (truncate (scale-float (* f sign) (float-precision f)))))
      (dump-fop 'lisp::fop-float)
      (dump-byte (1+ (integer-length exponent)))
      (dump-byte exponent)
      (dump-byte (1+ (integer-length mantissa)))
      (quick-dump-number mantissa (compute-bytes mantissa)))))
|#
;;; For long-floats we're careful that the dumped mantissa actually
;;; has 63 significant bits, so the fasloader can recognize it as such.

(defun dump-long-float (x)
  (multiple-value-bind (f exponent sign) (decode-float x)
    (let ((mantissa (truncate (scale-float (* f sign) (float-precision f)))))
      (dump-fop 'lisp::fop-float)
      (dump-byte (1+ (integer-length exponent)))
      (quick-dump-number exponent (compute-bytes exponent))
      (dump-byte (1+ (integer-length mantissa)))
      (quick-dump-number mantissa (compute-bytes mantissa)))))

;;;; Symbol Dumping:

(defun dump-symbol (s)
  (let ((number (get s 'lisp::fop-table-index)))
    (if (and number (not *hands-off-table*))
	;; Symbol is already in the table.  Just dump the index.
	(dump-fop* number lisp::fop-byte-push lisp::fop-push)
	;; Got to dump the symbol and put it into the table.
	(let* ((pname (symbol-name s))
	       (pname-length (length pname))
	       (pkg (symbol-package s)))
	  (cond ((null pkg)
		 ;; Symbol is uninterned.
		 (dump-fop* pname-length lisp::fop-uninterned-small-symbol-save
			    lisp::fop-uninterned-symbol-save))
		((eq pkg *package*)
		 ;; Symbol is in current default package.  Just dump it.
		 (dump-fop* pname-length lisp::fop-small-symbol-save
			    lisp::fop-symbol-save))
		((eq pkg *lisp-package*)
		 (dump-fop* pname-length lisp::fop-lisp-small-symbol-save
			    lisp::fop-lisp-symbol-save))
		((eq pkg *keyword-package*)
		 ;; Symbol is in current default package.  Just dump it.
		 (dump-fop* pname-length lisp::fop-keyword-small-symbol-save
			    lisp::fop-keyword-symbol-save))
		(t
		 ;; We have to dump this symbol with a package specifier.
		 (let ((entry (assq pkg fop-table-package-list)))
		   ;; Put the package into the table unless it's already there.
		   (unless entry
		     (unless *hands-off-table*
		       (dump-fop 'lisp::fop-normal-load))
		     (dump-string (package-name pkg))
		     (dump-fop 'lisp::fop-package)
		     (dump-fop 'lisp::fop-pop)
		     (unless *hands-off-table*
		       (dump-fop 'lisp::fop-maybe-cold-load))
		     (setq entry (cons pkg fop-table-counter))
		     (push entry fop-table-package-list)
		     (incf fop-table-counter))
		   (setq entry (cdr entry))
		   (cond
		    ((< pname-length 256)
		     (dump-fop* entry
				lisp::fop-small-symbol-in-byte-package-save
				lisp::fop-small-symbol-in-package-save)
		     (dump-byte pname-length))
		    (t
		     (dump-fop* entry
				lisp::fop-symbol-in-byte-package-save
				lisp::fop-symbol-in-package-save)
		     (quick-dump-number pname-length 4))))))
	  ;; Finish dumping the symbol and put it in table.
	  (do ((index 0 (1+ index)))
	      ((= index pname-length))
	    (dump-byte (char-code (schar pname index))))
	  (unless *hands-off-table*
	    (setf (get s 'lisp::fop-table-index) fop-table-counter))
	  (push s fop-table-symbol-list)
	  (setq fop-table-counter (1+ fop-table-counter))))))

;;; Dumper for lists.

(defun dump-list (list)
  (if (null list)
      (dump-fop 'lisp::fop-empty-list)
      (let ((index (gethash list *table-table*)))
	(cond ((and index (not *hands-off-table*))
	       (dump-fop* index lisp::fop-byte-push lisp::fop-push))
	      (t
	       (do ((l list (cdr l))
		    (n 0 (1+ n)))
		   ((atom l)
		    (cond ((null l)
			   (terminate-undotted-list n))
			  (t (dump-object l)
			     (terminate-dotted-list n))))
		 (dump-object (car l)))
	       (unless *hands-off-table*
		 (dump-fop 'lisp::fop-pop)
		 (dump-fop* fop-table-counter lisp::fop-byte-push
			    lisp::fop-push)
		 (setf (gethash list *table-table*) fop-table-counter)
		 (incf fop-table-counter)))))))

(defun terminate-dotted-list (n)
  (case n
    (1 (dump-fop 'lisp::fop-list*-1))
    (2 (dump-fop 'lisp::fop-list*-2))
    (3 (dump-fop 'lisp::fop-list*-3))
    (4 (dump-fop 'lisp::fop-list*-4))
    (5 (dump-fop 'lisp::fop-list*-5))
    (6 (dump-fop 'lisp::fop-list*-6))
    (7 (dump-fop 'lisp::fop-list*-7))
    (8 (dump-fop 'lisp::fop-list*-8))
    (T (do ((nn n (- nn 255)))
	   ((< nn 256)
	    (dump-fop 'lisp::fop-list*)
	    (dump-byte nn))
	 (dump-fop 'lisp::fop-list*)
	 (dump-byte 255)))))

;;; If N > 255, must build list with one list operator, then list* operators.

(defun terminate-undotted-list (n)
    (case n
      (1 (dump-fop 'lisp::fop-list-1))
      (2 (dump-fop 'lisp::fop-list-2))
      (3 (dump-fop 'lisp::fop-list-3))
      (4 (dump-fop 'lisp::fop-list-4))
      (5 (dump-fop 'lisp::fop-list-5))
      (6 (dump-fop 'lisp::fop-list-6))
      (7 (dump-fop 'lisp::fop-list-7))
      (8 (dump-fop 'lisp::fop-list-8))
      (T (cond ((< n 256)
		(dump-fop 'lisp::fop-list)
		(dump-byte n))
	       (t (dump-fop 'lisp::fop-list)
		  (dump-byte 255)
		  (do ((nn (- n 255) (- nn 255)))
		      ((< nn 256)
		       (dump-fop 'lisp::fop-list*)
		       (dump-byte nn))
		    (dump-fop 'lisp::fop-list*)
		    (dump-byte 255)))))))

;;;; Array dumping:

;;; Named G-vectors get their subtype field set at load time.

(defun dump-vector (obj)
  (cond ((and (simple-vector-p obj)
	      (= (%primitive get-vector-subtype obj)
		 %g-vector-structure-subtype))
	 (normal-dump-vector obj)
	 (dump-fop 'lisp::fop-structure))
	(t
	 (normal-dump-vector obj))))

(defun normal-dump-vector (v)
  (do ((index 0 (1+ index))
       (length (length v)))
      ((= index length)
       (dump-fop* length lisp::fop-small-vector lisp::fop-vector))
    (dump-object (aref v index))))

;;; Dump a string.

(defun dump-string (s)
  (let ((length (length s)))
    (dump-fop* length lisp::fop-small-string lisp::fop-string)
    (dotimes (i length)
      (dump-byte (char-code (char s i))))))

;;; Dump-Array  --  Internal
;;;
;;;    Dump a multi-dimensional array.  Someday when we figure out what
;;; a displaced array looks like, we can fix this.
;;;
(defun dump-array (array)
  (unless (zerop (%primitive header-ref array %array-displacement-slot))
    (clc-error "Attempt to dump an array with a displacement, you lose big.")
    (dump-object nil)
    (return-from dump-array nil))

  (let ((rank (array-rank array)))
    (dotimes (i rank)
      (dump-integer (array-dimension array i)))
    (dump-object (%primitive header-ref array %array-data-slot))
    (dump-fop 'lisp::fop-array)
    (quick-dump-number rank 4)))


;;; Dump a character.

(defun dump-character (ch)
  (cond
   ((string-char-p ch)
    (dump-fop 'lisp::fop-short-character)
    (dump-byte (char-code ch)))
   (t
    (dump-fop 'lisp::fop-character)
    (dump-byte (char-code ch))
    (dump-byte (char-bits ch))
    (dump-byte (char-font ch)))))

