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

(import '(lisp::%fasl-code-format lisp::compiler-notification))

;;; Version number.

(defparameter assembler-version "2.0")
(defparameter target-fasl-code-format 3)

(proclaim '(special compiler-version target-machine target-system
		    *compile-to-lisp* *lisp-package* *keyword-package*))
   
  
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
  (compiler-notification :comment function-name)
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
  (compiler-notification :warning function-name)
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
  (compiler-notification :error function-name)
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
	 (then (if (and *input-filename*
			(not *surrogate-input-filename*))
		   (file-write-date *input-filename*)))
	 (then-string (if then (universal-time-to-string then)))
	 (where (cond ((not *clc-input-stream*)
		       (format nil "Lisp on ~A, machine ~A" now-string host))
		      ((and *surrogate-input-filename* in-string)
		       (format nil "~A ~A" in-string now-string))
		      ((not then)
		       (format nil "~S on ~A, machine ~A" *clc-input-stream*
			       now-string host))
		      (t
		       (format nil "~A ~A" in-string then-string)))))
    
    ;; Set the defined-from string:
    (setq *current-defined-from* (format nil "~A ~D" where (or then now)))

    (when (and *input-filename* (not *surrogate-input-filename*))
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
	      target-fasl-code-format)
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
  (when (and *input-filename* (not *surrogate-input-filename*))
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
  (dump-byte target-fasl-code-format))

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

;;; Start-Fasl-Function  --  Internal
;;;
;;;    Push on the fasl stack the function-overhead pseudo-constants for
;;; Lap-Function.
;;;
(defun start-fasl-function (lap-function)
  ;;
  ;; First comes the function type, which is not really a boxed thing
  ;; at all, but rather the subtype of the function object.
  (dump-integer (lap-function-subtype lap-function))
  ;;
  ;; The first actual boxed thing is Function-Name, which is the
  ;; name of the innermost enclosing named function.
  (dump-object (lap-function-name lap-function))
  ;;
  ;; Second, place holder for code vector.
  (dump-fop 'lisp::fop-misc-trap)
  ;;
  ;; The Arg info is third:
  (dump-integer (lap-function-arg-info lap-function))
  ;;
  ;; Fourth, the defined-from string:
  (dump-object *current-defined-from*)
  ;;
  ;; Fifth, debug arguments:
  (dump-object (make-arg-names lap-function)))

;;; Finish-Fasl-Function  --  Internal
;;;
;;;    Turn the constants on the fop-stack into a function-object corresponding
;;; to Lap-Function and Num-Consts.
;;;
(defun finish-fasl-function (lap-function num-consts fixups)
  (declare (ignore lap-function))
  (let ((num-consts (+ num-consts %function-constants-offset 1))
	(num-bytes romp-code-size))
    (cond ((and (< num-consts #x100) (< num-bytes #x10000))
	   (dump-fop 'lisp::fop-small-code)
	   (dump-byte num-consts)
	   (quick-dump-number num-bytes 2))
	  (t
	   (dump-fop 'lisp::fop-code)
	   (quick-dump-number num-consts 4)
	   (quick-dump-number num-bytes 4)))
    ;;
    ;; Dump code bytes.
    (dotimes (i romp-code-size)
      (dump-byte (aref romp-code-vector i)))

    ;;
    ;; Dump loader function link fixups.
    (dolist (fixup fixups)
      (cond ((eq (car fixup) 'miscop)
	     (let ((pc (cadr fixup))
		   (index (caddr fixup)))
	       (dump-fop 'lisp::fop-miscop-fixup)
	       (quick-dump-number index 2)
	       (quick-dump-number pc 4)))
	    ((eq (car fixup) 'user-miscop)
	     (let ((pc (cadr fixup)))
	       (dump-symbol (caddr fixup))
	       (dump-fop 'lisp::fop-user-miscop-fixup)
	       (quick-dump-number pc 4)))
	    ((eq (car fixup) 'load-link)
	     (let* ((lte (caddr fixup))
		    (function (cadr lte))
		    (nargs (caddr lte))
		    (flag (cadddr lte))
		    (offset (cadr fixup)))
	       (dump-symbol function)
	       (dump-fop 'lisp::fop-link-address-fixup)
	       (dump-byte nargs)
	       (dump-byte (if flag 1 0))
	       (quick-dump-number offset 4)))
	    (T (clc-error "Unknown fixup block: ~A." fixup))))))

;;; Dump-Function  --  Internal
;;;
;;;    Dump a compiled function object.  We signal an error if the
;;; current fasl code format is different from the target fasl code format,
;;; since dumping a function is ill-defined then.
;;;
(defun dump-function (fun)
  (when (/= %fasl-code-format target-fasl-code-format)
    (clc-error "Cannot dump compiled function for different fasl format:~%  ~S"
	       fun)
    (dump-object nil)
    (return-from dump-function nil))
  
  (let* ((num-consts (1+ (%primitive header-length fun)))
	 (code (%primitive header-ref fun %function-code-slot))
	 (num-bytes (length code)))
    ;;
    ;; Dump the subtype which is a pseudo-boxed-thing.
    (dump-integer (%primitive get-vector-subtype fun))
    ;;
    ;; Dump all the slots except for the code; leave a placeholder for it.
    (dotimes (i (1- num-consts))
      (if (= i %function-code-slot)
	  (dump-fop 'lisp::fop-misc-trap)
	  (dump-object (%primitive header-ref fun i))))
    ;;
    ;; Dump the appropriate fop.
    (cond ((and (< num-consts #x100) (< num-bytes #x10000))
	   (dump-fop 'lisp::fop-small-code)
	   (dump-byte num-consts)
	   (quick-dump-number num-bytes 2))
	  (t
	   (dump-fop 'lisp::fop-code)
	   (quick-dump-number num-consts 4)
	   (quick-dump-number num-bytes 4)))
    ;;
    ;; Dump code bytes.
    (dotimes (i num-bytes)
      (dump-byte (aref code i)))))

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
   ((short-floatp x) (dump-short-float x))
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

;;; Load-Time-Eval  --  Internal
;;;
;;;    This guy deals with the magical %Eval-At-Load-Time marker that
;;; #, turns into when the *compiler-is-reading* and a fasl file is being
;;; written.
;;;
(defun load-time-eval (x)
  (when *compile-to-lisp*
    (clc-error "#,~S in a bad place." (third x)))
  (assemble-one-lambda (cadr x))
  (dump-fop 'lisp::fop-funcall)
  (dump-byte 0))

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

;;; Dump-I-Vector  --  Internal  
;;;
;;;    Dump an I-Vector using the Guy Steele memorial fasl-operation.
;;;
(defun dump-i-vector (vec)
  (let* ((len (length vec))
	 (ac (%primitive get-vector-access-code
			 (if (%primitive complex-array-p vec)
			     (%primitive header-ref vec %array-data-slot)
			     vec)))
	 (size (ash 1 ac))
	 (count (ceiling size 8))
	 (ints-per-entry (floor (* count 8) size)))
    (declare (fixnum len ac size count ints-per-entry))
    (dump-fop 'lisp::fop-int-vector)
    (quick-dump-number len 4)
    (dump-byte size)
    (dump-byte count)
    (if (> ints-per-entry 1)
	(do ((prev 0 end)
	     (end ints-per-entry (the fixnum (+ end ints-per-entry))))
	    ((>= end len)
	     (unless (= prev len)
	       (do ((pos (* (1- ints-per-entry) size) (- pos size))
		    (idx prev (1+ idx))
		    (res 0))
		   ((= idx len)
		    (dump-byte res))
		 (setq res (dpb (aref vec idx) (byte size pos) res)))))
	  (declare (fixnum prev end))
	  (do* ((idx prev (1+ idx))
		(res 0))
	       ((= idx end)
		(dump-byte res))
	    (declare (fixnum idx))
	    (setq res (logior (ash res size) (aref vec idx)))))
	(dotimes (i len)
	  (declare (fixnum i))
	  (quick-dump-number (aref vec i) count)))))

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


;;;; THE ASSEMBLER

;;; List of (TAG . POSITION) for each tag within a function.
(defvar Assemble-Label-List)

;;; Vector to hold assembled code.
(defvar Romp-Code-Vector)

;;; List of free code vector buffers.
(defvar free-code-vectors ())

;;; Define macros to access information about Romp instructions.
;;;
;;; Romp-Instruction-Type given a symbol which is the name of a Romp
;;; instruction returns the type of the instruction (i.e., one of JI, X,
;;; DS, R, BI, BA, or D).

(defmacro Get-Instruction-Type (Instruction)
  `(get ,Instruction 'romp-instruction-type))

;;; Romp-Operation-Code given a symbol which is the name of a Romp
;;; instruction returns the integer code for the instruction.

(defmacro Get-Operation-Code (Instruction)
  `(get ,Instruction 'romp-operation-code))

(defmacro Get-Condition-Code (Test)
  `(if (symbolp ,Test) (get ,Test 'Condition-Code) ,Test))

(defmacro Get-lis-label-value (label)
  `(logand (ash (+ (cdr (assoc (cadr ,label) assemble-label-list))
		   I-Vector-Header-Size) -16) #xF))

;;; Assemble-Function  --  Internal
;;;
;;;    Assembles the supplied lap-function and puts the definition all the
;;; places it belongs.
;;;
(defun assemble-function (lap-function)
  (let ((name (lap-function-name lap-function)))
    (case (lap-function-type lap-function)
      ((expr fexpr)
       (when *clc-fasl-stream* (dump-symbol name))
       (let ((res (assemble-one-lambda lap-function)))
	 (when *clc-fasl-stream* (dump-fop 'lisp::fop-fset))
	 (when *compile-to-lisp*
	   (set-symbol-function-carefully name res))))
      (macro
       (when *clc-fasl-stream*
	 (dump-symbol name)
	 (dump-symbol 'MACRO))
       (let ((res (assemble-one-lambda lap-function)))
	 (when *clc-fasl-stream*
	   (dump-fop 'lisp::fop-list*-1)
	   (dump-fop 'lisp::fop-fset))
	 (when *compile-to-lisp*
	   (set-symbol-function-carefully name res))))
      (one-shot
       (let ((res (assemble-one-lambda lap-function)))
	 (when *clc-fasl-stream*
	   (dump-fop 'lisp::fop-funcall-for-effect)
	   (dump-byte 0))
	 (when *compile-to-lisp* (funcall res))))
      (t
       (error "Bad function type for Assemble-Function: ~S" lap-function))))
  (when *clc-fasl-stream* (dump-fop 'lisp::fop-verify-empty-stack)))

;;;; Printing LAP files:
;;;
;;;    This code attempts to come up with an output file that is both
;;; human and machine readable, without worrying a great deal about speed.
;;; Unfortunately, human and machine readability conflict, so we have a switch:

(defvar *print-readable-lap* nil
  "If true, lap files will be printed so that they are READ'able, but less
  readable.")


(defparameter lap-indent-step 7)

;;; Indent  --  Internal
;;;
;;;    This is the 10-millionth place where this is defined, but what the hey?
;;;
(defun indent (n stream)
  (terpri stream)
  (write-string "                                                                      "
		stream :end n))
;;;
(defmacro iformat (n &rest args)
  `(progn
    (indent ,n stream)
    (format stream ,@args)))

;;; Start-Lap-Function  --  Internal
;;;
;;;    Write out all the info except the constants and code.
;;;
(defun start-lap-function (fun level idx)
  (let ((stream *clc-lap-stream*)
	(*print-pretty* t)
	(n (* lap-indent-step level)))
    (declare (stream stream))
    (if (zerop level)
	(format stream "~|~%")
	(iformat n "#|~D|#" idx))
	   
    (iformat n "#S(Lap-Function" #|)|#)
    (incf n 4)
    (iformat n "name ~S  type ~A" (lap-function-name fun)
	     (lap-function-type fun))
    (iformat n "min-args ~D  max-args ~D  restp ~S"
	     (lap-function-min-args fun)
	     (lap-function-max-args fun) (lap-function-restp fun))
    (iformat n "entry-points ~A" (lap-function-entry-points fun))
    (iformat n "debug-arglist ~S" (lap-function-debug-arglist fun))
    (iformat n "locals ~D" (lap-function-locals fun))
    (cond (*print-readable-lap*
	   (iformat n "storage-map ~S" (lap-function-storage-map fun))
	   (iformat n "constants"))
	  (t
	   (iformat n "Interesting constants:")))
    (iformat (+ n 2) "(" #|)|#)))

;;; Print-Lap-Constant  --  Internal
;;;
;;;    Print a constant into the lap file.  If *print-readable-lap* is false,
;;; this only prints things which are apt to be truncated in the operand
;;; annotation.  Note that this doesn't get called on lap-function constants
;;; or entry points.
;;;
(defun print-lap-constant (const level idx)
  (let* ((str (let ((*print-pretty* nil))
		(format nil "#|~D|# ~S " idx const)))
	 (len (length str))
	 (n (* (1+ level) lap-indent-step))
	 (stream *clc-lap-stream*)
	 (pos (lisp::charpos stream))
	 (line-len *lap-line-length*))
    (when (or *print-readable-lap*
	      (and (not (symbolp const)) (> len (- line-len n 30))))
      (cond ((> (+ pos len) line-len)
	     (when (> pos n) (indent n stream))
	     (cond ((< len (- line-len n))
		    (write-string str stream))
		   (t
		    (iformat n "#|~D|#" idx )
		    (let ((*print-pretty* t))
		      (iformat n " ~S" const))
		    (indent n stream))))
	    (t
	     (write-string str stream))))))

;;; Finish-Lap-Function  --  Internal
;;;
;;;    Write out the code and stuff for a lap function.
;;;
(defun finish-lap-function (fun level)
  (let* ((n (+ (* level lap-indent-step) 2))
	 (stream *clc-lap-stream*)
	 (line-len *lap-line-length*)
	 (constants (lap-function-constants fun))
	 (const-offset (+ (length (lap-function-entry-points fun))
			  function-header-size-in-words)))
    (write-char #|(|# #\) stream)
    (iformat (+ n 2) "code (" #|)|#)
    (let ((*print-pretty* nil)
	  (*print-length* 20)
	  (*print-level* 3))
      (dolist (inst (lap-function-code fun))
	(cond ((tagp inst)
	       (iformat (- n 2) "~S ; Address: ~D" inst
			(cdr (assq inst assemble-label-list))))
	      (t
	       (indent n stream)
	       (if *print-readable-lap*
		   (prin1 inst stream)
		   (princ inst stream))
	       (when (and (memq (first inst) '(ls l)) (eq (third inst) 'af))
		 (let ((idx (- (ash (fourth inst) -2) const-offset)))
		   (unless (minusp idx)
		     (let* ((str (format nil " ; ~S" (elt constants idx)))
			    (pos (lisp::charpos stream))
			    (nlpos (position #\newline str))
			    (len (or nlpos (length str)))
			    (left (- line-len pos len)))
		       (declare (simple-string str))
		       (cond ((or nlpos (minusp left))
			      (write-string str stream :end
					    (min (max (+ len left) 8) len))
			      (write-string "..." stream))
			     (t
			      (write-string str stream))))))))))
    (write-string #|((|# "))" stream)
    (when (zerop level) (terpri stream)))))

;;; Get-Code-Vector  --  Internal
;;;
;;;    Return a code vector at least Bytes long.
;;;
(defun get-code-vector (bytes)
  (if free-code-vectors
      (let ((vec (pop free-code-vectors)))
	(if (>= (length vec) bytes)
	    vec
	    (make-array (max (* (length vec) 2) bytes)
			:element-type '(unsigned-byte 8))))
      (make-array bytes :element-type '(unsigned-byte 8))))

;;; Fixup-To-Lisp  --  Internal
;;;
;;;    Do the fixups on the code-vector for the function.
;;;
(defun fixup-to-lisp (fun fixups)
  (let ((code (%primitive header-ref fun %function-code-slot)))
    (dolist (fixup fixups)
      (ecase (first fixup)
	     (miscop
	      (%primitive miscop-fixup code (second fixup) (third fixup)))
	     (user-miscop
	      (let* ((miscop-name (third fixup))
		     (offset (second fixup))
		     (loaded-addr (get miscop-name 'lisp::%loaded-address)))
		(cond ((null loaded-addr)
		       (format t "User miscop: ~A has not been loaded,~%  ~
				  function ~A will not run."
			       miscop-name
			       (%primitive header-ref fun
					   %function-name-slot)))
		      (T (let ((addr (logand loaded-addr #xFFFFFF)))
			   (declare (fixnum addr))
			   (setf (aref code (+ offset 1))
				 (logand (ash addr -16) #xFF))
			   (setf (aref code (+ offset 2))
				 (logand (ash addr -8) #xFF))
			   (setf (aref code (+ offset 3))
				 (logand loaded-addr #xFF)))))))
	     (load-link
	      (let* ((lte (third fixup))
		     (symbol (second lte))
		     (nargs (third lte))
		     (flag (fourth lte)))
		(%primitive link-address-fixup symbol
			    (if flag (logior nargs #x8000) nargs)
			    code (second fixup))))))))
 
;;; Assemble-One-Lambda  --  Internal
;;;
;;;    Assemble the supplied Lap-Function and dump output to the appropriate
;;; streams.  If *Compile-To-Lisp* is true then a function object is returned
;;; as well.
;;;
(defun assemble-one-lambda (lap-function &optional (depth 0) index)
  (let* ((ep (lap-function-entry-points lap-function))
	 (constants (append ep (lap-function-constants lap-function)))
	 (num-consts (length constants))
	 (code (lap-function-code lap-function))
	 assemble-label-list
	 romp-code-size)
    (do ()
	((not (change-branches-to-jumps code))))
    (change-load-return code)
    (let* ((romp-code-vector (get-code-vector romp-code-size))
	   (fixups (translate code)))
      ;;
      ;; If there is a lap file, write out the header...
      (if *clc-lap-stream* (start-lap-function lap-function depth index))
      ;;
      ;; If there is a fasl file, start the function definition by
      ;; pushing the standard stuff that comes before the constants.
      (if *clc-fasl-stream* (start-fasl-function lap-function))
      ;;
      ;; Loop over the constants, setting function slots and pushing
      ;; on the stack, as appropriate.  We special-case the magical
      ;; Lap-Function and **tag** constants.
      (let ((fun (if *compile-to-lisp*
		     (create-function lap-function num-consts)))
	    (idx %function-constants-offset)
	    (jdx (- (+ (length ep) %function-constants-offset))))
	
	(dolist (const constants)
	  (cond ((lap-function-p const)
		 (let ((res (assemble-one-lambda const (1+ depth) idx)))
		   (when *compile-to-lisp*
		     (%primitive header-set fun idx res))))
		((or (atom const) (not (eq (car const) '**tag**)))
		 (when (eq const make-const-lexical-slot)
		   (setq const 0))
		 (when (and *clc-lap-stream* (>= jdx 0))
		   (print-lap-constant const depth jdx))
		 (when *clc-fasl-stream* (dump-object const))
		 (when *compile-to-lisp*
		   (%primitive header-set fun idx const)))
		(t
		 (let ((pc (+ (cdr (assoc (cadr const) assemble-label-list))
			      i-vector-header-size)))
		   (when (and *clc-lap-stream* (>= jdx 0))
		     (print-lap-constant const depth jdx))
		   (when *clc-fasl-stream* (dump-integer pc))
		   (when *compile-to-lisp*
		     (%primitive header-set fun idx pc)))))
	  (incf idx)
	  (incf jdx))
	;;
	;; If dumping to a fasl file, emit the stuff to cons up the function
	;; out of the constants we have pushed on the stack.
	(if *clc-fasl-stream*
	    (finish-fasl-function lap-function num-consts fixups))
	(push romp-code-vector free-code-vectors)
	;;
	;; Dump lap code...
	(if *clc-lap-stream* (finish-lap-function lap-function depth))
	;;
	;; Return the result, macroifying it if appropriate.
	(when *compile-to-lisp*
	  (fixup-to-lisp fun fixups)
	  (if (eq (lap-function-type lap-function) 'macro)
	      (cons 'macro fun)
	      fun))))))

;;; Make-Arg-Names  --  Internal
;;;
;;;    Takes the list representation of the debug arglist and turns it into a
;;; string.
;;;
(defun make-arg-names (x)
  (let ((args (lap-function-debug-arglist x)))
    (if (null args)
	"()"
	(let ((*print-pretty* t)
	      (*print-escape* nil)
	      (*print-case* :downcase))
	  (write-to-string args)))))
  
;;; Create-Function  --  Internal
;;;
;;;    Cons up a function object for the function currently being assembled.
;;; Leaves the constants to be filled in by Assemble-One-Lambda.
;;;
(defun create-function (lap-function num-consts)
  (let* ((fun-length (+ %function-constants-offset num-consts))
	 (function (%primitive alloc-function fun-length))
	 (code (%primitive alloc-i-vector romp-code-size 3)))
    (%primitive set-vector-subtype code %i-vector-code-subtype)
    (%primitive set-vector-subtype function
		(lap-function-subtype lap-function))
    (%primitive header-set function %function-name-slot
		(lap-function-name lap-function))
    (%primitive header-set function %function-code-slot code)
    (%primitive header-set function %function-min-args-slot
		(lap-function-arg-info lap-function))
    (%primitive header-set function %function-defined-from-slot
		*current-defined-from*)
    (%primitive header-set function %function-arg-names-slot
		(make-arg-names lap-function))

    (replace code romp-code-vector :end2 romp-code-size)
    function))

;;; Lap-Function-Arg-Info  --  Internal
;;;
;;;    Returns the magical argument info corresponding to Lap-Function.
;;;
(defun lap-function-arg-info (lap-function)
  (let ((res (dpb (lap-function-locals lap-function) %function-locals-byte
		  (dpb (lap-function-min-args lap-function)
		       %function-min-args-byte
		       (dpb (lap-function-max-args lap-function)
			    %function-max-args-byte
			    0)))))
     (if (lap-function-restp lap-function)
	 (%primitive logdpb 1 (byte-size %function-rest-arg-byte)
		     (byte-position %function-rest-arg-byte)
		     res)
	 res)))

;;; Lap-Function-Subtype  --  Internal
;;;
;;;    Returns the function object subtype corresponding to the type of
;;; Lap-Function.
;;;
(defun lap-function-subtype (lap-function)
  (ecase (lap-function-type lap-function)
    (expr %function-expr-subtype)
    (fexpr %function-fexpr-subtype)
    (macro %function-macro-subtype)
    (break-off %function-anonymous-expr-subtype)
    (one-shot %function-top-level-form-subtype)))

;;;; Shorten branches and assign addresses to labels.

(defun change-branches-to-jumps (code)
  (declare (optimize (speed 3) (safety 0)))
  (let ((label-list (assign-label-values code)))
    (do* ((inst-ptr code (cdr inst-ptr))
	  (inst (car inst-ptr) (car inst-ptr))
	  (repeat NIL)
	  (pc 0))
	 ((null inst-ptr)
	  repeat)
      (declare (fixnum pc) (list inst-ptr))
      (cond ((listp inst)
	     (cond ((memq (opcode inst) '(bb bnb))
		    (let ((label (cdr (assq (branch-tag inst) label-list))))
		      (declare (fixnum label))
		      (cond ((and (fixnump label)
				  (< (the fixnum
					  (abs (the fixnum
						    (- pc label)))) 128))
			     (setq repeat t)
			     (rplaca inst-ptr
				     `(,(if (eq (opcode inst) 'bb) 'jb 'jnb)
				       ,@(cdr inst))))))))
	     (setq PC (the fixnum
			   (+ PC (the fixnum
				      (romp-instruction-length
				       (opcode (car inst-ptr))))))))))))

(defun assign-label-values (code)
  (declare (optimize (speed 3) (safety 0)))
  (do* ((inst-ptr code (cdr inst-ptr))
	(inst (car inst-ptr) (car inst-ptr))
	(pc 0)
	(label-list NIL))
       ((null inst-ptr)
	(setq assemble-label-list label-list)
	(setq Romp-Code-Size pc)
	label-list)
    (declare (fixnum pc))
    (if (atom inst)
	(push (cons inst pc) label-list)
	(setq  pc
	       (the fixnum
		    (+ pc (the fixnum
			       (romp-instruction-length (opcode inst)))))))))

(defun romp-instruction-length (opcode)
  (cond ((eq opcode 'load-link) 8)
	((memq opcode '(miscop miscopx user-miscop user-miscopx)) 4)
	((eq opcode 'load-return) 8)
	(T (case (get opcode 'romp-instruction-type)
	     (ji 2)
	     (x 2)
	     (ds 2)
	     (r 2)
	     (bi 4)
	     (ba 4)
	     (d 4)
	     (T (clc-error "Unknown opcode: ~A." opcode) 0)))))


;;; Fix up load-return pseudos.

(defun change-load-return (code)
  (declare (optimize (speed 3) (safety 0)))
  (assign-label-values code)
  (do* ((inst-ptr (cdr code) (cdr inst-ptr))
	(inst (car inst-ptr) (car inst-ptr))
	(pc-diff 0)
	(label-list assemble-label-list))
       ((null inst-ptr)
	(assign-label-values code))
    (declare (fixnum pc-diff))
    (cond ((and (listp inst) (eq (opcode inst) 'load-return))
	   (let* ((label (operand1 inst))
		  (offset (- (cdr (assoc (cadr label) label-list)) pc-diff)))
	     (declare (fixnum offset))
	     (cond ((<= offset romp-max-immed-number)
		    (rplaca inst-ptr `(cal PC 0 ,label))
		    (setq pc-diff (the fixnum (+ pc-diff 4))))
		   ((= (the fixnum (ash offset -16)) 0)
		    (setq pc-diff (the fixnum (+ pc-diff 2)))
		    (rplaca inst-ptr `(lis pc ,label))
		    (rplacd inst-ptr (cons `(oil pc pc ,label)
					   (cdr inst-ptr))))
		   (T (rplaca inst-ptr `(cau pc 0 ,label))
		      (rplacd inst-ptr (cons `(oil pc pc ,label)
					     (cdr inst-ptr))))))))))


;;; Translate translates the assembler code into actual bits.  The list
;;; of loader-fixups is returned.

(defvar Assemble-PC)
(defvar Assemble-Fixup-List NIL)

(defun translate (code)
  (declare (optimize (speed 3) (safety 0)))
  (do* ((Rest code (cdr Rest))
	(Instruction (car Rest) (car Rest))
	(Assemble-PC 0)
	(Assemble-Fixup-List NIL))
       ((null Rest)
	assemble-fixup-list)
    (declare (fixnum assemble-pc))
    (cond ((atom Instruction))
	  ((eq (car Instruction) 'load-link)
	   (push `(load-link ,assemble-pc ,(caddr instruction))
		 assemble-fixup-list)
	   (process-d-instruction `(cau ,(cadr Instruction) 0 0))
	   (process-d-instruction `(oil ,(cadr Instruction)
					,(cadr Instruction) 0)))
	  ((memq (car Instruction) '(miscop miscopx))
	   (let ((index (get (cadr Instruction) 'Transfer-Vector-Index)))
	     (cond (index (push `(miscop ,assemble-pc ,index)
				Assemble-Fixup-List)
			  (process-ba-instruction (if (eq (car Instruction)
							  'miscop)
						      '(bala 0)
						      '(balax 0))))
		   (T (clc-error "Unknown primitive: ~A.~%"
				 (cadr Instruction))))))
	  ((memq (car Instruction) '(user-miscop user-miscopx))
	   (push `(user-miscop ,assemble-pc ,(cadr Instruction))
		 Assemble-Fixup-List)
	   (process-ba-instruction  (if (eq (car Instruction) 'user-miscop)
					'(bala 0)
					'(balax 0))))
	  (T (case (Get-Instruction-Type (car Instruction))
	       (JI (Process-JI-Instruction Instruction))
	       (X (Process-X-Instruction Instruction))
	       (DS (Process-DS-Instruction Instruction))
	       (R (Process-R-Instruction Instruction))
	       (BI (Process-BI-Instruction Instruction))
	       (BA (Process-BA-Instruction Instruction))
	       (D (Process-D-Instruction Instruction))
	       (T (clc-error "Undefined op-code in ~A.~%" Instruction)))))))

(defun Process-JI-Instruction (Instruction)
  (declare (optimize (speed 3) (safety 0)))
  (let ((I-Length (length Instruction))
	(operand (get-label-value (caddr Instruction) assemble-pc)))
    (declare (fixnum operand i-length))
    (cond ((< I-Length 3)
	   (clc-error "Too few operands (~D) in ~A for JI instruction format.~%"
		      I-Length Instruction))
	  ((> I-Length 3)
	   (clc-error "Too many operands (~D) in ~A for JI instruction format.~%"
		      I-Length Instruction))
	  (T (setf (aref romp-code-vector Assemble-PC)
		   (the fixnum
			(+ (the fixnum
				(ash (the fixnum (logand (the fixnum
							      (Get-Operation-Code (car Instruction)))
					     #x1F)) 3))
			   (logand (the fixnum
					(- (the fixnum
						(Get-Condition-Code (cadr Instruction)))
					   8)) #x7))))
	     (setf (aref romp-code-vector (the fixnum (1+ (the fixnum assemble-pc))))
		   (logand operand #xFF))))
    (setq Assemble-PC (the fixnum (+ Assemble-PC 2)))))

(defun Process-X-Instruction (Instruction)
  (declare (optimize (speed 3) (safety 0)))
  (let ((I-Length (length Instruction)))
    (declare (fixnum i-length))
    (cond ((< I-Length 4)
	   (clc-error "Too few operands (~D) in ~A for X instruction format.~%"
		      I-Length Instruction))
	  ((> I-Length 4)
	   (clc-error "Too many operands (~D) in ~A for X instruction format.~%"
		      I-Length Instruction))
	  (T (setf (aref romp-code-vector Assemble-PC)
		   (the fixnum
			(+ (the fixnum
				(ash (the fixnum
					  (logand (the fixnum
						       (Get-Operation-Code
							(car Instruction))) #xF)) 4))
			   (the fixnum
				(logand (the fixnum
					     (eval-register (cadr Instruction))) #xF)))))
	     (setf (aref romp-code-vector (the fixnum (1+ assemble-pc)))
		   (the fixnum
			(+ (the fixnum
				(ash (the fixnum
					  (logand (the fixnum
						       (eval-register
							(caddr Instruction))) #xF)) 4))
			   (the fixnum
				(logand (the fixnum
					     (eval-register
					      (cadddr Instruction))) #xF)))))))
    (setq Assemble-PC (the fixnum (+ (the fixnum Assemble-PC) 2)))))

(defun Process-DS-Instruction (Instruction)
  (declare (optimize (speed 3) (safety 0)))
  (let ((I-Length (length Instruction)))
    (declare (fixnum i-length))
    (cond ((< I-Length 4)
	   (clc-error "Too few operands (~D) in ~A for DS instruction format.~%"
		      I-Length Instruction))
	  ((> I-Length 4)
	   (clc-error "Too many operands (~D) in ~A for DS instruction format.~%"
		      I-Length Instruction))
	  (T (setf (aref romp-code-vector Assemble-PC)
		   (the fixnum (+ (the fixnum
				       (ash (the fixnum
						 (logand (the fixnum
							      (Get-Operation-Code
							       (car Instruction))) #xF)) 4))
				  (the fixnum
				       (logand (the fixnum
						    (Get-Immediate-Value (car Instruction)
									 (cadddr Instruction)))
					       #xF)))))
	     (setf (aref romp-code-vector (the fixnum (1+ assemble-pc)))
		   (the fixnum
			(+ (the fixnum
				(ash (the fixnum
					  (logand (the fixnum
						       (eval-register
							(cadr Instruction))) #xF)) 4))
			   (logand (the fixnum (eval-register (caddr Instruction)))))))))
    (setq Assemble-PC (the fixnum (+ (the fixnum Assemble-PC) 2)))))

(defun Process-R-Instruction (Instruction)
  (let ((I-Length (length Instruction)))
    (cond ((< I-Length 3)
	   (clc-error "Too few operands (~D) in ~A for R instruction format.~%"
		      I-Length Instruction))
	  ((> I-Length 3)
	   (clc-error "Too many operands (~D) in ~A for R instruction format.~%"
		      I-Length Instruction))
	  (T (setf (aref romp-code-vector Assemble-PC)
		   (logand (Get-Operation-Code (car Instruction)) #xFF))
	     (setf (aref romp-code-vector (1+ assemble-pc))
		   (+ (ash (logand (if (memq (car instruction)
					     '(bbr bnbr bbrx bnbrx))
				       (get-condition-code (cadr instruction))
				       (eval-register (cadr Instruction)))
				   #xF) 4)
		      (if (and (eq (car Instruction) 'lis)
			       (consp (caddr Instruction))
			       (eq (caaddr Instruction) '*return-pc*))
			  (get-lis-label-value (caddr instruction))
			  (logand (eval-register (caddr Instruction)) #xF))))))
    (setq Assemble-PC (+ Assemble-PC 2))))

(defun Process-BI-Instruction (Instruction)
  (let ((I-Length (length Instruction))
	(operand (get-label-value (caddr Instruction) assemble-pc)))
    (cond ((< I-Length 3)
	   (clc-error "Too few operands (~D) in ~A for BI instruction format.~%"
		      I-Length Instruction))
	  ((> I-Length 3)
	   (clc-error "Too many operands (~D) in ~A for BI instruction format.~%"
		      I-Length Instruction))
	  (T (setf (aref romp-code-vector assemble-pc)
		   (logand (get-operation-code (car Instruction)) #xFF))
	     (setf (aref romp-code-vector (1+ assemble-pc))
		   (+ (ash (get-condition-code (cadr Instruction)) 4)
		      (logand (ash operand -16) #xF)))
	     (setf (aref romp-code-vector (+ assemble-pc 2))
		   (logand (ash operand -8) #xFF))
	     (setf (aref romp-code-vector (+ assemble-pc 3))
		   (logand operand #xFF))))
    (setq assemble-pc (+ assemble-pc 4))))

(defun Process-BA-Instruction (Instruction)
  (let ((I-Length (length Instruction))
	(operand (get-ba-operand (cadr Instruction))))
    (cond ((< I-Length 2)
	   (clc-error "Too few operands (~D) in ~A for BA instruction format.~%"
		      I-Length Instruction))
	  ((> I-Length 2)
	   (clc-error "Too many operands (~D) in ~A for BA instruction format.~%"
		      I-Length Instruction))
	  (T (setf (aref romp-code-vector Assemble-PC)
		   (logand (get-operation-code (car Instruction)) #xFF))
	     (setf (aref romp-code-vector (1+ assemble-pc))
		   (logand (ash operand -16) #xFF))
	     (setf (aref romp-code-vector (+ assemble-pc 2))
		   (logand (ash operand -8) #xFF))
	     (setf (aref romp-code-vector (+ assemble-pc 3))
		   (logand operand #xFF))))
    (setq assemble-pc (+ assemble-pc 4))))

(defun Process-D-Instruction (Instruction)
  (if (memq (car Instruction) '(ci cli))
      (setq instruction
	    `(,(car Instruction) 0 ,(cadr Instruction) ,(caddr Instruction))))
  (let ((I-Length (length Instruction))
	(operand (get-d-operand (car Instruction) (cadddr Instruction))))
    (cond ((< I-Length 4)
	   (clc-error "Too few operands (~D) in ~A for D instruction format.~%"
		      I-Length Instruction))
	  ((> I-Length 4)
	   (clc-error "Too many operands (~D) in ~A for D instruction format.~%"
		      I-Length Instruction))
	  (T (setf (aref romp-code-vector assemble-pc)
		   (logand (get-operation-code (car Instruction)) #xFF))
	     (setf (aref romp-code-vector (1+ assemble-pc))
		   (+ (ash (logand (eval-register (cadr Instruction)) #xF) 4)
		      (logand (eval-register (caddr Instruction)))))
	     (setf (aref romp-code-vector (+ assemble-pc 2))
		   (logand (ash operand -8) #xFF))
	     (setf (aref romp-code-vector (+ assemble-pc 3))
		   (logand operand #xFF))))
    (setq assemble-pc (+ assemble-pc 4))))


(defun get-label-value (label pc)
  (declare (fixnum pc) (optimize (speed 3) (safety 0)))
  (let ((offset (cdr (assoc (cadr label) assemble-label-list))))
    (declare (fixnum offset))
    (the fixnum (ash (the fixnum (- offset pc)) -1))))

(defun Get-Immediate-Value (Op-Code Value)
  (declare (fixnum value))
  (cond ((not (fixnump Value))
	 (clc-error "Immediate field (~A) for op-code ~A should be a fixnum.~%"
		Value Op-Code)
	 0)
	(T (case Op-Code
	     ((lcs stcs) Value)
	     ((lhas lhs sths) (ash Value -1))
	     ((ls sts) (ash Value -2))
	     (T Value)))))

(defun Get-D-Operand (Instruction Operand)
  (cond ((and (consp Operand) (or (eq (car operand) '*return-pc*)
				  (eq (car operand) '**tag**)))
	 (let ((offset (+ (cdr (assoc (cadr operand) assemble-label-list))
			  I-Vector-header-size)))
	   (cond ((eq Instruction 'cal)
		  (logand offset #x7FFF))
		 ((eq Instruction 'cau)
		  (logand (ash offset -16) #xFFFF))
		 (T (logand offset #xFFFF)))))
	((fixnump Operand) Operand)
	(T (clc-error "Illegal operand: ~A to D format instruction.~%" Operand))))

(defun Get-BA-Operand (Operand &aux Index)
  (cond ((and (consp Operand) (eq (car Operand) 'miscop))
	 (cond ((null (setq Index (Get (cadr Operand) 'Transfer-Vector-Index)))
		(clc-error "Unknow primitive: ~A.  Ignoring.~%" Operand)
		(setq index -1)))
	 
	 (push `(miscop ,assemble-pc ,index) assemble-fixup-list)
	 Index)
	((fixnump Operand) Operand)
	(T (clc-error "Illegal operand: ~A to BA format instruction.~%"
		      Operand))))
