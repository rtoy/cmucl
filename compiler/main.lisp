;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the top-level interfaces to the compiler.
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(proclaim '(special *constants* *free-variables* *compile-component*
		    *code-vector* *next-location* *result-fixups*
		    *free-functions* *source-paths* *failed-optimizations*
		    *seen-blocks* *seen-functions* *list-conflicts-table*
		    *continuation-number* *continuation-numbers*
		    *number-continuations* *tn-id* *tn-ids* *id-tns*
		    *label-ids* *label-id* *id-labels* *sb-list*
		    *unknown-functions* *compiler-error-count*
		    *compiler-warning-count* *compiler-note-count*
		    *compiler-error-output* *compiler-error-bailout*
		    *compiler-trace-output*
		    *last-source-context* *last-original-source*
		    *last-source-form* *last-format-string* *last-format-args*
		    *last-message-count* *fenv*))

(defparameter compiler-version "0.0")

(defvar *check-consistency* t)
(defvar *all-components*)

;;; The value of the :Block-Compile argument that Compile-File was called with.
;;;
(defvar *block-compile*)

;;; When block compiling, used by PROCESS-FORM to accumulate top-level lambdas
;;; resulting from compiling subforms.  (In reverse order.)
;;;
(defvar *top-level-lambdas*)

;;; Used to control compiler verbosity.
;;;
(defvar *compile-verbose* nil)

;;; The values of *Package* and *Default-Cookie* when compilation started.
;;;
(defvar *initial-package*)
(defvar *initial-cookie*)

;;; The source-info structure for the current compilation.  This is null
;;; globally to indicate that we aren't currently in any identifiable
;;; compilation.
;;;
(defvar *source-info* nil)


;;; Maybe-Mumble  --  Internal
;;;
;;;    Mumble conditional on *Compile-Verbose*.
;;;
(defun maybe-mumble (&rest foo)
  (when *compile-verbose*
    (apply #'compiler-mumble foo)))

(deftype object () '(or fasl-file core-object null))


;;;; Component compilation:

(defparameter max-optimize-iterations 3
  "The upper limit on the number of times that we will consecutively do IR1
  optimization that doesn't introduce any new code.  A finite limit is
  necessary, since type inference may take arbitrarily long to converge.")

(defevent ir1-optimize-until-done "IR1-OPTIMIZE-UNTIL-DONE called.")
(defevent ir1-optimize-maxed-out "Hit MAX-OPTIMIZE-ITERATIONS limit.")

;;; IR1-Optimize-Until-Done  --  Internal
;;;
;;;    Repeatedly optimize Component until no further optimizations can be
;;; found or we hit our iteration limit.  When we hit the limit, we clear the
;;; component and block REOPTIMIZE flags to discourage following the next
;;; optimization attempt from pounding on the same code.
;;;
(defun ir1-optimize-until-done (component)
  (declare (type component component))
  (maybe-mumble "Opt")
  (event ir1-optimize-until-done)
  (let ((count 0)
	(cleared-reanalyze nil))
    (loop
      (when (component-reanalyze component)
	(setq count 0)
	(setq cleared-reanalyze t)
	(setf (component-reanalyze component) nil))
      (setf (component-reoptimize component) nil)
      (ir1-optimize component)
      (unless (component-reoptimize component)
	(maybe-mumble " ")
	(return))
      (incf count)
      (when (= count max-optimize-iterations)
	(event ir1-optimize-maxed-out)
	(maybe-mumble "* ")
	(setf (component-reoptimize component) nil)
	(do-blocks (block component)
	  (setf (block-reoptimize block) nil))
	(return))
      (maybe-mumble "."))
    (when cleared-reanalyze
      (setf (component-reanalyze component) t)))
  (undefined-value))

(defvar *constraint-propagate* t)

;;; IR1-Phases  --  Internal
;;;
;;;    Do all the IR1 phases for a non-top-level component.
;;;
(defun ir1-phases (component)
  (declare (type component component))
  (let ((*constraint-number* 0))
    (declare (special *constraint-number*))
    (tagbody
     TOP
      (ir1-optimize-until-done component)
      (when (component-reanalyze component) (go DFO))
      (when *constraint-propagate*
	(maybe-mumble "Constraint ")
	(constraint-propagate component))
      (maybe-mumble "Type ")
      (generate-type-checks component)
      (unless (or (component-reoptimize component)
		  (component-reanalyze component))
	(go DONE))
      
      (go TOP)
     DFO
      (maybe-mumble "DFO ")
      (find-dfo component)
      (go TOP)
     DONE)
    (undefined-value)))


;;; Compile-Component  --  Internal
;;;
(defun compile-component (component object)
  (compiler-mumble "Compiling ~A: " (component-name component))
  
  (ir1-phases component)
  
  #|
  (maybe-mumble "Dom ")
  (find-dominators component)
  (maybe-mumble "Loop ")
  (loop-analyze component)
  |#

  (let ((*compile-component* component))
    (maybe-mumble "Env ")
    (environment-analyze component)
    (maybe-mumble "GTN ")
    (gtn-analyze component)
    (maybe-mumble "Control ")
    (control-analyze component)
    (maybe-mumble "LTN ")
    (ltn-analyze component)

    (when (ir2-component-values-receivers (component-info component))
      (maybe-mumble "Stack ")
      (stack-analyze component))

    (when (component-reanalyze component)
      (find-dfo component))

    (maybe-mumble "IR2Tran ")
    (init-assembler)
    (entry-analyze component)
    (ir2-convert component)

    (when *check-consistency*
      (maybe-mumble "Check2 ")
      (check-ir2-consistency component))
    
    (maybe-mumble "Life ")
    (lifetime-analyze component)

    (when *compile-verbose*
      (compiler-mumble "") ; Sync before doing random output.
      (pre-pack-tn-stats component *compiler-error-output*))

    (delete-unreferenced-tns component)

    (when *check-consistency*
      (maybe-mumble "CheckL ")
      (check-life-consistency component))

    (maybe-mumble "Pack ")
    (pack component)

    (when *compiler-trace-output*
      (describe-component component *compiler-trace-output*))
    
    (maybe-mumble "Code ")
    (generate-code component))

  (etypecase object
    (fasl-file
     (maybe-mumble "FASL")
     (fasl-dump-component component *code-vector* *next-location*
			  *assembler-nodes* (1+ *current-assembler-node*)
			  *result-fixups* object))
    (core-object
     (maybe-mumble "Core"
     (make-core-component component *code-vector* *next-location*
			  *assembler-nodes* (1+ *current-assembler-node*)
			  *result-fixups* object)))
    (null))

  (compiler-mumble "~%")
  (undefined-value))


;;;; Clearing global data structures:

;;; CLEAR-IR2-INFO  --  Internal
;;;
;;;    Clear all the INFO slots in sight in Component to allow the IR2 data
;;; structures to be reclaimed.  We also clear the INFO in constants in the
;;; *FREE-VARIABLES*, etc.  The latter is required for correct assignment of
;;; costant TNs, in addition to allowing stuff to be reclaimed.
;;;
;;;    We don't clear the FUNCTIONAL-INFO slots, since they are used to keep
;;; track of functions across component boundaries.
;;;
(defun clear-ir2-info (component)
  (declare (type component component))
  (setf (component-info component) nil)

  (do-blocks (block component)
    (setf (block-info block) nil)
    (do-nodes (node cont block)
      (setf (continuation-info cont) nil)
      (when (basic-combination-p node)
	(setf (basic-combination-info node) nil))))

  (dolist (fun (component-lambdas component))
    (let ((tails (lambda-tail-set fun)))
      (when tails
	(setf (tail-set-info tails) nil)))
    (let ((env (lambda-environment fun)))
      (setf (environment-info env) nil)
      (dolist (nlx (environment-nlx-info env))
	(setf (nlx-info-info nlx) nil)))
    (dolist (var (lambda-vars fun))
      (setf (leaf-info var) nil)))

  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (setf (leaf-info v) nil))
	   *constants*)

  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (when (constant-p v)
		 (setf (leaf-info v) nil)))
	   *free-variables*)

  (undefined-value))


;;;
(defun clear-stuff ()
  ;;
  ;; Clear global tables.
  (clrhash *free-functions*)
  (clrhash *free-variables*)
  (clrhash *constants*)
  (clrhash *source-paths*)
  (clrhash *failed-optimizations*)
  ;;
  ;; Clear debug counters and tables.
  (clrhash *seen-blocks*)
  (clrhash *seen-functions*)
  (clrhash *list-conflicts-table*)
  (clrhash *continuation-numbers*)
  (clrhash *number-continuations*)
  (setq *continuation-number* 0)
  (clrhash *tn-ids*)
  (clrhash *id-tns*)
  (setq *tn-id* 0)
  (clrhash *label-ids*)
  (clrhash *id-labels*)
  (setq *label-id* 0)
  ;;
  ;; Clear some Pack data structures (for GC purposes only.)
  (dolist (sb *sb-list*)
    (when (finite-sb-p sb)
      (fill (finite-sb-live-tns sb) nil)))
  ;;
  ;; Reset Gensym.
  (setq lisp:*gensym-counter* 0))


;;; PRINT-SUMMARY  --  Interface
;;;
;;;    This function is called by WITH-COMPILATION-UNIT at the end of a
;;; compilation unit.  It prints out any residual unknown function warnings and
;;; the total error counts.  Abort-P should be true when the compilation unit
;;; was aborted by throwing out.  Abort-Count is the number of dynamically
;;; enclosed nested compilation units that were aborted.
;;;
(defun print-summary (abort-p abort-count)
  (unless abort-p
    (let ((funs (sort *unknown-functions* #'string<
		      :key #'(lambda (x)
			       (let ((x (unknown-function-name x)))
				 (if (symbolp x)
				     (symbol-name x)
				     (symbol-name (cadr x))))))))
      (dolist (fun funs)
	(let ((name (unknown-function-name fun))
	      (warnings (unknown-function-warnings fun))
	      (count (unknown-function-count fun)))
	  (dolist (*compiler-error-context* warnings)
	    (compiler-warning "Call to unknown function."))
	  
	  (let ((warn-count (length warnings)))
	    (when (> count warn-count)
	      (let ((more (- count warn-count)))
		(compiler-warning
		 "~D ~:[~;more ~]call~P to unknown function ~S."
		 more warnings more name))))))))

  (compiler-mumble
   "~2&Compilation unit ~:[finished~;aborted~].~
    ~[~:;~:*~&  ~D fatal error~:P~]~
    ~[~:;~:*~&  ~D error~:P~]~
    ~[~:;~:*~&  ~D warning~:P~]~
    ~[~:;~:*~&  ~D note~:P~]~2%"
   abort-p
   abort-count
   *compiler-error-count*
   *compiler-warning-count*
   *compiler-note-count*))
   
   
;;; Describe-Component  --  Internal
;;;
;;;    Print out some useful info about Component to Stream.
;;;
(defun describe-component (component &optional
				     (*standard-output* *standard-output*))
  (declare (type component component) (type stream stream))
  (format t "~|~%;;;; Component: ~S~2%" (component-name component))
  (print-blocks component)
  
  (format t "~%~|~%;;;; IR2 component: ~S~2%" (component-name component))
  
  (format t "Entries:~%")
  (dolist (entry (ir2-component-entries (component-info component)))
    (format t "~4TL~D: ~S~:[~; [Closure]~]~%"
	    (label-id (entry-info-offset entry))
	    (entry-info-name entry)
	    (entry-info-closure-p entry)))
  
  (terpri)
  (pre-pack-tn-stats component *standard-output*)
  (terpri)
  (print-ir2-blocks component)
  (terpri)
  
  (undefined-value))


;;; Compile-Top-Level  --  Internal
;;;
;;;    Compile Lambdas (a list of the lambdas for top-level forms) into the
;;; Object file.
;;;
(defun compile-top-level (lambdas object)
  (declare (list lambdas) (type object object))
  (maybe-mumble "Local call analyze")
  (dolist (lambda lambdas)
    (let* ((component (block-component (node-block (lambda-bind lambda))))
	   (*all-components* (list component)))
      (local-call-analyze component)))
  (maybe-mumble ".~%")
  
  (maybe-mumble "Find components")
  (let* ((components (find-initial-dfo lambdas))
	 (*all-components* components))
    
    (when *check-consistency*
      (maybe-mumble "[Check]~%")
      (check-ir1-consistency components))
    
    (dolist (component components)
      (compile-component component object)
      (clear-ir2-info component))

    (etypecase object
      (fasl-file
       (dolist (lambda lambdas)
	 (fasl-dump-top-level-lambda-call lambda object)))
      (core-object
       (dolist (lambda lambdas)
	 (core-call-top-level-lambda lambda object)))
      (null))
    
    (when *check-consistency*
      (maybe-mumble "[Check]~%")
      (check-ir1-consistency components))
    
    (ir1-finalize)
    (undefined-value)))


;;;; File reading:
;;;
;;;    When reading from a file, we have to keep track of some source
;;; information.  We also exploit our ability to back up for printing the error
;;; context and for recovering from errors.
;;;
;;; The interface we provide to this stuff is the stream-oid Source-Info
;;; structure.  The bookkeeping is done as a side-effect of getting the next
;;; source form.


;;; The File-Info structure holds all the source information for a given file.
;;;
(defstruct file-info
  ;;
  ;; If a file, the truename of the corresponding source file.  If from a Lisp
  ;; form, :LISP, if from a stream, :STREAM.
  (name nil :type (or pathname (member :lisp :stream)))
  ;;
  ;; The file's write date (if relevant.)
  (write-date nil :type (or unsigned-byte null))
  ;;
  ;; The source path root number of the first form in this file (i.e. the
  ;; total number of forms converted previously in this compilation.)
  (source-root 0 :type unsigned-byte)
  ;;
  ;; Parallel vectors containing the forms read out of the file and the file
  ;; positions that reading of each form started at (i.e. the end of the
  ;; previous form.)
  (forms (make-array 10 :fill-pointer 0 :adjustable t) (vector t))
  (positions (make-array 10 :fill-pointer 0 :adjustable t) (vector t)))


;;; The Source-Info structure provides a handle on all the source information
;;; for an entire compilation.
;;;
(defstruct (source-info
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (format stream "#<Source-Info>"))))
  ;;
  ;; The UT that compilation started at.
  (start-time (get-universal-time) :type unsigned-byte)
  ;;
  ;; A list of the file-info structures for this compilation.
  (files nil :type list)
  ;;
  ;; The tail of the Files for the file we are currently reading.
  (current-file nil :type list)
  ;;
  ;; The stream that we are using to read the Current-File.  Null if no stream
  ;; has been opened yet.
  (stream nil :type (or stream null)))


;;; Make-File-Source-Info  --  Internal
;;;
;;;    Given a list of pathnames, return a Source-Info structure.
;;;
(defun make-file-source-info (files)
  (declare (list files))
  (let ((file-info
	 (mapcar #'(lambda (x)
		     (make-file-info :name x
				     :write-date (file-write-date x)))
		 files)))

    (make-source-info :files file-info
		      :current-file file-info)))


;;; MAKE-LISP-SOURCE-INFO  --  Interface
;;;
;;;    Return a SOURCE-INFO to describe the incremental compilation of Form.
;;; Also used by EVAL:INTERNAL-EVAL.
;;;
(defun make-lisp-source-info (form)
  (make-source-info
   :start-time (get-universal-time)
   :files (list (make-file-info :name :lisp
				:forms (vector form)
				:positions '#(0)))))


;;; MAKE-STREAM-SOURCE-INFO  --  Internal
;;;
;;;    Return a SOURCE-INFO which will read from Stream.
;;;
(defun make-stream-source-info (stream)
  (let ((files (list (make-file-info :name :stream))))
    (make-source-info
     :files files
     :current-file files
     :stream stream)))


;;; Normal-Read-Error  --  Internal
;;;
;;;    Print an error message for a non-EOF error on Stream.  Old-Pos is a
;;; preceding file position that hopefully comes before the beginning of the
;;; line.  Of course, this only works on streams that support the file-position
;;; operation.
;;;
(defun normal-read-error (stream old-pos condition)
  (declare (type stream stream) (type unsigned-byte old-pos pos))
  (let ((pos (file-position stream)))
    (file-position stream old-pos)
    (let ((start old-pos))
      (loop
	(let ((line (read-line stream nil))
	      (end (file-position stream)))
	  (when (>= end pos)
	    (compiler-error-message
	     "Read error at ~D:~% \"~A/\\~A\"~%~A"
	     pos
	     (string-left-trim " 	"
			       (subseq line 0 (- pos start)))
	     (subseq line (- pos start))
	     condition)
	    (return))
	  (setq start end)))))
  (undefined-value))


;;; Ignore-Error-Form  --  Internal
;;;
;;;    Back Stream up to the position Pos, then read a form with
;;; *Read-Suppress* on, discarding the result.  If an error happens during this
;;; read, then bail out using Compiler-Error (fatal in this context).
;;;
(defun ignore-error-form (stream pos)
  (declare (type stream stream) (type unsigned-byte pos))
  (file-position stream pos)
  (handler-case (let ((*read-suppress* t))
		  (read stream))
    (error (condition)
      (declare (ignore condition))
      (compiler-error "Unable to recover from read error."))))


;;; Unexpected-EOF-Error  --  Internal
;;;
;;;    Print an error message giving some context for an EOF error.  We print
;;; the first line after Pos that contains #\" or #\(, or lacking that, the
;;; first non-empty line.
;;;
(defun unexpected-eof-error (stream pos condition)
  (declare (type stream stream) (type unsigned-byte pos))
  (let ((res nil))
    (file-position stream pos)
    (loop
      (let ((line (read-line stream nil nil))) 
	(unless line (return))
	(when (or (find #\" line) (find #\( line))
	  (setq res line)
	  (return))
	(unless (or res (zerop (length line)))
	  (setq res line))))

    (compiler-error-message
     "Read error in form starting at ~D:~%~@[ \"~A\"~%~]~A"
     pos res condition))

  (file-position stream (file-length stream))
  (undefined-value))


;;; Careful-Read  --  Internal
;;;
;;;    Read a form from Stream, returning EOF at EOF.  If a read error happens,
;;; then attempt to recover if possible, returing a proxy error form.
;;;
(defun careful-read (stream eof pos)
  (handler-case (read stream nil eof)
    (error (condition)
      (let ((new-pos (file-position stream)))
	(cond ((= new-pos (file-length stream))
	       (unexpected-eof-error stream pos condition))
	      (t
	       (normal-read-error stream pos condition)
	       (ignore-error-form stream pos))))
      '(error "Attempt to load a file having a compile-time read error."))))


;;; Get-Source-Stream  --  Internal
;;;
;;;    If Stream is present, return it, otherwise open a stream to the current
;;; file.  There must be a current file.  When we open a new file, we also
;;; reset *Package* and *Default-Cookie*.  This gives the effect of rebinding
;;; around each file.
;;;
(defun get-source-stream (info)
  (declare (type source-info info))
  (cond ((source-info-stream info))
	(t
	 (setq *package* *initial-package*)
	 (setq *default-cookie* (copy-cookie *initial-cookie*))
	 (setf (source-info-stream info)
	       (open (file-info-name (first (source-info-current-file info)))
		     :direction :input)))))


;;; CLOSE-SOURCE-INFO  --  Internal
;;;
;;;    Close the stream in Info if it is open.
;;;
(defun close-source-info (info)
  (declare (type source-info info))
  (let ((stream (source-info-stream info)))
    (when stream (close stream)))
  (setf (source-info-stream info) nil)
  (undefined-value))


;;; Advance-Source-File  --  Internal
;;;
;;;    Advance Info to the next source file.  If none, return NIL, otherwise T.
;;;
(defun advance-source-file (info)
  (declare (type source-info info))
  (close-source-info info)
  (let ((prev (pop (source-info-current-file info))))
    (if (source-info-current-file info)
	(let ((current (first (source-info-current-file info))))
	  (setf (file-info-source-root current)
		(+ (file-info-source-root prev)
		   (length (file-info-forms prev))))
	  t)
	nil)))


;;; Read-Source-Form  --  Internal
;;;
;;;    Read the next form from the source designated by Info.  The second value
;;; is the top-level form number of the read form.  The third value is true
;;; when at EOF.
;;;
;;;   We carefully read from the current source file.  If it is at EOF, we
;;; advance to the next file and try again.  When we get a form, we enter it
;;; into the per-file Forms and Positions vectors.
;;;
(defun read-source-form (info) 
  (declare (type source-info info))
  (let ((eof '(*eof*)))
    (loop
      (let* ((file (first (source-info-current-file info)))
	     (stream (get-source-stream info))
	     (pos (file-position stream))
	     (res (careful-read stream eof pos)))
	(unless (eq res eof)
	  (let* ((forms (file-info-forms file))
		 (current-idx (+ (fill-pointer forms)
				 (file-info-source-root file))))
	    (vector-push-extend res forms)
	    (vector-push-extend pos (file-info-positions file))
	    (return (values res current-idx nil))))

	(unless (advance-source-file info)
	  (return (values nil nil t)))))))


;;; Find-Source-Root  --  Interface
;;;
;;;    Return the Index'th source form read from Info and the position that it
;;; was read at.
;;;
(defun find-source-root (index info)
  (declare (type unsigned-byte index) (type source-info info))
  (dolist (file (source-info-files info))
    (let ((root (file-info-source-root file))
	  (forms (file-info-forms file)))
      (when (> (+ (length forms) root) index)
	(let ((idx (- index root)))
	  (return (values (aref forms idx)
			  (aref (file-info-positions file) idx))))))))


;;;; Top-level form processing:

;;; CONVERT-AND-MAYBE-COMPILE  --  Internal
;;;
;;;    Called by top-level form processing when we are ready to actually
;;; compile something.  If *BLOCK-COMPILE* is true, then we still convert the
;;; form, but delay compilation, pushing the result on *TOP-LEVEL-LAMBDAS*
;;; instead.
;;; 
(defun convert-and-maybe-compile (form tlf-num object)
  (declare (type index tlf-num) (type object object))
  (let ((tll (ir1-top-level form tlf-num nil)))
    (cond (*block-compile* (push tll *top-level-lambdas*))
	  (t
	   (compile-top-level (list tll) object)
	   (clear-stuff)))))


;;; PROCESS-PROGN  --  Internal
;;;
;;;    Process a PROGN-like portion of a top-level form.  Forms is a list of
;;; the forms, and TLF-Num is the top-level form number of the form they came
;;; out of.
;;;
(defun process-progn (forms tlf-num object)
  (declare (list forms) (type index tlf-num) (type object object))
  (dolist (form forms)
    (process-form form tlf-num object)))


(proclaim '(special *compiler-error-bailout*))

;;; PROCESS-FORM  --  Internal
;;;
;;;    Process a top-level Form with the specified source Path and output to
;;; Object.  If this is a magic top-level form, then do stuff, otherwise just
;;; compile it.
;;;
;;; ### At least for now, always dump package frobbing as interpreted cold load
;;; forms.  This might want to be on a switch someday.
;;;
(defun process-form (form tlf-num object)
  (declare (type index tlf-num) (type object object))
  (catch 'process-form-error-abort
    (let* ((*compiler-error-bailout*
	    #'(lambda ()
		(convert-and-maybe-compile
		 `(error "Execution of a form compiled with errors:~% ~S"
			 ',form)
		 tlf-num object)
		(throw 'process-form-error-abort nil)))
	   (form
	    (handler-case (macroexpand form *fenv*)
	      (error (condition)
		     (compiler-error "(during macroexpansion)~%~A"
				     condition)))))
      (if (atom form)
	  (convert-and-maybe-compile form tlf-num object)
	  (case (car form)
	    ((make-package in-package shadow shadowing-import export
			   unexport use-package unuse-package import)
	     (eval form)
	     (etypecase object
	       (fasl-file (fasl-dump-cold-load-form form object))
	       ((or null core-object))))
	    ((eval-when)
	     (unless (>= (length form) 2)
	       (compiler-error "EVAL-WHEN form is too short: ~S." form))
	     (do-eval-when-stuff
	      (cadr form) (cddr form)
	      #'(lambda (forms)
		  (process-progn forms tlf-num object))))
	    ((macrolet)
	     (unless (>= (length form) 2)
	       (compiler-error "MACROLET form is too short: ~S." form))
	     (do-macrolet-stuff
	      (cadr form)
	      #'(lambda ()
		  (process-progn (cddr form) tlf-num object))))
	    (progn (process-progn (cdr form) tlf-num object))
	    (t
	     (convert-and-maybe-compile form tlf-num object))))))
      
  (undefined-value))


;;;; COMPILE-FILE and COMPILE-FROM-STREAM: 

;;; Sub-Compile-File  --  Internal
;;;
;;;    Read all forms from Info and compile them, with output to Object.  If
;;; *Block-Compile* is true, we combine all the forms and compile as a unit,
;;; otherwise we compile each one separately.  We return :ERROR, :WARNING,
;;; :NOTE or NIL to indicate the most severe kind of compiler diagnostic
;;; emitted.
;;;
(defun sub-compile-file (info object)
  (declare (type source-info info) (type object object))
  (with-compilation-unit ()
    (let ((start-errors *compiler-error-count*)
	  (start-warnings *compiler-warning-count*)
	  (start-notes *compiler-note-count*))
      (with-ir1-namespace
	(clear-stuff)
	(let* ((*package* *package*)
	       (*initial-package* *package*)
	       (*initial-cookie* *default-cookie*)
	       (*default-cookie* (copy-cookie *initial-cookie*))
	       (*current-cookie* (make-cookie))
	       (*fenv* ())
	       (*source-info* info)
	       (*top-level-lambdas* ())
	       (*compiler-error-bailout*
		#'(lambda ()
		    (compiler-mumble
		     "~2&Fatal error, aborting compilation...~%")
		    (return-from sub-compile-file :error)))
	       (*last-source-context* nil)
	       (*last-original-source* nil)
	       (*last-source-form* nil)
	       (*last-format-string* nil)
	       (*last-format-args* nil)
	       (*last-message-count* 0))
	  (loop
	    (multiple-value-bind (form tlf eof-p)
				 (read-source-form info)
	      (when eof-p (return))
	      (clrhash *source-paths*)
	      (find-source-paths form tlf)
	      (process-form form tlf object)))
	  
	  (when *block-compile*
	    (compile-top-level (nreverse *top-level-lambdas*) object)
	    (clear-stuff))
	  
	  (etypecase object
	    (fasl-file (fasl-dump-source-info info object))
	    (core-object (fix-core-source-info info object))
	    (null))))
      
      (cond ((> *compiler-error-count* start-errors) :error)
	    ((> *compiler-warning-count* start-warnings) :warning)
	    ((> *compiler-note-count* start-notes) :note)
	    (t nil)))))


;;; Verify-Source-Files  --  Internal
;;;
;;;    Return a list of pathnames that are the truenames of all the named
;;; files.
;;;
(defun verify-source-files (stuff)
  (unless stuff
    (error "Can't compile with no source files."))
  (mapcar #'(lambda (x)
	      (or (probe-file x)
		  (truename
		   (merge-pathnames x (make-pathname :type "lisp")))))
	  (if (listp stuff) stuff (list stuff))))


#+new-compiler
;;; COMPILE-FROM-STREAM  --  Public
;;;
;;;    Just call SUB-COMPILE-FILE on the on a stream source info for the
;;; stream, sending output to core.
;;;
(defun compile-from-stream (stream
			    &key
			    ((:error-stream *compiler-error-output*)
			     *error-output*)
			    ((:trace-stream *compiler-trace-output*) nil)
			    (defined-from-pathname nil)
			    ((:block-compile *block-compile*) nil))
  (declare (ignore defined-from-pathname))
  "Similar to COMPILE-FILE, but compiles text from Stream into the current lisp
  environment.  Stream is closed when compilation is complete.  These keywords
  are supported:

  :Error-Stream
      The stream to write compiler error output to (default *ERROR-OUTPUT*.)
  :Trace-Stream
      The stream that we write compiler trace output to, or NIL (the default)
      to inhibit trace output.
  :Block-Compile
        If true, then function names will be resolved at compile time."
  (let ((info (make-stream-source-info stream)))
    (unwind-protect
	(let ((won (sub-compile-file info (make-core-object))))
	  (values (not (null won))
		  (if (member won '(:error :warning)) t nil)))
      (close-source-info info))))


;;; COMPILE-FILE  --  Public
;;;
;;;    Open some files and call SUB-COMPILE-FILE.  If the compile is unwound
;;; out of, then abort the writing of the output file, so we don't overwrite it
;;; with known garbage.
;;;
(defun #-new-compiler ncompile-file #+new-compiler compile-file
  (source &key
	  (output-file t)
	  (error-file t)
	  (trace-file nil) 
	  (error-output t)
	  (load nil)
	  ((:block-compile *block-compile*) nil))
  "Compiles Source, producing a corresponding .FASL file.  Source may be a list
  of files, in which case the files are compiled as a unit, producing a single
  .FASL file.  The output file names are defaulted from the first (or only)
  input file name.  Other options available via keywords:
  :Output-File
        The name of the fasl to output, NIL for none, T for the default.
  :Error-File
        The name of the error listing file, NIL for none, T for the .ERR
        default.
  :Trace-File
        If specified, internal data structures are dumped to this file.  T for
        the .TRACE default.
  :Error-Output
        If a stream, then error output is sent there as well as to the listing
        file.  NIL suppresses this additional error output.  The default is T,
        which means use *ERROR-OUTPUT*.
  :Block-Compile
        If true, then function names will be resolved at compile time."
  
  (let* ((fasl-file nil)
	 (error-file-stream nil)
	 (output-file-name nil)
	 (*compiler-error-output* nil)
	 (*compiler-trace-output* nil)
	 (compile-won nil)
	 (error-severity nil)
	 (source (verify-source-files source))
	 (source-info (make-file-source-info source))
	 (default (pathname (first source))))
    (unwind-protect
	(progn
	  #-new-compiler
	  (pushnew :new-compiler *features*)
	  (flet ((frob (file type)
		   (if (eq file t)
		       (make-pathname :type type  :defaults default)
		       (pathname file))))
	    
	    (when output-file
	      (setq output-file-name (frob output-file "nfasl"))
	      (setq fasl-file (open-fasl-file output-file-name
					      (namestring (first source)))))
	    
	    (when trace-file
	      (setq *compiler-trace-output*
		    (open (frob trace-file "trace")
			  :if-exists :supersede
			  :direction :output)))
	    
	    (when error-file
	      (setq error-file-stream
		    (open (frob error-file "err")
			  :if-exists :supersede
			  :direction :output))))
	  
	  (setq *compiler-error-output*
		(apply #'make-broadcast-stream
		       (remove nil
			       (list (if (eq error-output t)
					 *error-output*
					 error-output)
				     error-file-stream))))
	  (setq error-severity
		(sub-compile-file source-info fasl-file))
	  (setq compile-won t))

      #-new-compiler
      (setq *features* (remove :new-compiler *features*))

      (close-source-info source-info)

      (when fasl-file
	(close-fasl-file fasl-file (not compile-won)))

      (when error-file-stream
	(let ((name (pathname error-file-stream)))
	  (close error-file-stream)
	  (unless error-severity
	    (delete-file name))))

      (when *compiler-trace-output*
	(close *compiler-trace-output*)))

    (when load
      (unless output-file
	(error "Can't :LOAD with no output file."))
      (load output-file-name :verbose t))

    (values (if output-file (truename output-file-name) nil)
	    (not (null error-severity))
	    (if (member error-severity '(:warning :error)) t nil))))


;;;; COMPILE and UNCOMPILE:

;;; GET-LAMBDA-TO-COMPILE  --  Internal
;;;
(defun get-lambda-to-compile (definition)
  (if (consp definition)
      definition
      (multiple-value-bind (def env-p)
			   (function-lambda-expression definition)
	(when env-p
	  (error "~S was defined in a non-null environment." definition))
	(unless def
	  (error "Can't find a definition for ~S." definition))
	def)))


;;; COMPILE-FIX-FUNCTION-NAME  --  Internal
;;;
;;;    Find the function that is being compiled by COMPILE and bash its name to
;;; NAME.  We also substitute for any references to name so that recursive
;;; calls will be compiled direct.  Lambda is the top-level lambda for the
;;; compilation.  A REF for the real function is the only thing in the
;;; top-level lambda other than the bind and return, so it isn't too hard to
;;; find.
;;;
(defun compile-fix-function-name (lambda name)
  (declare (type clambda lambda) (type (or symbol cons) name))
  (when name
    (let ((fun (ref-leaf
		(continuation-next
		 (node-cont (lambda-bind lambda))))))
      (setf (leaf-name fun) name)
      (let ((old (gethash name *free-functions*)))
	(when old (substitute-leaf fun old)))
      name)))


;;; COMPILE  --  Public
;;;
#+new-compiler
(defun compile (name &optional (definition (fdefinition name)))
  "Compiles the function whose name is Name.  If Definition is supplied,
  it should be a lambda expression that is compiled and then placed in the
  function cell of Name.  If Name is Nil, the compiled code object is
  returned."
  (with-compilation-unit ()
    (with-ir1-namespace
      (clear-stuff)
      (let* ((start-errors *compiler-error-count*)
	     (start-warnings *compiler-warning-count*)
	     (start-notes *compiler-note-count*)
	     (*current-cookie* (make-cookie))
	     (*fenv* ())
	     (form `#',(get-lambda-to-compile definition))
	     (*source-info* (make-lisp-source-info form))
	     (*top-level-lambdas* ())
	     (*compiler-error-bailout*
	      #'(lambda ()
		  (compiler-mumble
		   "~2&Fatal error, aborting compilation...~%")
		  (return-from compile (values nil t nil))))
	     (*compiler-error-output* *error-output*)
	     (*compiler-trace-output* nil)
	     (*last-source-context* nil)
	     (*last-original-source* nil)
	     (*last-source-form* nil)
	     (*last-format-string* nil)
	     (*last-format-args* nil)
	     (*last-message-count* 0)
	     (object (make-core-object)))
	(find-source-paths form 0)
	(let ((lambda (ir1-top-level form 0 t)))
	  
	  (compile-fix-function-name lambda name)
	  (let* ((component
		  (block-component (node-block (lambda-bind lambda))))
		 (*all-components* (list component)))
	    (local-call-analyze component))
	  
	  (let* ((components (find-initial-dfo (list lambda)))
		 (*all-components* components))
	    (dolist (component components)
	      (compile-component component object)
	      (clear-ir2-info component)))
	  
	  (fix-core-source-info *source-info* object)
	  (let* ((res (core-call-top-level-lambda lambda object))
		 (return (or name res)))
	    (when name
	      (setf (fdefinition name) res))
	    
	    (cond ((or (> *compiler-error-count* start-errors)
		       (> *compiler-warning-count* start-warnings))
		   (values return t t))
		  ((> *compiler-note-count* start-notes)
		   (values return t nil))
		  (t
		   (values return nil nil)))))))))

#+new-compiler
;;; UNCOMPILE  --  Public
;;;
(defun uncompile (name)
  "Attempt to replace Name's definition with an interpreted version of that
  definition.  If no interpreted definition is to be found, then signal an
  error."
  (let ((def (fdefinition name)))
    (if (eval:interpreted-function-p def)
	(warn "~S is already interpreted." name)
	(setf (fdefinition name)
	      (coerce (get-lambda-to-compile def) 'function))))
  name)
