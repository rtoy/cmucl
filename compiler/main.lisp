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
(in-package "C")
(in-package "EXTENSIONS")
(export '(compile-from-stream))
(in-package "C")

(proclaim '(special *constants* *free-variables* *compile-component*
		    *code-vector* *next-location* *result-fixups*
		    *free-functions* *source-paths* *failed-optimizations*
		    *seen-blocks* *seen-functions* *list-conflicts-table*
		    *continuation-number* *continuation-numbers*
		    *number-continuations* *tn-id* *tn-ids* *id-tns*
		    *label-ids* *label-id* *id-labels* *sb-list*
		    *undefined-warnings* *compiler-error-count*
		    *compiler-warning-count* *compiler-note-count*
		    *compiler-error-output* *compiler-error-bailout*
		    *compiler-trace-output*
		    *last-source-context* *last-original-source*
		    *last-source-form* *last-format-string* *last-format-args*
		    *last-message-count* *lexical-environment*
		    *count-vop-usages*))

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


;;;; Vop counting utilities

;;; T if we should count the number of times we use each vop and the number
;;; of instructions that come from each.
;;; 
(defvar *count-vop-usages* nil)

;;; Hash table containing all the current counts.  The key is the name of the
;;; vop, and the value is a cons of the car holding the number of times that
;;; vop has been used and the cdr holding the number of instructions that
;;; vop has accounted for.
;;; 
(defvar *vop-counts* (make-hash-table :test #'eq))

(defun count-vop (vop)
  (let ((entry (gethash vop *vop-counts*)))
    (unless entry
      (setf entry (cons 0 0))
      (setf (gethash vop *vop-counts*) entry))
    (incf (car entry))))

(defun count-vop-instructions (vop instructions)
  (let ((entry (gethash vop *vop-counts*)))
    (unless entry
      (setf entry (cons 0 0))
      (setf (gethash vop *vop-counts*) entry))
    (incf (cdr entry) instructions)))

;;; Clear-Vop-Counts -- interface
;;; 
(defun clear-vop-counts ()
  (clrhash *vop-counts*)
  nil)

;;; Report-Vop-Counts -- interface
;;;
(defun report-vop-counts (&key (cut-off 15) (sort-by :size))
  (declare (type (or null unsigned-byte) cut-off)
	   (type (member :size :count :name) sort-by))
  (let ((results nil)
	(total-count 0)
	(total-size 0))
    (maphash #'(lambda (key value)
		 (push (cons key value) results)
		 (incf total-count (car value))
		 (incf total-size (cdr value)))
	     *vop-counts*)
    (format t "~20<Vop ~> ~20:@<Count~> ~20:@<Bytes~> Ave Sz~%")
    (dolist (info (sort results
			(ecase sort-by
			  (:name #'(lambda (name-1 name-2)
				     (string< (symbol-name name-1)
					      (symbol-name name-2))))
			  ((:count :size) #'>))
			:key (ecase sort-by
			       (:name #'car)
			       (:count #'cadr)
			       (:size #'cddr))))
      (when cut-off
	(if (zerop cut-off)
	    (return)
	    (decf cut-off)))
      (format t "~20<~S~> ~20<~:D (~4,1,2F%)~> ~20<~D (~4,1,2F%)~> ~6D~%"
	      (car info)
	      (cadr info)
	      (/ (coerce (cadr info) 'double-float)
		 (coerce total-count 'double-float))
	      (cddr info)
	      (/ (coerce (cddr info) 'double-float)
		 (coerce total-size 'double-float))
	      (truncate (cddr info) (cadr info)))))
  (values))


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

(defparameter *constraint-propagate* t)
(defparameter *reoptimize-after-type-check-max* 5)

(defevent reoptimize-maxed-out
  "*REOPTIMIZE-AFTER-TYPE-CHECK-MAX* exceeded.")

;;; IR1-Phases  --  Internal
;;;
;;;    Do all the IR1 phases for a non-top-level component.
;;;
(defun ir1-phases (component)
  (declare (type component component))
  (let ((*constraint-number* 0)
	(loop-count 1))
    (declare (special *constraint-number*))
    (loop
      (ir1-optimize-until-done component)
      (when (component-reanalyze component)
	(maybe-mumble "DFO ")
	(find-dfo component))
      (when *constraint-propagate*
	(maybe-mumble "Constraint ")
	(constraint-propagate component))
      (maybe-mumble "Type ")
      (generate-type-checks component)
      (unless (or (component-reoptimize component)
		  (component-reanalyze component))
	(return))
      (when (>= loop-count *reoptimize-after-type-check-max*)
	(maybe-mumble "[Reoptimize Limit]")
	(event reoptimize-maxed-out)
	(return))
      (incf loop-count)))
  
  (maphash #'note-failed-optimization *failed-optimizations*)
  (clrhash *failed-optimizations*)
  (undefined-value))


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

  (let ((*compile-component* component)
	(*code-segment* nil)
	(*elsewhere* nil))
    (maybe-mumble "Env ")
    (environment-analyze component)
    (maybe-mumble "GTN ")
    (gtn-analyze component)
    (maybe-mumble "LTN ")
    (ltn-analyze component)
    (maybe-mumble "Control ")
    (control-analyze component)

    (when (ir2-component-values-receivers (component-info component))
      (maybe-mumble "Stack ")
      (stack-analyze component))

    ;; Assign BLOCK-NUMBER for any cleanup blocks introduced by environment
    ;; or stack analysis.  There shouldn't be any unreachable code after
    ;; control, so this won't delete anything.
    (when (component-reanalyze component)
      (find-dfo component))

    (maybe-mumble "IR2Tran ")
    (init-assembler)
    (entry-analyze component)
    (ir2-convert component)

    (select-representations component)

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
    (let ((length (generate-code component)))
      
      (when *compiler-trace-output*
	(format *compiler-trace-output*
		"~|~%Assembly code for ~S~2%"
		component)
	(dump-segment *code-segment* *compiler-trace-output*))

      (when *count-vop-usages*
	(count-vops component)
	(count-instructions *code-segment*))

      (etypecase object
	(fasl-file
	 (maybe-mumble "FASL")
	 (fasl-dump-component component *code-segment* length object))
	(core-object
	 (maybe-mumble "Core")
	 (make-core-component component *code-segment* length object))
	(null))

      (nuke-segment *code-segment*)))

  (compiler-mumble "~&")
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
  (nuke-ir2-component component)
  (setf (component-info component) nil)

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


;;; CLEAR-IR1-INFO  --  Internal
;;;
;;;    Blow away the REFS for all global variables, and recycle the IR1 for
;;; Component.
;;;
(defun clear-ir1-info (component)
  (declare (type component component))
  (flet ((blast (x)
	   (maphash #'(lambda (k v)
			(declare (ignore k))
			(setf (leaf-refs v) nil)
			(when (basic-var-p v)
			  (setf (basic-var-sets v) nil)))
		    x)))
    (blast *free-variables*)
    (blast *free-functions*)
    (blast *constants*))
  (macerate-ir1-component component)
  (undefined-value))


;;; CLEAR-STUFF  --  Interface
;;;
;;;    Clear all the global variables used by the compiler.
;;;
(defun clear-stuff (&optional (debug-too t))
  ;;
  ;; Clear global tables.
  (when (boundp '*free-functions*)
    (clrhash *free-functions*)
    (clrhash *free-variables*)
    (clrhash *constants*))
  (clrhash *failed-optimizations*)
  ;;
  ;; Clear debug counters and tables.
  (clrhash *seen-blocks*)
  (clrhash *seen-functions*)
  (clrhash *list-conflicts-table*)

  (when debug-too
    (clrhash *continuation-numbers*)
    (clrhash *number-continuations*)
    (setq *continuation-number* 0)
    (clrhash *tn-ids*)
    (clrhash *id-tns*)
    (setq *tn-id* 0)
    (clrhash *label-ids*)
    (clrhash *id-labels*)
    (setq *label-id* 0))
  ;;
  ;; Clear some Pack data structures (for GC purposes only.)
  (dolist (sb *sb-list*)
    (when (finite-sb-p sb)
      (fill (finite-sb-live-tns sb) nil)))
  ;;
  ;; Reset Gensym.
  (setq lisp:*gensym-counter* 0)

  (values))


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
    (let ((undefs (sort *undefined-warnings* #'string<
			:key #'(lambda (x)
				 (let ((x (undefined-warning-name x)))
				   (if (symbolp x)
				       (symbol-name x)
				       (prin1-to-string x)))))))
      (unless *converting-for-interpreter*
	(dolist (undef undefs)
	  (let ((name (undefined-warning-name undef))
		(kind (undefined-warning-kind undef))
		(warnings (undefined-warning-warnings undef))
		(count (undefined-warning-count undef)))
	    (dolist (*compiler-error-context* warnings)
	      (compiler-warning "Undefined ~(~A~): ~S" kind name))
	    
	    (let ((warn-count (length warnings)))
	      (when (and warnings (> count warn-count))
		(let ((more (- count warn-count)))
		  (compiler-warning "~D more use~:P of undefined ~(~A~) ~S."
				    more kind name)))))))
  
      (dolist (kind '(:variable :function :type))
	(let ((summary (mapcar #'undefined-warning-name
			       (remove kind undefs :test-not #'eq
				       :key #'undefined-warning-kind))))
	  (when summary
	    (compiler-warning
	     "~:[This ~(~A~) is~;These ~(~A~)s are~] undefined:~
	      ~%  ~{~<~%  ~1:;~S~>~^ ~}"
	     (cdr summary) kind summary))))))
  
  (unless (or *converting-for-interpreter*
	      (and (not abort-p) (zerop abort-count)
		   (zerop *compiler-error-count*)
		   (zerop *compiler-warning-count*)
		   (zerop *compiler-note-count*)))
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
     *compiler-note-count*)))

   
;;; Describe-Component  --  Internal
;;;
;;;    Print out some useful info about Component to Stream.
;;;
(defun describe-component (component &optional
				     (*standard-output* *standard-output*))
  (declare (type component component))
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
  ;; This file's FILE-COMMENT, or NIL if none.
  (comment nil :type (or simple-string null))
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
  (declare (type stream stream) (type unsigned-byte old-pos))
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


;;; FIND-FILE-INFO  --  Interface
;;;
;;;    Return the File-Info describing the Index'th form.
;;;
(defun find-file-info (index info)
  (declare (type index index) (type source-info info))
  (dolist (file (source-info-files info))
    (when (> (+ (length (file-info-forms file))
		(file-info-source-root file))
	     index)
      (return file))))


;;; FIND-SOURCE-ROOT  --  Interface
;;;
;;;    Return the Index'th source form read from Info and the position that it
;;; was read at.
;;;
(defun find-source-root (index info)
  (declare (type source-info info) (type index index))
  (let* ((file (find-file-info index info))
	 (idx (- index (file-info-source-root file))))
    (values (aref (file-info-forms file) idx)
	    (aref (file-info-positions file) idx))))

;;;; Top-level form processing:

;;; CONVERT-AND-MAYBE-COMPILE  --  Internal
;;;
;;;    Called by top-level form processing when we are ready to actually
;;; compile something.  If *BLOCK-COMPILE* is true, then we still convert the
;;; form, but delay compilation, pushing the result on *TOP-LEVEL-LAMBDAS*
;;; instead.
;;;
;;;   The *DEFAULT-COOKIE* at this time becomes the default policy for
;;; compiling the form.  Any enclosed PROCLAIMs will affect only subsequent
;;; forms.
;;; 
(defun convert-and-maybe-compile (form path object)
  (declare (list path) (type object object))
  (let* ((*lexical-environment*
	  (make-lexenv :cookie *default-cookie*))
	 (tll (ir1-top-level form path nil)))
    (cond (*block-compile* (push tll *top-level-lambdas*))
	  (t
	   (compile-top-level (list tll) object)
	   (clear-stuff))))
  #+new-compiler
  (lisp::maybe-gc))


;;; PROCESS-PROGN  --  Internal
;;;
;;;    Process a PROGN-like portion of a top-level form.  Forms is a list of
;;; the forms, and Path is source path of the form they came out of.
;;;
(defun process-progn (forms path object)
  (declare (list forms) (list path) (type object object))
  (dolist (form forms)
    (process-form form path object)))


;;; PREPROCESSOR-MACROEXPAND  --  Internal
;;;
;;;    Macroexpand form in the current environment with an error handler.  We
;;; only expand one level, so that we retain all the intervening forms in the
;;; source path.
;;;
(defun preprocessor-macroexpand (form)
  (handler-case #+new-compiler (macroexpand-1 form *lexical-environment*)
                #-new-compiler
                (let ((fenv (lexenv-functions *lexical-environment*)))
		  (if (consp form)
		      (let* ((name (car form))
			     (exp (or (cddr (assoc name fenv))
				      (info function macro-function name))))
			(if exp
			    (funcall exp form fenv)
			    form))
		      form))
    (error (condition)
       (compiler-error "(during macroexpansion)~%~A" condition))))


(proclaim '(special *compiler-error-bailout*))

;;; PROCESS-FORM  --  Internal
;;;
;;;    Process a top-level Form with the specified source Path and output to
;;; Object.
;;; -- If this is a magic top-level form, then do stuff.
;;; -- If it is a macro expand it.
;;; -- Otherwise, just compile it.
;;;
;;; ### At least for now, always dump package frobbing as interpreted cold load
;;; forms.  This might want to be on a switch someday.
;;;
(defun process-form (form path object)
  (declare (list path) (type object object))
  (catch 'process-form-error-abort
    (let* ((path (or (gethash form *source-paths*) (cons form path)))
	   (*compiler-error-bailout*
	    #'(lambda ()
		(convert-and-maybe-compile
		 `(error "Execution of a form compiled with errors:~% ~S"
			 ',form)
		 path object)
		(throw 'process-form-error-abort nil))))
      (if (atom form)
	  (convert-and-maybe-compile form path object)
	  (case (car form)
	    ((make-package in-package shadow shadowing-import export
			   unexport use-package unuse-package import)
	     (eval form)
	     (etypecase object
	       (fasl-file
		(compile-top-level-lambdas () t object)
		(fasl-dump-cold-load-form form object))
	       ((or null core-object)
		(convert-and-maybe-compile form path object))))
	    ((eval-when)
	     (unless (>= (length form) 2)
	       (compiler-error "EVAL-WHEN form is too short: ~S." form))
	     (do-eval-when-stuff
	      (cadr form) (cddr form)
	      #'(lambda (forms)
		  (process-progn forms path object))))
	    ((macrolet)
	     (unless (>= (length form) 2)
	       (compiler-error "MACROLET form is too short: ~S." form))
	     (do-macrolet-stuff
	      (cadr form)
	      #'(lambda ()
		  (process-progn (cddr form) path object))))
	    (progn (process-progn (cdr form) path object))
	    (file-comment
	     (unless (and (= (length form) 2) (stringp (second form)))
	       (compiler-error "Bad FILE-COMMENT form: ~S." form))
	     (let ((file (first (source-info-current-file *source-info*))))
	       (cond ((file-info-comment file)
		      (compiler-warning "Ignoring extra file comment:~%  ~S."
					form))
		     (t
		      (let ((comment (coerce (second form) 'simple-string)))
			(setf (file-info-comment file) comment)
			(compiler-mumble "~&Comment: ~A~2&" comment))))))
	    (t
	     (let ((exp (preprocessor-macroexpand form)))
	       (if (eq exp form)
		   (convert-and-maybe-compile form path object)
		   (process-form exp path object))))))))
      
  (undefined-value))


;;;; COMPILE-FILE and COMPILE-FROM-STREAM: 

;;; We build a list of top-level lambdas, and then periodically smash them
;;; together into a single component and compile it.
;;;
(defvar *pending-top-level-lambdas*)

;;; The maximum number of top-level lambdas we put in a single top-level
;;; component.
;;;
(defparameter top-level-lambda-max 10)


;;; COMPILE-TOP-LEVEL-LAMBDAS  --  Internal
;;;
;;;    Add Lambdas to the pending lambdas.  If this leaves more than
;;; TOP-LEVEL-LAMBDA-MAX lambdas in the list, or if Force-P is true, then smash
;;; the lambdas into a single component, compile it, and call the resulting
;;; function.
;;;
(defun compile-top-level-lambdas (lambdas force-p object)
  (declare (list lambdas) (type object object))
  (setq *pending-top-level-lambdas*
	(append *pending-top-level-lambdas* lambdas))
  (let ((pending *pending-top-level-lambdas*))
    (when (and pending
	       (or (> (length pending) top-level-lambda-max)
		   force-p))
      (multiple-value-bind (component tll)
			   (merge-top-level-lambdas pending)
	(setq *pending-top-level-lambdas* ())
	(compile-component component object)
	(clear-ir2-info component)
	(clear-ir1-info component)
	(etypecase object
	  (fasl-file
	   (fasl-dump-top-level-lambda-call tll object))
	  (core-object
	   (core-call-top-level-lambda tll object))
	  (null)))))
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
  (multiple-value-bind (components top-components)
		       (find-initial-dfo lambdas)
    (let ((*all-components* (append components top-components))
	  (top-level-closure nil))
      (when *check-consistency*
	(maybe-mumble "[Check]~%")
	(check-ir1-consistency *all-components*))
      
      (dolist (component top-components)
	(pre-environment-analyze-top-level component))
      
      (dolist (component components)
	(compile-component component object)
	(clear-ir2-info component)
	(if (replace-top-level-xeps component)
	    (setq top-level-closure t)
	    (unless *check-consistency*
	      (clear-ir1-info component))))
      
      (when *check-consistency*
	(maybe-mumble "[Check]~%")
	(check-ir1-consistency *all-components*))
      
      (compile-top-level-lambdas lambdas top-level-closure object)

      (when *check-consistency*
	(dolist (component components)
	  (clear-ir1-info component)))))
    
  (maphash #'check-free-function *free-functions*)
  (undefined-value))


;;; Sub-Compile-File  --  Internal
;;;
;;;    Read all forms from Info and compile them, with output to Object.  If
;;; *Block-Compile* is true, we combine all the forms and compile as a unit,
;;; otherwise we compile each one separately.  We return :ERROR, :WARNING,
;;; :NOTE or NIL to indicate the most severe kind of compiler diagnostic
;;; emitted.
;;;
(defun sub-compile-file (info object &optional d-s-info)
  (declare (type source-info info) (type object object))
  (with-ir1-namespace
    (clear-stuff)
    (let* ((start-errors *compiler-error-count*)
	   (start-warnings *compiler-warning-count*)
	   (start-notes *compiler-note-count*)
	   (*package* *package*)
	   (*initial-package* *package*)
	   (*initial-cookie* *default-cookie*)
	   (*default-cookie* (copy-cookie *initial-cookie*))
	   (*lexical-environment* (make-null-environment))
	   (*converting-for-interpreter* nil)
	   (*source-info* info)
	   (*top-level-lambdas* ())
	   (*pending-top-level-lambdas* ())
	   (*compiler-error-bailout*
	    #'(lambda ()
		(compiler-mumble
		 "~2&Fatal error, aborting compilation...~%")
		(return-from sub-compile-file :error)))
	   (*current-path* nil)
	   (*last-source-context* nil)
	   (*last-original-source* nil)
	   (*last-source-form* nil)
	   (*last-format-string* nil)
	   (*last-format-args* nil)
	   (*last-message-count* 0))
      (with-compilation-unit ()
	(loop
	  (multiple-value-bind (form tlf eof-p)
			       (read-source-form info)
	    (when eof-p (return))
	    (clrhash *source-paths*)
	    (find-source-paths form tlf)
	    (process-form form `(original-source-start 0 ,tlf) object)))
	
	(when *block-compile*
	  (compile-top-level (nreverse *top-level-lambdas*) object)
	  (clear-stuff))

	(compile-top-level-lambdas () t object)
	
	(etypecase object
	  (fasl-file (fasl-dump-source-info info object))
	  (core-object (fix-core-source-info info object d-s-info))
	  (null))
    
	(cond ((> *compiler-error-count* start-errors) :error)
	      ((> *compiler-warning-count* start-warnings) :warning)
	      ((> *compiler-note-count* start-notes) :note)
	      (t nil))))))


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
			    ((:block-compile *block-compile*) nil)
			    source-info)
  "Similar to COMPILE-FILE, but compiles text from Stream into the current lisp
  environment.  Stream is closed when compilation is complete.  These keywords
  are supported:

  :Error-Stream
      The stream to write compiler error output to (default *ERROR-OUTPUT*.)
  :Trace-Stream
      The stream that we write compiler trace output to, or NIL (the default)
      to inhibit trace output.
  :Block-Compile
        If true, then function names will be resolved at compile time.
  :Source-Info
        Some object to be placed in the DEBUG-SOURCE-INFO."
  (let ((info (make-stream-source-info stream)))
    (unwind-protect
	(let ((won (sub-compile-file info (make-core-object) source-info)))
	  (values (not (null won))
		  (if (member won '(:error :warning)) t nil)))
      (close-source-info info))))


(defun elapsed-time-to-string (tsec)
  (multiple-value-bind (tmin sec)
		       (truncate tsec 60)
    (multiple-value-bind (thr min)
			 (truncate tmin 60)
      (format nil "~D:~2,'0D:~2,'0D" thr min sec))))


;;; START-ERROR-OUTPUT, FINISH-ERROR-OUTPUT  --  Internal
;;;
;;;    Print some junk at the beginning and end of compilation.
;;;
(defun start-error-output (source-info)
  (declare (type source-info source-info))
  (compiler-mumble "~2&Python version ~A, VM version ~A on ~A.~%"
		   compiler-version vm-version
		   (ext:format-universal-time nil (get-universal-time)
					      :style :government
					      :print-weekday nil
					      :print-timezone nil))
  (dolist (x (source-info-files source-info))
    (compiler-mumble "Compiling: ~A ~A~%"
		     (namestring (file-info-name x))
		     (ext:format-universal-time nil (file-info-write-date x)
						:style :government
						:print-weekday nil
						:print-timezone nil)))
  (compiler-mumble "~%")
  (undefined-value))
;;;
(defun finish-error-output (source-info won)
  (declare (type source-info source-info))
  (compiler-mumble "Compilation ~:[aborted after~;finished in~] ~A.~&"
		   won
		   (elapsed-time-to-string
		    (- (get-universal-time)
		       (source-info-start-time source-info))))
  (undefined-value))


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
	 (*compiler-error-output* *compiler-error-output*)
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
	      (setq output-file-name
		    (frob output-file vm:target-fasl-file-type))
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

	  (start-error-output source-info)
	  (setq error-severity
		(sub-compile-file source-info fasl-file))
	  (setq compile-won t))

      #-new-compiler
      (setq *features* (remove :new-compiler *features*))

      (close-source-info source-info)

      (when fasl-file
	(close-fasl-file fasl-file (not compile-won))
	(when compile-won
	  (compiler-mumble "~2&~A written.~%"
			   (namestring (truename output-file-name)))))

      (finish-error-output source-info compile-won)

      (when error-file-stream
	(let ((name (pathname error-file-stream)))
	  (close error-file-stream)
	  (when (and compile-won (not error-severity))
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
	(when old
	  (substitute-leaf-if #'(lambda (x)
				  (not (eq (ref-inlinep x) :notinline)))
			      fun old)))
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
	     (*lexical-environment* (make-null-environment))
	     (form `#',(get-lambda-to-compile definition))
	     (*source-info* (make-lisp-source-info form))
	     (*top-level-lambdas* ())
	     (*converting-for-interpreter* nil)
	     (*compiler-error-bailout*
	      #'(lambda ()
		  (compiler-mumble
		   "~2&Fatal error, aborting compilation...~%")
		  (return-from compile (values nil t nil))))
	     (*compiler-error-output* *error-output*)
	     (*compiler-trace-output* nil)
	     (*current-path* nil)
	     (*last-source-context* nil)
	     (*last-original-source* nil)
	     (*last-source-form* nil)
	     (*last-format-string* nil)
	     (*last-format-args* nil)
	     (*last-message-count* 0)
	     (object (make-core-object)))
	(find-source-paths form 0)
	(let ((lambda (ir1-top-level form '(original-source-start 0 0) t)))
	  
	  (compile-fix-function-name lambda name)
	  (let* ((component
		  (block-component (node-block (lambda-bind lambda))))
		 (*all-components* (list component)))
	    (local-call-analyze component))
	  
	  (multiple-value-bind (components top-components)
			       (find-initial-dfo (list lambda))
	    (let ((*all-components* (append components top-components)))
	      (dolist (component *all-components*)
		(compile-component component object)
		(clear-ir2-info component))))
	  
	  (let* ((res (core-call-top-level-lambda lambda object))
		 (return (or name res)))
	    (fix-core-source-info *source-info* object res)
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
