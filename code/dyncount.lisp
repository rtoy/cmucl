;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/dyncount.lisp,v 1.2 1992/02/13 09:54:29 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Runtime support for dynamic VOP statistics collection.
;;; 
(in-package "C")

#|
Put *count-adjustments* back into VOP costs, and verify them.
Make sure multi-cycle instruction costs are plausible.
VOP classification.
  Make tables of %cost for benchmark X class.
  Could be represented as a sort of bar chart.
|#

(eval-when (compile)
  (when *collect-dynamic-statistics*
    (error "Compiling this file with dynamic stat collection turn on would ~
    be a very bad idea.")))

;;;; Hash utilities:

(defmacro do-hash ((key-var value-var table &optional result)
		   &body (body decls))
  "DO-HASH (Key-Var Value-Var Table [Result]) Declaration* Form*
   Iterate over the entries in a hash-table."
  (let ((gen (gensym))
	(n-more (gensym)))
    `(with-hash-table-iterator (,gen ,table)
       (loop
	 (multiple-value-bind (,n-more ,key-var ,value-var)
			      (,gen)
	   ,@decls
	   (unless ,n-more (return ,result))
	   ,@body)))))

(defun make-hash-table-like (table)
  "Make a hash-table with the same test as table."
  (declare (type hash-table table))
  (make-hash-table :test (lisp::hash-table-kind table)))

(defun hash-difference (table1 table2)
  "Return a hash-table containing only the entries in Table1 whose key is not
   also a key in Table2." (declare (type hash-table table1 table2))
  (let ((res (make-hash-table-like table1)))
    (do-hash (k v table1)
      (unless (nth-value 1 (gethash k table2))
	(setf (gethash res k) v)))
    res))

(defun hash-list (table)
  "Return a list of the values in Table."
  (declare (type hash-table table))
  (collect ((res))
    (do-hash (k v table)
      (declare (ignore k))
      (res v))
    (res)))

;;; READ-HASH-TABLE, WRITE-HASH-TABLE  --  Public
;;;
;;;    Read (or write) a hashtable from (or to) a file.
;;;
(defun read-hash-table (file)
  (with-open-file (s file :direction :input)
    (dotimes (i 3)
      (format t "~%; ~A" (read-line s)))
    (let* ((eof '(nil))
	   (test (read s))
	   (reader (read s))
	   (res (make-hash-table :test test)))
      (read s); Discard writer...
      (loop
	(let ((key (read s nil eof)))
	  (when (eq key eof) (return))
	  (setf (gethash key res)
		(funcall reader s key))))
      res)))
;;;
(defun write-hash-table (table file &key
			       (comment (format nil "Contents of ~S" table))
			       (reader 'read) (writer 'prin1) (test 'equal))
  (with-open-file (s file :direction :output :if-exists :new-version)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
	(format s "~A~%Version ~A on ~A~%"
		comment (lisp-implementation-version)
		(machine-instance))
	(format-universal-time s (get-universal-time))
	(terpri s)
	(format s "~S ~S ~S~%" test reader writer)
	(do-hash (k v table)
	  (prin1 k s)
	  (write-char #\space s)
	  (funcall writer v s)
	  (terpri s)))))
  table)


;;;; Info accumulation:

;;; Used to accumulate info about the usage of a single VOP.  Cost and count
;;; are kept as double-floats, which lets us get more bits and avoid annoying
;;; overflows.
;;;
(deftype count-vector () '(simple-array double-float (2)))
;;;
(defstruct (vop-stats
	    (:constructor %make-vop-stats (name))
	    (:constructor make-vop-stats-key))
  (name (required-argument) :type simple-string)
  (data (make-array 2 :element-type 'double-float) :type count-vector))

(defmacro vop-stats-count (x) `(aref (vop-stats-data ,x) 0))
(defmacro vop-stats-cost (x) `(aref (vop-stats-data ,x) 1))

(defun make-vop-stats (&key name count cost)
  (let ((res (%make-vop-stats name)))
    (setf (vop-stats-count res) count)
    (setf (vop-stats-cost res) cost)
    res))
    
(declaim (freeze-type dyncount-info vop-stats))


;;; NOTE-DYNCOUNT-INFO  --  Internal
;;;
;;;    Add the Info into the cumulative result on the VOP name plist.  We use
;;; plists so that we will touch minimal system code outside of this file
;;; (which may be compiled with profiling on.)
;;;
(defun note-dyncount-info (info)
  (declare (type dyncount-info info) (inline get %put)
	   (optimize (speed 2)))
  (let ((counts (dyncount-info-counts info))
	(vops (dyncount-info-vops info)))
    (dotimes (index (length counts))
      (declare (type index index))
      (let ((count (coerce (the (unsigned-byte 31)
				(aref counts index))
			   'double-float)))
	(when (minusp count)
	  (warn "Oops: overflow.")
	  (return-from note-dyncount-info nil))
	(unless (zerop count)
	  (let* ((vop-info (svref vops index))
		 (length (length vop-info)))
	    (declare (simple-vector vop-info))
	    (do ((i 0 (+ i 4)))
		((>= i length))
	      (declare (type index i))
	      (let* ((name (svref vop-info i))
		     (entry (or (get name 'vop-stats)
				(setf (get name 'vop-stats)
				      (%make-vop-stats (symbol-name name))))))
		(incf (vop-stats-count entry)
		      (* (coerce (the index (svref vop-info (1+ i)))
				 'double-float)
			 count))
		(incf (vop-stats-cost entry)
		      (* (coerce (the index (svref vop-info (+ i 2)))
				 'double-float)
			 count))))))))))

(defun clear-dyncount-info (info)
  (declare (type dyncount-info info))
  (declare (optimize (speed 3) (safety 0)))
  (let ((counts (dyncount-info-counts info)))
    (dotimes (i (length counts))
      (setf (aref counts i) 0))))


;;; CLEAR-VOP-COUNTS  --  Public
;;;
;;;    Clear any VOP-COUNTS properties and the counts vectors for all code
;;; objects.  The latter loop must not call any random functions.
;;;
(defun clear-vop-counts (&optional (spaces '(:dynamic)))
  "Clear all dynamic VOP counts for code objects in the specified spaces."
  (do-hash (k v (backend-template-names *backend*))
    (declare (ignore v))
    (remprop k 'vop-stats))
  
  (locally
      (declare (optimize (speed 3) (safety 0))
	       (inline vm::map-allocated-objects))
    (without-gcing
      (dolist (space spaces)
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (dyncount-info-p object)
	       (clear-dyncount-info object)))
	 space)))))


;;; GET-VOP-COUNTS  --  Public
;;;
;;;    Call NOTE-DYNCOUNT-INFO on all DYNCOUNT-INFO structure allocated in the
;;; specified spaces.  Return a hashtable describing the counts.  The initial
;;; loop must avoid calling any functions outside this file to prevent adding
;;; noise to the data, since other files may be compiled with profiling.
;;;
(defun get-vop-counts (&optional (spaces '(:dynamic)) &key (clear t))
  "Return a hash-table mapping string VOP names to VOP-STATS structures
   describing the VOPs executed.  If clear is true, then reset all counts to
   zero as a side-effect."
  (locally
      (declare (optimize (speed 3) (safety 0))
	       (inline vm::map-allocated-objects))
    (without-gcing
      (dolist (space spaces)
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (dyncount-info-p object)
	       (note-dyncount-info object)
	       (when clear
		 (clear-dyncount-info object))))
	 space))))
  
  (let ((counts (make-hash-table :test #'equal)))
    (do-hash (k v (backend-template-names *backend*))
      (declare (ignore v))
      (let ((stats (get k 'vop-stats)))
	(when stats
	  (setf (gethash k counts) stats)
	  (when clear
	    (remprop k 'vop-stats)))))
    counts))


;;; FIND-INFO-FOR  --  Interface
;;;
;;;    Return the DYNCOUNT-INFO for FUNCTION.
;;;
(defun find-info-for (function)
  (declare (type function function))
  (let* ((function (%primitive closure-function function))
	 (component (di::function-code-header function)))
    (do ((end (get-header-data component))
	 (i vm:code-constants-offset (1+ i)))
	((= end i))
      (let ((constant (code-header-ref component i)))
	(when (dyncount-info-p constant)
	  (return constant))))))


(defun vop-counts-apply (function args &key (spaces '(:dynamic)) by-space)
  "Apply Function to Args, collecting dynamic statistics on the running.
   Spaces are the spaces to scan for counts.  If By-Space is true, we return a
   list of result tables, instead of a single table.  In this case, specify
   :READ-ONLY first."
  (clear-vop-counts spaces)
  (apply function args)
  (if by-space
      (mapcar #'get-vop-counts spaces)
      (get-vop-counts spaces)))

;;;; Adjustments:

(defparameter *count-adjustments*
  '((return-multiple 152)
    (tail-call-variable 88)
    (unwind 92)
    (throw 116)
    (allocate-vector 72)
    (sxhash-simple-string 248)
    (sxhash-simple-substring 264)
    (copy-to-system-area 1200)
    (copy-from-system-area 1200)
    (system-area-copy 1204)
    (bit-bash-copy 1412)
    (vm::generic-+ 72)
    (vm::generic-- 72)
    (vm::generic-* 184)
    (vm::generic-< 68)
    (vm::generic-> 68)
    (vm::generic-eql 80)
    (vm::generic-= 80)
    (vm::generic-/= 104)
    (%make-weak-pointer 60)
    (make-value-cell 56)
    (vm::make-funcallable-instance 76)
    (make-closure 76)
    (make-complex 60)
    (make-ratio 60)
    (%allocate-bignum 72)
    (make-structure 72)
    (cons 50)))

;;; GET-VOP-COSTS  --  Public
;;;
(defun get-vop-costs ()
  "Return a hash-table mapping string VOP names to the cost recorded in the
   generator for all VOPs which are also the names of assembly routines."
  (let ((res (make-hash-table :test #'equal)))
     (do-hash (name v lisp::*assembler-routines*)
       (declare (ignore v))
       (let ((vop (gethash name (backend-template-names *backend*))))
	 (when vop
	   (setf (gethash (symbol-name name) res)
		 (template-cost (template-or-lose name))))))
    res))

(defvar *native-costs* (get-vop-costs)
  "Costs of assember routines on this machine.")


;;;; Analysis and report generation:

;;; COST-SUMMARY  --  Internal
;;;
;;;    Sum the count and costs.
;;;
(defun cost-summary (table)
  (let ((total-count 0d0)
	(total-cost 0d0))
    (do-hash (k v table)
      (declare (ignore k))
      (incf total-count (vop-stats-count v))
      (incf total-cost (vop-stats-cost v)))
    (values total-count total-cost)))

  
;;; COMPENSATE-COSTS  --  Internal
;;;
;;;    Return a hashtable of DYNCOUNT-INFO structures, with cost adjustments
;;; according to the Costs table.
;;;
(defun compensate-costs (table costs)
  (let ((res (make-hash-table-like table)))
    (do-hash (key value table)
      (unless (string= key "COUNT-ME")
	(let ((cost (gethash key costs)))
	  (if cost
	      (let* ((count (vop-stats-count value))
		     (sum (+ (* cost count)
			     (vop-stats-cost value))))
		(setf (gethash key res)
		      (make-vop-stats :name key :count count :cost sum)))
	      (setf (gethash key res) value)))))
    res))


;;; COMBINE-STATS  --  Internal
;;;
;;;    Take two tables of vop-stats and return a table of entries where the
;;; entries have been compared somehow.  The counts are normalized to Compared.
;;; The costs are the difference of the costs adjusted by the difference in
;;; counts: the cost for Original is modified to correspond to the count in
;;; Compared.
;;;
(defun combine-stats (original compared)
  (declare (type hash-table original compared))
  (let ((res (make-hash-table-like original)))
    (do-hash (k cv compared)
      (let ((ov (gethash k original)))
	(when ov
	  (let ((norm-cnt (/ (vop-stats-count ov) (vop-stats-count cv))))
	    (setf (gethash k res)
		  (make-vop-stats
		   :name k
		   :count norm-cnt
		   :cost (- (/ (vop-stats-cost ov) norm-cnt)
			    (vop-stats-cost cv))))))))
    res))


;;; SORT-RESULT  --  Internal
;;;
(defun sort-result (table by)
  (sort (hash-list table) #'>
	:key #'(lambda (x)
		 (abs (ecase by
			(:count (vop-stats-count x))
			(:cost (vop-stats-cost x)))))))


;;; GENERATE-REPORT  --  Public
;;;
;;; Generate a report from the specified table.
;;;
(defun generate-report (table &key (cut-off 15) (sort-by :cost)
			      (costs *native-costs*) compare)
  (let* ((compensated (if costs (compensate-costs table costs) table))
	 (compared (if compare
		       (combine-stats compensated compare)
		       compensated))
	 (*gc-verbose* nil))
    (multiple-value-bind (total-count total-cost)
			 (cost-summary (or compare compensated))
      (format t "~30<Vop~>  ~13<Count~> ~6<Cost~>  ~6:@<Percent~>~%")
      (dolist (entry (sort-result compared sort-by))
	(when (and cut-off (minusp (decf cut-off)))
	  (return))
	(let* ((cost (vop-stats-cost entry))
	       (name (vop-stats-name entry))
	       (entry-count (vop-stats-count entry))
	       (comp-entry (if compare (gethash name compare) entry))
	       (count (vop-stats-count comp-entry)))
	  (format t "~30<~A~>: ~:[~13:D~;~13,2F~] ~6,1F  ~4,1,2F%~%"
		  (vop-stats-name entry)
		  compare
		  (if compare entry-count (round entry-count))
		  (/ cost count)
		  (/ cost total-cost))))
	(format t "~%Total count ~,3E, total cost ~,3E.~%"
		total-count total-cost)))
  (values))


;;; STATS-{READER,WRITER}  --  Public
;;;
;;;    Read & write VOP stats using hash IO utility.
;;;
(defun stats-reader (stream key)
  (make-vop-stats :name key :count (read stream) :cost (read stream)))
;;;
(defun stats-writer (object stream)
  (format stream "~S ~S" (vop-stats-count object) (vop-stats-cost object)))
