;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;
;;;    This file contains stuff that knows how to load compiled code directly
;;; into core, e.g. incremental compilation.
;;;
(in-package 'c)


;;; The CORE-OBJECT structure holds the state needed to resolve cross-component
;;; references during in-core compilation.
;;;
(defstruct (core-object
	    (:constructor make-core-object ())
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (format stream "#<Core-Object>"))))
  ;;
  ;; A hashtable translating ENTRY-INFO structures to the corresponding actual
  ;; FUNCTIONs for functions in this compilation.
  (entry-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; A hashtable translating ENTRY-INFO structures to a list of pairs
  ;; (<code object> . <offset>) describing the places that need to be
  ;; backpatched to point to the function for ENTRY-INFO.
  (patch-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; A list of all the DEBUG-INFO objects created, kept so that we can
  ;; backpatch with the source info.
  (debug-info () :type list))


;;; MAKE-FUNCTION-ENTRY  --  Internal
;;;
;;;    Make a function entry, filling in slots from the ENTRY-INFO.
;;;
#+new-compiler
(defun make-function-entry (entry code-obj object)
  (declare (type entry-info entry) (type core-object object))
  (let ((offset (label-position (entry-info-offset entry))))
    (declare (type index offset))
    (unless (zerop (logand offset vm:lowtag-mask))
      (error "Unaligned function object, offset = #x~X." offset))
    (let* ((res (%primitive compute-function code-obj offset))
	   (patch-table (core-object-patch-table object)))
      (%primitive set-function-self res res)
      (%primitive set-function-next res
		  (%primitive code-entry-points code-obj))
      (%primitive set-code-entry-points code-obj res)
      (%primitive set-function-name res (entry-info-name entry))
      (%primitive set-function-arglist res
		  (entry-info-arguments entry))
      (%primitive set-function-type res
		  (entry-info-type entry))

      (dolist (patch (gethash entry patch-table))
	(%primitive code-constant-set (car patch) (the index (cdr patch))
		    res))
      (remhash entry patch-table)
      (setf (gethash entry (core-object-entry-table object)) res)))
  (undefined-value))


;;; DO-CORE-FIXUPS  --  Internal
;;;
;;;    Do "load-time" fixups on the code vector.
;;;
#+new-compiler
(defun do-core-fixups (code fixups)
  (declare (list fixups))
  (dolist (info fixups)
    (let* ((kind (first info))
	   (fixup (second info))
	   (name (fixup-name fixup))
	   (flavor (fixup-flavor fixup))
	   (offset (third info)))
      (multiple-value-bind
	  (value found)
	  (ecase flavor
	    (:assembly-routine
	     (assert (symbolp name))
	     (gethash name lisp::*assembler-routines*))
	    (:foreign
	     (assert (stringp name))
	     (gethash name lisp::*foreign-symbols*)))
	(unless found
	  (error (ecase flavor
		   (:assembly-routine "Undefined assembler routine: ~S")
		   (:foreign "Unknown foreign symbol: ~S"))
		 name))
	(lisp::fixup-code-object code offset value kind)))))


;;; REFERENCE-CORE-FUNCTION  --  Internal
;;;
;;;    Stick a reference to the function Fun in Code-Object at index I.  If the
;;; function hasn't been compiled yet, make a note in the Patch-Table.
;;;
(defun reference-core-function (code-obj i fun object)
  (declare (type core-object object) (type functional fun)
	   (type index i))
  (let* ((info (leaf-info fun))
	 (found (gethash info (core-object-entry-table object))))
    (if found
	(%primitive code-constant-set code-obj i found)
	(push (cons code-obj i)
	      (gethash info (core-object-patch-table object)))))
  (undefined-value))


;;; MAKE-CORE-COMPONENT  --  Interface
;;;
;;;    Dump a component to core.  We pass in the assembler fixups, code vector
;;; and node info.
;;;
#+new-compiler
(defun make-core-component (component segment length object)
  (declare (type component component)
	   (type index length)
	   (type core-object object))
  (without-gcing
    (let* ((2comp (component-info component))
	   (constants (ir2-component-constants 2comp))
	   (box-num (- (length constants) vm:code-constants-offset))
	   (code-obj (%primitive allocate-code-object box-num length))
	   (fixups (emit-code-vector (make-code-instruction-stream code-obj)
				     segment)))
      (do-core-fixups code-obj fixups)
      
      (dolist (entry (ir2-component-entries 2comp))
	(make-function-entry entry code-obj object))
      
      (let ((info (debug-info-for-component component)))
	(push info (core-object-debug-info object))
	(%primitive set-code-debug-info code-obj info))
      
      (dotimes (i box-num)
	(let ((const (aref constants (+ i vm:code-constants-offset))))
	  (etypecase const
	    (null)
	    (constant
	     (%primitive code-constant-set code-obj i
			 (constant-value const)))
	    (list
	     (ecase (car const)
	       (:entry
		(reference-core-function code-obj i (cdr const) object))
	       #+nil
	       (:label
		(%primitive header-set code-obj i
			    (+ (label-location (cdr const))
				clc::i-vector-header-size))))))))))
  (undefined-value))


;;; CORE-CALL-TOP-LEVEL-LAMBDA  --  Interface
;;;
;;;    Call the top-level lambda function dumped for Entry, returning the
;;; values.  Entry may be a :TOP-LEVEL-XEP functional.
;;;
#+new-compiler
(defun core-call-top-level-lambda (entry object)
  (declare (type functional entry) (type core-object object))
  (funcall (or (gethash (leaf-info entry)
			(core-object-entry-table object))
	       (error "Unresolved forward reference."))))


;;; FIX-CORE-SOURCE-INFO  --  Interface
;;;
;;;    Backpatch all the DEBUG-INFOs dumped so far with the specified
;;; SOURCE-INFO list.  We also check that there are no outstanding forward
;;; references to functions.
;;;
#+new-compiler
(defun fix-core-source-info (info object source-info)
  (declare (type source-info info) (type core-object object))
  (assert (zerop (hash-table-count (core-object-patch-table object))))
  (let ((res (debug-source-for-info info)))
    (dolist (sinfo res)
      (setf (debug-source-info sinfo) source-info))
    (dolist (info (core-object-debug-info object))
      (setf (compiled-debug-info-source info) res))
    (setf (core-object-debug-info object) ()))
  (undefined-value))


;;;; Code-instruction-streams

#+new-compiler
(defstruct (code-instruction-stream
	    (:print-function %print-code-inst-stream)
	    (:include stream
		      (lisp::sout #'code-inst-stream-sout)
		      (lisp::misc #'code-inst-stream-misc))
	    (:constructor make-code-instruction-stream
			  (code-object
			   &aux
			   (current (truly-the system-area-pointer
					       (%primitive code-instructions
							   code-object)))
			   (end (sap+ current
				      (* (%primitive code-code-size
						     code-object)
					 vm:word-bytes))))))
  code-object
  current
  end)

#+new-compiler
(defun %print-code-inst-stream (code-inst-stream stream depth)
  (declare (ignore depth))
  (format stream "#<Code Instruction Stream for ~S>"
	  (code-instruction-stream-code-object code-inst-stream)))

#+new-compiler
(defun code-inst-stream-sout (stream string start end)
  (let* ((start (or start 0))
	 (end (or end (length string)))
	 (length (- end start))
	 (current (code-instruction-stream-current stream))
	 (new (sap+ current length)))
    (when (pointer> new (code-instruction-stream-end stream))
      (error "Writing ~D bytes to ~S would cause it to overflow."
	     length stream))
    (copy-to-system-area string (+ (* start vm:byte-bits)
				   (* vm:vector-data-offset vm:word-bits))
			 current 0
			 (* length vm:byte-bits))
    (setf (code-instruction-stream-current stream) new)))

#+new-compiler
(defun code-inst-stream-misc (stream method &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case method
    (:close
     (lisp::set-closed-flame stream))
    (t
     nil)))
