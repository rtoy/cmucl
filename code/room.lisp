;;; -*- Mode: Lisp; Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (Scott.Fahlman@CS.CMU.EDU). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/room.lisp,v 1.2 1990/09/27 06:11:22 wlott Exp $
;;; 
;;; Heap grovelling memory usage stuff.
;;; 
(in-package "VM")
(use-package "SYSTEM")
(in-package "C")
(import '(function-code-header make-lisp-obj dynamic-space-free-pointer
	  code-code-size vector-length)
	"VM")
(in-package "LISP")
(import '(
	  dynamic-0-space-start dynamic-1-space-start read-only-space-start
	  static-space-start current-dynamic-space-start
	  *static-space-free-pointer* *read-only-space-free-pointer*)
	"VM")
(in-package "VM")


;;;; Type format database.

(defstruct room-info
  ;;
  ;; The name of this type.
  (name nil :type symbol)
  ;;
  ;; Kind of type (how we determine length).
  (kind nil :type (member :lowtag :fixed :header :vector
			  :string :code :closure))
  ;;
  ;; Length if fixed-length, shift amount for element size if :vector.
  (length nil :type (or fixnum null)))

(defvar *room-info* (make-array 256 :initial-element nil))


(dolist (obj *primitive-objects*)
  (let ((header (primitive-object-header obj))
	(lowtag (primitive-object-lowtag obj))
	(name (primitive-object-name obj))
	(variable (primitive-object-variable-length obj))
	(size (primitive-object-size obj)))
    (cond
     ((not lowtag))
     ((not header)
      (let ((info (make-room-info :name name  :kind :lowtag))
	    (lowtag (symbol-value lowtag)))
	(declare (fixnum lowtag))
	(dotimes (i 32)
	  (setf (svref *room-info* (logior lowtag (ash i 3))) info))))
     (variable)
     (t
      (setf (svref *room-info* (symbol-value header))
	    (make-room-info :name name  :kind :fixed  :length size))))))

(dolist (code (list complex-string-type simple-array-type
		    complex-bit-vector-type complex-vector-type 
		    complex-array-type))
  (setf (svref *room-info* code)
	(make-room-info :name 'array-header  :kind :header)))

(setf (svref *room-info* bignum-type)
      (make-room-info :name 'bignum  :kind :header))

(setf (svref *room-info* closure-header-type)
      (make-room-info :name 'closure  :kind :closure))

(dolist (stuff '((simple-bit-vector-type . -3)
		 (simple-vector-type . 2)
		 (simple-array-unsigned-byte-2-type . -2)
		 (simple-array-unsigned-byte-4-type . -1)
		 (simple-array-unsigned-byte-8-type . 0)
		 (simple-array-unsigned-byte-16-type . 1)
		 (simple-array-unsigned-byte-32-type . 2)
		 (simple-array-single-float-type . 2)
		 (simple-array-double-float-type . 3)))
  (let ((name (car stuff))
	(size (cdr stuff)))
    (setf (svref *room-info* (symbol-value name))
	  (make-room-info :name name  :kind :vector  :length size))))

(setf (svref *room-info* simple-string-type)
      (make-room-info :name 'simple-string-type :kind :string :length 0))

(setf (svref *room-info* code-header-type)
      (make-room-info :name 'code  :kind :code))

(deftype spaces () '(member :static :dynamic :read-only))


;;;; MAP-ALLOCATED-OBJECTS:

(proclaim '(type fixnum *static-space-free-pointer*
		 *read-only-space-free-pointer* ))

(defun space-bounds (space)
  (declare (type spaces space))
  (ecase space
    (:static
     (values (int-sap (static-space-start))
	     (int-sap (* *static-space-free-pointer* word-bytes))))
    (:read-only
     (values (int-sap (read-only-space-start))
	     (int-sap (* *read-only-space-free-pointer* word-bytes))))
    (:dynamic
     (values (int-sap (current-dynamic-space-start))
	     (dynamic-space-free-pointer)))))


;;; ROUND-TO-DUALWORD  --  Internal
;;;
;;;    Round Size (in bytes) up to the next dualword (eight byte) boundry.
;;;
(proclaim '(inline round-to-dualword))
(defun round-to-dualword (size)
  (declare (fixnum size))
  (logand (the fixnum (+ size lowtag-mask)) (lognot lowtag-mask)))


;;; VECTOR-TOTAL-SIZE  --  Internal
;;;
;;;    Return the total size of a vector in bytes, including any pad.
;;;
(proclaim '(inline vector-total-size))
(defun vector-total-size (obj info)
  (let ((shift (room-info-length info))
	(len (+ (vector-length obj)
		(ecase (room-info-kind info)
		  (:vector 0)
		  (:string 1)))))
    (declare (type (integer -3 3) shift))
    (round-to-dualword
     (+ (* vector-data-offset word-bytes)
	(the fixnum
	     (if (minusp shift)
		 (ash (the fixnum
			   (+ len (the fixnum
				       (1- (the fixnum (ash 1 (- shift)))))))
		      shift)
		 (ash len shift)))))))


;;; MAP-ALLOCATED-OBJECTS  --  Interface
;;;
;;;    Iterate over all the objects allocated in Space, calling Fun with the
;;; object, the object's type code, and the objects total size in bytes,
;;; including any header and padding.
;;;
(proclaim '(maybe-inline map-allocated-objects))
(defun map-allocated-objects (fun space)
  (declare (type function fun) (type spaces space))
  (multiple-value-bind (start end)
		       (space-bounds space)
    (declare (optimize (speed 3) (safety 0)))
    (let ((current start)
	  (prev nil))
      (loop
	(let* ((header (sap-ref-32 current 0))
	       (header-type (logand header #xFF))
	       (info (svref *room-info* header-type)))
	  (cond
	   ((or (not info)
		(eq (room-info-kind info) :lowtag))
	    (let ((size (* cons-size word-bytes)))
	      (funcall fun
		       (make-lisp-obj (logior (sap-int current)
					      list-pointer-type))
		       list-pointer-type
		       size)
	      (setq current (sap+ current size))))
	   ((eql header-type closure-header-type)
	    (let* ((obj (make-lisp-obj (logior (sap-int current)
					       function-pointer-type)))
		   (size (round-to-dualword
			  (* (the fixnum (1+ (get-closure-length obj)))
			     word-bytes))))
	      (funcall fun obj header-type size)
	      (setq current (sap+ current size))))
	   (t
	    (let* ((obj (make-lisp-obj
			 (logior (sap-int current) other-pointer-type)))
		   (size (ecase (room-info-kind info)
			   (:fixed
			    (assert (or (eql (room-info-length info)
					     (1+ (get-header-data obj)))
					(floatp obj)))
			    (round-to-dualword
			     (* (room-info-length info) word-bytes)))
			   ((:vector :string)
			    (vector-total-size obj info))
			   (:header
			    (round-to-dualword
			     (* (1+ (get-header-data obj)) word-bytes)))
			   (:code
			    (+ (the fixnum
				    (* (get-header-data obj) word-bytes))
			       (round-to-dualword
				(* (the fixnum
					(%primitive code-code-size obj))
				   word-bytes)))))))
	      (declare (fixnum size))
	      (funcall fun obj header-type size)
	      (assert (zerop (logand size lowtag-mask)))
	      (when (> size 200000) (break "Implausible size, prev ~S" prev))
	      (setq prev current)
	      (setq current (sap+ current size))))))
	(unless (pointer< current end)
	  (assert (not (pointer> current end)))
	  (return)))

      prev)))


;;;; MEMORY-USAGE:

;;; TYPE-BREAKDOWN  --  Interface
;;;
;;;    Return a list of 3-lists (bytes object type-name) for the objects
;;; allocated in Space.
;;;
(defun type-breakdown (space)
  (let ((sizes (make-array 256 :initial-element 0 :element-type 'fixnum))
	(counts (make-array 256 :initial-element 0 :element-type 'fixnum))
	(structure-count 0)
	(structure-size 0))
    (declare (fixnum structure-size structure-count))
    (map-allocated-objects
     #'(lambda (obj type size)
	 (declare (fixnum size) (optimize (speed 3) (safety 0)))
	 (cond ((and (eql type simple-vector-type)
		     (eql (get-header-data obj) vector-structure-subtype))
		(incf structure-count)
		(incf structure-size size))
	       (t
		(incf (aref sizes type) size)
		(incf (aref counts type)))))
     space)

    (let ((totals (make-hash-table :test #'eq)))
      (dotimes (i 256)
	(let ((total-count (aref counts i)))
	  (unless (zerop total-count)
	    (let* ((total-size (aref sizes i))
		   (name (room-info-name (aref *room-info* i)))
		   (found (gethash name totals)))
	      (cond (found
		     (incf (first found) total-size)
		     (incf (second found) total-count))
		    (t
		     (setf (gethash name totals)
			   (list total-size total-count name))))))))

      (collect ((totals-list))
	(maphash #'(lambda (k v)
		     (declare (ignore k))
		     (totals-list v))
		 totals)
	(totals-list (list structure-size structure-count 'structure))
	(sort (totals-list) #'> :key #'first)))))


;;; PRINT-SUMMARY  --  Internal
;;;
;;;    Handle the summary printing for MEMORY-USAGE.  Totals is a list of lists
;;; (space-name . totals-for-space), where totals-for-space is the list
;;; returned by TYPE-BREAKDOWN.
;;;
(defun print-summary (spaces totals)
  (let ((summary (make-hash-table :test #'eq)))
    (dolist (space-total totals)
      (dolist (total (cdr space-total))
	(push (cons (car space-total) total)
	      (gethash (third total) summary))))

    (collect ((summary-totals))
      (maphash #'(lambda (k v)
		   (declare (ignore k))
		   (let ((sum 0))
		     (declare (fixnum sum))
		     (dolist (space-total v)
		       (incf sum (first (cdr space-total))))
		     (summary-totals (cons sum v))))
	       summary)
      
      (format t "~2&Summary of spaces: ~(~{~A ~}~)~%" spaces)
      (let ((summary-total-bytes 0)
	    (summary-total-objects 0))
	(declare (fixnum summary-total-bytes summary-total-objects))
	(dolist (space-totals
		 (mapcar #'cdr (sort (summary-totals) #'> :key #'car)))
	  (let ((total-objects 0)
		(total-bytes 0)
		name)
	    (declare (fixnum total-objects total-bytes))
	    (collect ((spaces))
	      (dolist (space-total space-totals)
		(let ((total (cdr space-total)))
		  (setq name (third total))
		  (incf total-bytes (first total))
		  (incf total-objects (second total))
		  (spaces (cons (car space-total) (first total)))))
	      (format t "~%~A:~%    ~:D bytes, ~:D object~:P"
		      name total-bytes total-objects)
	      (dolist (space (spaces))
		(format t ", ~D% ~(~A~)"
			(round (* (cdr space) 100) total-bytes)
			(car space)))
	      (format t ".~%")
	      (incf summary-total-bytes total-bytes)
	      (incf summary-total-objects total-objects))))
	(format t "~%Summary total:~%    ~:D bytes, ~:D objects.~%"
		summary-total-bytes summary-total-objects)))))


;;; MEMORY-USAGE  --  Public
;;;
(defun memory-usage (&key print-spaces (count-spaces '(:dynamic))
			  (print-summary t))
  "Print out information about the heap memory in use.  :Print-Spaces is a list
  of the spaces to print detailed information for.  :Count-Spaces is a list of
  the spaces to scan.  For either one, T means all spaces (:Static, :Dyanmic
  and :Read-Only.)  If :Print-Summary is true, then summary information will be
  printed.  The defaults print only summary information for dynamic space."
  (let* ((spaces (if (eq count-spaces t)
		     '(:static :dynamic :read-only)
		     count-spaces))
	 (totals (mapcar #'(lambda (space)
			     (cons space (type-breakdown space)))
			 spaces)))

    (dolist (space-total totals)
      (when (or (eq print-spaces t)
		(member (car space-total) print-spaces))
	(format t "~2&Breakdown for ~(~A~) space:~2%" (car space-total))
	(let ((total-objects 0)
	      (total-bytes 0))
	  (declare (fixnum total-objects total-bytes))
	  (dolist (total (cdr space-total))
	    (incf total-bytes (first total))
	    (incf total-objects (second total))
	    (format t "~%~A:~%    ~:D bytes, ~:D object~:P.~%"
		    (third total) (first total) (second total)))
	  (format t "~%Space total:~%    ~:D bytes, ~:D object~:P.~%"
		  total-bytes total-objects))))

    (when print-summary (print-summary spaces totals)))

  (values))


;;; COUNT-NO-OPS  --  Public
;;;
(defun count-no-ops (space)
  "Print info about how much code and no-ops there are in Space."
  (declare (type spaces space))
  (let ((code-words 0)
	(no-ops 0)
	(total-bytes 0))
    (declare (fixnum code-words no-ops))
    (map-allocated-objects
     #'(lambda (obj type size)
 	 (declare (fixnum size) (optimize (speed 3) (safety 0)))
	 (when (eql type code-header-type)
	   (incf total-bytes size)
	   (let ((words (truly-the fixnum (%primitive code-code-size obj)))
		 (sap (truly-the system-area-pointer
				 (%primitive code-instructions obj))))
	     (incf code-words words)
	     (dotimes (i words)
	       (when (zerop (sap-ref-32 sap i)) (incf no-ops))))))
     space)
    
    (format t
	    "~:D code-object bytes, ~:D code words, with ~:D no-ops (~D%).~%"
	    total-bytes code-words no-ops
	    (round (* no-ops 100) code-words)))
  
  (values))


;;; STRUCTURE-USAGE  --  Public
;;;
(defun structure-usage (space &key (top-n 15))
  (declare (type spaces space) (type (or fixnum null) top-n))
  "Print a breakdown by structure type of all the structures allocated in
  Space.  If TOP-N is true, print only information for the the TOP-N types with
  largest usage."
  (let ((totals (make-hash-table :test #'eq))
	(total-objects 0)
	(total-bytes 0))
    (declare (fixnum total-objects total-bytes))
    (map-allocated-objects
     #'(lambda (obj type size)
	 (declare (fixnum size) (optimize (speed 3) (safety 0)))
	 (when (and (eql type simple-vector-type)
		    (eql (get-header-data obj) vector-structure-subtype))
	   (incf total-objects)
	   (incf total-bytes size)
	   (let* ((name (svref obj 0))
		  (found (gethash name totals)))
	     (cond (found
		    (incf (the fixnum (car found)))
		    (incf (the fixnum (cdr found)) size))
		   (t
		    (setf (gethash name totals) (cons 1 size)))))))
     space)

    (collect ((totals-list))
      (maphash #'(lambda (name what)
		   (totals-list (cons name what)))
	       totals)
      (let ((sorted (sort (totals-list) #'> :key #'cddr))
	    (printed-bytes 0)
	    (printed-objects 0))
	(declare (fixnum printed-bytes printed-objects))
	(dolist (what (if top-n
			  (subseq sorted 0 (min (length sorted) top-n))
			  sorted))
	  (let ((bytes (cddr what))
		(objects (cadr what)))
	    (incf printed-bytes bytes)
	    (incf printed-objects objects)
	    (format t "~S: ~:D bytes, ~D object~:P.~%" (car what)
		    bytes objects)))

	(let ((residual-objects (- total-objects printed-objects))
	      (residual-bytes (- total-bytes printed-bytes)))
	  (unless (zerop residual-objects)
	    (format t "Other types: ~:D bytes, ~D: object~:P.~%"
		    residual-bytes residual-objects))))

      (format t "Structure total: ~:D bytes, ~:D object~:P.~%"
	      total-bytes total-objects)))

  (values))


;;; FIND-HOLES -- Public
;;; 
(defun find-holes (&rest spaces)
  (dolist (space (or spaces '(:read-only :static :dynamic)))
    (format t "In ~A space:~%" space)
    (let ((start-addr nil)
	  (total-bytes 0))
      (declare (type (or null (unsigned-byte 32)) start-addr)
	       (type (unsigned-byte 32) total-bytes))
      (map-allocated-objects
       #'(lambda (object typecode bytes)
	   (declare (ignore typecode)
		    (type (unsigned-byte 32) bytes))
	   (if (and (consp object)
		    (eql (car object) 0)
		    (eql (cdr object) 0))
	       (if start-addr
		   (incf total-bytes bytes)
		   (setf start-addr (di::get-lisp-obj-address object)
			 total-bytes bytes))
	       (when start-addr
		 (format t "~D bytes at #x~X~%" total-bytes start-addr)
		 (setf start-addr nil))))
       space)
      (when start-addr
	(format t "~D bytes at #x~X~%" total-bytes start-addr))))
  (values))
