;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Hashing and hash table functions for Spice Lisp.
;;; Written by Skef Wholey.
;;;
(in-package 'lisp)
(export '(hash-table hash-table-p make-hash-table
	  gethash remhash maphash clrhash
	  hash-table-count sxhash))

;;; Vector subtype codes.

(defconstant valid-hashing 2)
(defconstant must-rehash 3)


;;; What a hash-table is:

(defstruct (hash-table (:constructor make-hash-table-structure)
		       (:conc-name hash-table-)
		       (:print-function %print-hash-table))
  "Structure used to implement hash tables."
  (kind 'eq)
  (size 65 :type fixnum)
  (rehash-size 101)				; might be a float
  (rehash-threshold 57 :type fixnum)
  (number-entries 0 :type fixnum)
  (table () :type simple-vector))

;;; A hash-table-table is a vector of association lists.  When an
;;; entry is made in a hash table, a pair of (key . value) is consed onto
;;; the element in the vector arrived at by hashing.

;;; How to print one:

(defun %print-hash-table (structure stream depth)
  (declare (ignore depth))
  (format stream "#<~A Hash Table {~X}>"
	  (symbol-name (hash-table-kind structure))
	  (system:%primitive lisp::make-fixnum structure)))



;;; Hashing functions for the three kinds of hash tables:

(eval-when (compile)

(defmacro eq-hash (object)
  "Gives us a hashing of an object such that (eq a b) implies
   (= (eq-hash a) (eq-hash b))"
  `(%primitive make-fixnum ,object))

(defmacro eql-hash (object)
  "Gives us a hashing of an object such that (eql a b) implies
   (= (eql-hash a) (eql-hash b))"
  `(if (numberp ,object)
       (logand (truncate ,object) most-positive-fixnum)
       (%primitive make-fixnum ,object)))

(defmacro equal-hash (object)
  "Gives us a hashing of an object such that (equal a b) implies
   (= (equal-hash a) (equal-hash b))"
  `(sxhash ,object))

)

;;; Rehashing functions:

(defun almost-primify (num)
  (declare (fixnum num))
  "Almost-Primify returns an almost prime number greater than or equal
   to NUM."
  (if (= (rem num 2) 0)
      (setq num (+ 1 num)))
  (if (= (rem num 3) 0)
      (setq num (+ 2 num)))
  (if (= (rem num 7) 0)
      (setq num (+ 4 num)))
  num)

(eval-when (compile)

(defmacro grow-size (table)
  "Returns a fixnum for the next size of a growing hash-table."
  `(let ((rehash-size (hash-table-rehash-size ,table)))
     (if (floatp rehash-size)
	 (ceiling (* rehash-size (hash-table-size ,table)))
	 (+ rehash-size (hash-table-size ,table)))))

(defmacro grow-rehash-threshold (table new-length)
  "Returns the next rehash threshold for the table."
  table
  `,new-length
;  `(ceiling (* (hash-table-rehash-threshold ,table)
;	       (/ ,new-length (hash-table-size ,table))))
  )

(defmacro hash-set (vector key value length hashing-function)
  "Used for rehashing.  Enters the value for the key into the vector
   by hashing.  Never grows the vector.  Assumes the key is not yet
   entered."
  `(let ((index (rem (the fixnum (funcall ,hashing-function ,key))
		     (the fixnum ,length))))
     (declare (fixnum index))
     (setf (aref (the simple-vector ,vector) index)
	   (cons (cons ,key ,value)
		 (aref (the simple-vector ,vector) index)))))

)

(defun rehash (structure hash-vector new-length)
  (declare (simple-vector hash-vector))
  (declare (fixnum new-length))
  "Rehashes a hash table and replaces the TABLE entry in the structure if
   someone hasn't done so already.  New vector is of NEW-LENGTH."
  (do ((new-vector (make-array new-length))
       (i 0 (1+ i))
       (size (hash-table-size structure))
       (hashing-function (case (hash-table-kind structure)
			   (eq #'(lambda (x) (eq-hash x)))
			   (eql #'(lambda (x) (eql-hash x)))
			   (equal #'(lambda (x) (equal-hash x))))))
      ((= i size)
       (cond ((eq hash-vector (hash-table-table structure))
	      (cond ((> new-length size)
		     (setf (hash-table-table structure) new-vector)
		     (setf (hash-table-rehash-threshold structure)
			   (grow-rehash-threshold structure new-length))
		     (setf (hash-table-size structure) new-length))
		    (t
		     (setf (hash-table-table structure) new-vector)))
	      (if (not (eq (hash-table-kind structure) 'equal))
		  (%primitive set-vector-subtype new-vector
			      valid-hashing)))))
    (declare (fixnum i size))
    (do ((bucket (aref hash-vector i) (cdr bucket)))
	((null bucket))
      (hash-set new-vector (caar bucket) (cdar bucket) new-length
		hashing-function))
    (setf (aref hash-vector i) nil)))

;;; Macros for Gethash, %Puthash, and Remhash:

(eval-when (compile)

;;; Hashop dispatches on the kind of hash table we've got, rehashes if
;;; necessary, and binds Vector to the hash vector, Index to the index
;;; into that vector that the Key points to, and Size to the size of the
;;; hash vector.  Since Equal hash tables only need to be maybe rehashed
;;; sometimes, one can tell it if it's one of those times with the
;;; Equal-Needs-To-Rehash-P argument.

(defmacro hashop (equal-needs-to-rehash-p eq-body eql-body equal-body)
  `(let* ((vector (hash-table-table hash-table))
	  (size (length vector)))
     (declare (simple-vector vector) (fixnum size)
	      (inline assoc))
     (case (hash-table-kind hash-table)
       (equal
	,@(if equal-needs-to-rehash-p `((equal-rehash-if-needed)))
	(let ((index (rem (the fixnum (equal-hash key)) size)))
	  (declare (fixnum index))
	  ,equal-body))
       (eq
	(without-gcing
	  (eq-rehash-if-needed)
	  (let ((index (rem (the fixnum (eq-hash key)) size)))
	    (declare (fixnum index))
	    ,eq-body)))
       (eql
	(without-gcing
	  (eq-rehash-if-needed)
	  (let ((index (rem (the fixnum (eql-hash key)) size)))
	    (declare (fixnum index))
	    ,eql-body))))))

(defmacro eq-rehash-if-needed ()
  `(let ((subtype (%primitive get-vector-subtype vector)))
     (declare (fixnum subtype))
     (cond ((/= subtype valid-hashing)
	    (rehash hash-table vector size)
	    (setq vector (hash-table-table hash-table)))
	   ((> (hash-table-number-entries hash-table)
	       (hash-table-rehash-threshold hash-table))
	    (rehash hash-table vector (grow-size hash-table))
	    (setq vector (hash-table-table hash-table))
	    (setq size (length vector))))))

(defmacro equal-rehash-if-needed ()
  `(cond ((> (hash-table-number-entries hash-table)
	     (hash-table-rehash-threshold hash-table))
	  (rehash hash-table vector (grow-size hash-table))
	  (setq vector (hash-table-table hash-table))
	  (setq size (length vector)))))

(defmacro rehash-if-needed ()
  `(let ((subtype (%primitive get-vector-subtype vector))
	 (size (length vector)))
     (declare (fixnum subtype size))
     (cond ((and (not (eq (hash-table-kind hash-table) 'equal))
		 (/= subtype valid-hashing))
	    (rehash hash-table vector size)
	    (setq vector (hash-table-table hash-table))
	    (setq size (length vector)))
	   ((> (hash-table-number-entries hash-table)
	       (hash-table-rehash-threshold hash-table))
	    (rehash hash-table vector (grow-size hash-table))
	    (setq vector (hash-table-table hash-table))
	    (setq size (length vector))))))

)

;;; Making hash tables:

(defun make-hash-table (&key (test 'eql) (size 65) (rehash-size 101)
			     rehash-threshold)
  "Creates and returns a hash table.  See manual for details."
  (declare (fixnum size))
  (cond ((eq test #'eq) (setq test 'eq))
	((eq test #'eql) (setq test 'eql))
	((eq test #'equal) (setq test 'equal)))
  (if (not (member test '(eq eql equal) :test #'eq))
      (error "~S is an illegal :Test for hash tables." test))
  (setq size (if (<= size 37) 37 (almost-primify size)))
  (cond ((null rehash-threshold)
	 (setq rehash-threshold size))
	((floatp rehash-threshold)
	 (setq rehash-threshold (ceiling (* rehash-threshold size)))))
  (make-hash-table-structure :size size
			     :rehash-size rehash-size
			     :rehash-threshold rehash-threshold
			     :table
			     (if (eq test 'equal)
				 (make-array size)
				 (%primitive set-vector-subtype
					     (make-array size)
					     valid-hashing))
			     :kind test)))

;;; Manipulating hash tables:

(defun gethash (key hash-table &optional default)
  "Finds the entry in Hash-Table whose key is Key and returns the associated
   value and T as multiple values, or returns Default and Nil if there is no
   such entry."
  (macrolet ((lookup (test)
	       `(let ((cons (assoc key (aref vector index) :test #',test)))
		  (declare (list cons))
		  (if cons
		      (values (cdr cons) t)
		      (values default nil)))))
    (hashop nil
      (lookup eq)
      (lookup eql)
      (lookup equal))))

(defun %puthash (key hash-table value)
  "Create an entry in HASH-TABLE associating KEY with VALUE; if there already
   is an entry for KEY, replace it.  Returns VALUE."
  (macrolet ((store (test)
	       `(let ((cons (assoc key (aref vector index) :test #',test)))
		  (declare (list cons))
		  (cond (cons (setf (cdr cons) value))
			(t
			 (push (cons key value) (aref vector index))
			 (incf (hash-table-number-entries hash-table))
			 value)))))
    (hashop t
      (store eq)
      (store eql)
      (store equal))))

(defun remhash (key hash-table)
  "Remove any entry for KEY in HASH-TABLE.  Returns T if such an entry
   existed; () otherwise."
  (hashop nil
   (let ((bucket (aref vector index)))		; EQ case
     (cond ((and bucket (eq (caar bucket) key))
	    (pop (aref vector index))
	    (decf (hash-table-number-entries hash-table))
	    t)
	   (t
	    (do ((last bucket bucket)
		 (bucket (cdr bucket) (cdr bucket)))
		((null bucket) ())
	      (when (eq (caar bucket) key)
		(rplacd last (cdr bucket))
		(decf (hash-table-number-entries hash-table))
		(return t))))))
   (let ((bucket (aref vector index)))		; EQL case
     (cond ((and bucket (eql (caar bucket) key))
	    (pop (aref vector index))
	    (decf (hash-table-number-entries hash-table))
	    t)
	   (t
	    (do ((last bucket bucket)
		 (bucket (cdr bucket) (cdr bucket)))
		((null bucket) ())
	      (when (eql (caar bucket) key)
		(rplacd last (cdr bucket))
		(decf (hash-table-number-entries hash-table))
		(return t))))))
   (let ((bucket (aref vector index)))		; EQUAL case
     (cond ((and bucket (equal (caar bucket) key))
	    (pop (aref vector index))
	    (decf (hash-table-number-entries hash-table))
	    t)
	   (t
	    (do ((last bucket bucket)
		 (bucket (cdr bucket) (cdr bucket)))
		((null bucket) ())
	      (when (equal (caar bucket) key)
		(rplacd last (cdr bucket))
		(decf (hash-table-number-entries hash-table))
		(return t))))))))

(defun maphash (map-function hash-table)
  "For each entry in HASH-TABLE, calls MAP-FUNCTION on the key and value
  of the entry; returns T."
  (let ((vector (hash-table-table hash-table)))
    (declare (simple-vector vector))
    (rehash-if-needed)
    (do ((i 0 (1+ i))
	 (size (hash-table-size hash-table)))
	((= i size))
      (declare (fixnum i size))
      (do ((bucket (aref vector i) (cdr bucket)))
	  ((null bucket))
	
	(funcall map-function (caar bucket) (cdar bucket))))))

(defun clrhash (hash-table)
  "Removes all entries of HASH-TABLE and returns the hash table itself."
  (let ((vector (hash-table-table hash-table)))
    (declare (simple-vector vector))
    (setf (hash-table-number-entries hash-table) 0)
    (do ((i 0 (1+ i))
	 (size (hash-table-size hash-table)))
	((= i size) hash-table)
      (declare (fixnum i size))
      (setf (aref vector i) nil))))

(defun hash-table-count (hash-table)
  "Returns the number of entries in the given Hash-Table."
  (hash-table-number-entries hash-table))

;;; Primitive Hash Function

;;; The maximum length and depth to which we hash lists.
(defconstant sxhash-max-len 7)
(defconstant sxhash-max-depth 3)

(eval-when (compile eval)


(defconstant sxmash-total-bits 26)
(defconstant sxmash-rotate-bits 7)

(defmacro sxmash (place with)
  (let ((n-with (gensym)))
    `(let ((,n-with ,with))
       (declare (fixnum ,n-with))
       (setf ,place
	     (logxor (ash ,n-with ,(- sxmash-rotate-bits sxmash-total-bits))
		     (ash (logand ,n-with
				  ,(1- (ash 1
					    (- sxmash-total-bits
					       sxmash-rotate-bits))))
			  ,sxmash-rotate-bits)
		     (the fixnum ,place))))))

(defmacro sxhash-simple-string (sequence)
  `(%primitive sxhash-simple-string ,sequence))

(defmacro sxhash-string (sequence)
  (let ((data (gensym))
	(start (gensym))
	(end (gensym)))
    `(with-array-data ((,data ,sequence)
		       (,start)
		       (,end))
       (if (zerop ,start)
	   (%primitive sxhash-simple-substring ,data ,end)
	   (sxhash-simple-string (coerce (the string ,sequence)
					 'simple-string))))))

(defmacro sxhash-list (sequence depth)
  `(if (= ,depth sxhash-max-depth)
       0
       (do ((sequence ,sequence (cdr (the list sequence)))
	    (index 0 (1+ index))
	    (hash 2))
	   ((or (atom sequence) (= index sxhash-max-len)) hash)
	 (declare (fixnum hash index))
	 (sxmash hash (internal-sxhash (car sequence) (1+ ,depth))))))


); eval-when (compile eval)


(defun sxhash (s-expr)
  "Computes a hash code for S-EXPR and returns it as an integer."
  (internal-sxhash s-expr 0))


(defun internal-sxhash (s-expr depth)
  (typecase s-expr
    ;; The pointers and immediate types.
    (list (sxhash-list s-expr depth))
    (fixnum (abs s-expr))
    #+nil
    (structure ???)
    ;; Other-pointer types.
    (simple-string (sxhash-simple-string s-expr))
    (symbol (sxhash-simple-string (symbol-name s-expr)))
    (number
     (etypecase s-expr
       (integer (ldb (byte 23 0) s-expr))
       (float (multiple-value-bind (significand exponent)
				   (integer-decode-float s-expr)
		(logxor (the fixnum (ldb (byte 23 0) significand))
			(the fixnum (ldb (byte 23 0) exponent)))))
       (ratio (the fixnum (+ (internal-sxhash (numerator s-expr) 0)
			     (internal-sxhash (denominator s-expr) 0))))
       (complex (the fixnum (+ (internal-sxhash (realpart s-expr) 0)
			       (internal-sxhash (imagpart s-expr) 0))))))
    (array
     (typecase s-expr
       (string (sxhash-string s-expr))
       (t (array-rank s-expr))))
    #+nil
    (compiled-function (%primitive header-length s-expr))
    ;; Everything else.
    (t (%primitive make-fixnum s-expr))))
