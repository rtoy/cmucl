;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Functions to implement arrays for Spice Lisp 
;;; Written by Skef Wholey.
;;;
(in-package "LISP")

(export '(array-rank-limit array-dimension-limit array-total-size-limit
	  make-array vector aref array-element-type array-rank
	  array-dimension array-dimensions array-in-bounds-p
	  array-row-major-index array-total-size svref bit sbit
	  bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2
	  bit-orc1 bit-orc2 bit-not array-has-fill-pointer-p
	  fill-pointer vector-push vector-push-extend vector-pop adjust-array
          adjustable-array-p row-major-aref))

(defconstant array-rank-limit 65529
  "The exclusive upper bound on the rank of an array.")

(defconstant array-dimension-limit most-positive-fixnum
  "The exclusive upper bound any given dimension of an array.")

(defconstant array-total-size-limit most-positive-fixnum
  "The exclusive upper bound on the total number of elements in an array.")


;;; Random function used by WITH-ARRAY-DATA, which has to be defined in
;;; init.lisp.
;;;
(defun find-data-vector (array)
  (do ((data array (%primitive header-ref data %array-data-slot))
       (cumulative-offset 0
			  (the fixnum
			       (+ cumulative-offset
				  (the fixnum
				       (or (%primitive header-ref data
						       %array-displacement-slot)
					   0))))))
      ((not (array-header-p data))
       (values data cumulative-offset))
    (declare (fixnum cumulative-offset))))



(defun make-array (dimensions &key
			      (element-type t)
			      (initial-element nil initial-element-p)
			      initial-contents adjustable fill-pointer
			      displaced-to displaced-index-offset)
  "Creates an array of the specified Dimensions.  See manual for details."
  (unless (listp dimensions) (setq dimensions (list dimensions)))
  (let ((array-rank (length (the list dimensions))))
    (declare (fixnum array-rank))
    (if (eq fill-pointer t) (setq fill-pointer (car dimensions)))
    (if (and fill-pointer (> array-rank 1))
	(error "Multidimensional arrays can't have fill pointers."))
    (cond (displaced-to
	   ;; If the array is displaced, make a header and fill it up.
	   (unless (subtypep element-type (array-element-type displaced-to))
	     (error "One can't displace an array of type ~S into another of ~
		    type ~S." element-type (array-element-type displaced-to)))
	   (if (or initial-element initial-contents)
	       (error "The :initial-element or initial-contents option may not ~
		       be specified with :displaced-to."))
	   (let ((displacement (or displaced-index-offset 0))
		 (array-size (array-linear-length dimensions)))
	     (declare (fixnum displacement array-size))
	     (if (< (the fixnum (array-total-size displaced-to))
		    (the fixnum (+ displacement array-size)))
		 (error "The :displaced-to array is too small."))
	     (set-array-header (%primitive alloc-array array-rank)
			       displaced-to array-size
			       (or fill-pointer array-size)
			       displacement dimensions t)))
	  ((and (not adjustable) (= array-rank 1) (not fill-pointer))
	   ;; If the array can be represented as a simple thing, do that.
	   (if (and initial-element-p initial-contents)
	       (error "The :initial-contents option may not be specified with ~
		       :initial-element."))
	   (data-vector-from-inits dimensions (car dimensions) array-rank
				   element-type initial-contents
				   initial-element initial-element-p))
	  (t
	   ;; Otherwise, build a complex array.
	   (if (and initial-element-p initial-contents)
	       (error "The :initial-contents option may not be specified with ~
		       :initial-element."))
	   (let* ((array-size (array-linear-length dimensions))
		  (array (%primitive alloc-array array-rank))
		  (array-data (data-vector-from-inits
			       dimensions array-size array-rank element-type
			       initial-contents initial-element
			       initial-element-p)))
	     (set-array-header array array-data array-size
			       (or fill-pointer array-size)
			       0 ;displacement
			       dimensions nil))))))

;;; Some people out there are still calling MAKE-VECTOR:
;;;
(setf (symbol-function 'make-vector) #'make-array)

(defun vector (&rest objects)
  "Constructs a simple-vector from the given objects."
  (coerce (the list objects) 'simple-vector))

;;; DATA-VECTOR-FROM-INITS returns a simple vector that has the specified array
;;; characteristics.  Dimensions is only used to pass to COPY-CONTENTS-AUX
;;; for error checking on the structure of initial-contents.
;;;
(defun data-vector-from-inits (dimensions total-size rank element-type
			       initial-contents initial-element
			       initial-element-p)
  (let ((data (cond ((subtypep element-type 'string-char)
		     (%primitive alloc-string total-size))
		    ((subtypep element-type '(unsigned-byte 32))
		     (%primitive alloc-i-vector total-size
				 (element-type-to-access-code element-type)))
		    (t
		     (%primitive alloc-g-vector total-size initial-element)))))
    (cond (initial-element-p
	   (unless (simple-vector-p data)
	     (unless (typep initial-element element-type)
	       (error "~S cannot be used to initialize an array of type ~S."
		      initial-element element-type))
	     (fill (the vector data) initial-element)))
	  (initial-contents
	   (copy-contents-aux dimensions initial-contents element-type
			      rank 0 data)))
    data))

;;; COPY-CONTENTS-AUX spins down into the Data vector and the Initial-Contents
;;; filling the former from the latter.
;;;
(defun copy-contents-aux (dimensions initial-contents element-type
			  depth index data)
  (declare (fixnum depth index))
  (cond ((= depth 0)
	 (unless (typep initial-contents element-type)
	   (error "~S cannot be used to initialize an array of element-type ~S."
		  initial-contents element-type))
	 (setf (aref data index) initial-contents)
	 (the fixnum (1+ index)))
	((listp initial-contents)
	 (unless (= (length (the list initial-contents)) (car dimensions))
	   (error "This part of initial-contents, ~S, is an unappropriate ~
		   length for the dimension, ~S."
		  initial-contents (car dimensions)))
	 (do ((initial-contents initial-contents (cdr initial-contents))
	      (next-dimensions (cdr dimensions))
	      (next-depth (the fixnum (1- depth))))
	     ((null initial-contents) index)
	   (declare (list initial-contents))
	   (setq index (copy-contents-aux
			next-dimensions (car initial-contents) element-type
			next-depth index data))))
	((vectorp initial-contents)
	 (unless (= (length (the vector initial-contents)) (car dimensions))
	   (error "This part of initial-contents, ~S, is an unappropriate ~
		   length for the dimension, ~S."
		  initial-contents (car dimensions)))
	 (do ((i-index 0 (1+ i-index))
	      (i-end (length (the vector initial-contents)))
	      (next-dimensions (cdr dimensions))
	      (next-depth (the fixnum (1- depth))))
	     ((= i-index i-end) index)
	   (declare (fixnum i-index i-end))
	   (setq index (copy-contents-aux
			next-dimensions (aref initial-contents index)
			element-type next-depth index data))))
	(t
	 (error "~S is not a sequence, and cannot be used to initialize~%~
		the contents of an array." initial-contents))))

;;; ELEMENT-TYPE-TO-ACCESS-CODE returns the Spice Lisp I-Vector access code to
;;; be used for the data vector of an array with the given access code.
;;;
(defun element-type-to-access-code (type)
  (cond ((subtypep type 'bit) 0)
	((subtypep type '(unsigned-byte 2)) 1)
	((subtypep type '(unsigned-byte 4)) 2)
	((subtypep type '(unsigned-byte 8)) 3)
	((subtypep type '(unsigned-byte 16)) 4)
	((subtypep type '(unsigned-byte 32)) 5)
	(t (error "Unexpected array element type -- ~S." type))))


;;; ARRAY-LINEAR-LENGTH returns the number of elements an array with the
;;; specified dimensions would have.
;;;
(defun array-linear-length (dimensions)
  (do ((dimensions dimensions (cdr dimensions))
       (length 1))
      ((null dimensions) length)
    (declare (fixnum length))
    (setq length (* length (the fixnum (car dimensions))))))

(defun aref (array &rest subscripts)
  "Returns the element of the Array specified by the Subscripts."
  (if (and subscripts (null (cdr subscripts)))
      (aref array (car subscripts))
      (do ((subscripts (nreverse (the list subscripts)) (cdr subscripts))
	   (dim-index (1- (the fixnum (%primitive header-length array)))
		      (1- dim-index))
	   (chunk-size 1)
	   (result 0))
	  ((= (the fixnum dim-index) %array-dim-base)
	   (if (atom subscripts)
	       (with-array-data ((data array) (start) (end))
		 (declare (ignore end))
		 (aref data (the fixnum (+ start result))))
	       (error "Too many subscripts for array reference.")))
	(declare (fixnum dim-index chunk-size result))
	(let ((axis (%primitive header-ref array dim-index)))
	  (declare (fixnum axis))
	  (cond ((atom subscripts)
		 (error "Too few subscripts for array reference."))
		((not (< -1 (the fixnum (car subscripts)) axis))
		 (error "Subscript ~S is out of bounds." (car subscripts)))
		(t
		 (setq result (the fixnum
				   (+ result
				      (the fixnum
					   (* (the fixnum (car subscripts))
					      chunk-size)))))
		 (setq chunk-size (* chunk-size axis))))))))

(defun %aset (array &rest stuff)
  (if (and (cdr stuff) (null (cddr stuff)))
      (setf (aref array (car stuff)) (cadr stuff))
      (let ((rstuff (nreverse (the list stuff))))
	(do ((subscripts (cdr rstuff) (cdr subscripts))
	     (dim-index (1- (the fixnum (%primitive header-length array)))
			(1- dim-index))
	     (chunk-size 1)
	     (result 0))
	    ((= dim-index %array-dim-base)
	     (if (atom subscripts)
		 (with-array-data ((data array) (start) (end))
		   (declare (ignore end))
		   (setf (aref data (+ start result)) (car rstuff)))
		 (error "Too many subscripts for array reference.")))
	  (declare (fixnum dim-index chunk-size result))
	  (let ((axis (%primitive header-ref array dim-index)))
	    (declare (fixnum axis))
	    (cond ((atom subscripts)
		   (error "Too few subscripts for array reference."))
		  ((not (< -1 (the fixnum (car subscripts)) axis))
		   (error "Subscript ~S is out of bounds."
			  (car subscripts)))
		  (t
		   (setq result (+ result
				   (the fixnum (* (the fixnum (car subscripts))
						  chunk-size))))
		   (setq chunk-size (* chunk-size axis)))))))))


;;; %Apply-aset is called when (setf (apply #'aref ...) new-value) is
;;; called.

(defun %apply-aset (new-value array &rest stuff)
  (if (null (cdr stuff))
      (setf (aref array (car stuff)) new-value)
      (let ((rstuff (nreverse (the list stuff))))
	(do ((subscripts rstuff (cdr subscripts))
	     (dim-index (1- (the fixnum (%primitive header-length array)))
			(1- dim-index))
	     (chunk-size 1)
	     (result 0))
	    ((= dim-index %array-dim-base)
	     (if (atom subscripts)
		 (with-array-data ((data array) (start) (end))
				  (declare (ignore end))
				  (setf (aref data (+ start result)) new-value))
		 (error "Too many subscripts for array reference.")))
	  (declare (fixnum dim-index chunk-size result))
	  (let ((axis (%primitive header-ref array dim-index)))
	    (declare (fixnum axis))
	    (cond ((atom subscripts)
		   (error "Too few subscripts for array reference."))
		  ((not (< -1 (the fixnum (car subscripts)) axis))
		   (error "Subscript ~S is out of bounds."
			  (car subscripts)))
		  (t
		   (setq result (+ result
				   (the fixnum (* (the fixnum (car subscripts))
						  chunk-size))))
		   (setq chunk-size (* chunk-size axis)))))))))

(defun array-element-type (array)
  "Returns the type of the elements of the array"
  (cond ((bit-vector-p array)
	 '(mod 2))
	((stringp array)
	 'string-char)
	((simple-vector-p array)
	 t)
	((array-header-p array)
	 (with-array-data ((data array) (start) (end))
	   (declare (ignore start end))
	   (array-element-type data)))
	((vectorp array)
	 (case (%primitive get-vector-access-code array)
	   (0 'bit)
	   (1 '(unsigned-byte 2))
	   (2 '(unsigned-byte 4))
	   (3 '(unsigned-byte 8))
	   (4 '(unsigned-byte 16))
	   (5 '(unsigned-byte 32))))
	(t (error "~S is not an array." array))))

(defun array-rank (array)
  "Returns the number of dimensions of the Array."
  (if (array-header-p array)
      (the fixnum (- (the fixnum (%primitive header-length array))
		     %array-first-dim-slot))
      1))

(defun array-dimension (array axis-number)
  "Returns length of dimension Axis-Number of the Array."
  (declare (fixnum axis-number))
  (if (array-header-p array)
      (if (and (>= axis-number 0)
	       (< axis-number (the fixnum (array-rank array))))
	  (%primitive header-ref array (the fixnum (+ %array-first-dim-slot axis-number)))
	  (error "~S is an illegal axis number." axis-number))
      (if (= axis-number 0)
	  (%primitive vector-length array)
	  (error "~S is an illegal axis number." axis-number))))

(defun array-dimensions (array)
  "Returns a list whose elements are the dimensions of the array"
  (if (array-header-p array)
      (do ((index %array-first-dim-slot (1+ index))
	   (end (%primitive header-length array))
	   (result ()))
	  ((= index end) (nreverse result))
	(declare (fixnum index end))
	(push (%primitive header-ref array index) result))
      (list (%primitive vector-length array))))

(defun array-total-size (array)
  "Returns the total number of elements in the Array."
  (if (array-header-p array)
      (%primitive header-ref array %array-length-slot)
      (%primitive vector-length array)))

(defun array-in-bounds-p (array &rest subscripts)
  "Returns T if the Subscipts are in bounds for the Array, Nil otherwise."
  (if (array-header-p array)
      (do ((dim-index %array-first-dim-slot (1+ dim-index))
	   (dim-index-limit (+ %array-first-dim-slot
			       (the fixnum (array-rank array))))
	   (subs subscripts (cdr subs)))
	  ((= dim-index dim-index-limit)
	   (atom subs))
	(declare (fixnum dim-index dim-index-limit))
	(if (atom subs)
	    (return nil)
	    (if (not (< -1
			(the fixnum (car subs))
			(the fixnum (%primitive header-ref array dim-index))))
		(return nil))))
      (and (null (cdr subscripts))
	   (< -1
	      (the fixnum (car subscripts))
	      (the fixnum (%primitive vector-length array))))))

(defun array-row-major-index (array &rest subscripts)
  "Returns the index into the Array's data vector for the given subscripts."
  (if (array-header-p array)
      (do ((subscripts (nreverse (the list subscripts)) (cdr subscripts))
	   (dim-index (1- (the fixnum (%primitive header-length array)))
		      (1- dim-index))
	   (chunk-size 1)
	   (result 0))
	  ((= dim-index %array-dim-base)
	   (if (atom subscripts)
	       result
	       (error "Too many subscripts for array reference.")))
	(declare (fixnum dim-index chunk-size result))
	(let ((axis (%primitive header-ref array dim-index)))
	  (declare (fixnum axis))
	  (cond ((null subscripts)
		 (error "Too few subscripts for array reference."))
		((not (< -1 (the fixnum (car subscripts)) axis))
		 (error "Subscript ~S is out of bounds." (car subscripts)))
		(t
		 (setq result (+ result
				 (the fixnum (* (the fixnum (car subscripts))
						chunk-size))))
		 (setq chunk-size (* chunk-size axis))))))
      (cond ((null subscripts)
	     (error "Too few subscripts for array reference."))
	    ((not (< -1
		     (the fixnum (car subscripts))
		     (length (the simple-array array))))
	     (error "Subscript ~S is out of bounds." (car subscripts)))
	    (t
	     (car subscripts)))))

(defun adjustable-array-p (array)
  "Returns T if the given Array is adjustable, or Nil otherwise."
  (array-header-p array))


(defun row-major-aref (array index)
  "Returns the element of array corressponding to the row-major index.  This is
   SETF'able."
  (with-array-data ((data array) (start) (end))
    (declare (ignore end))
    (aref data (+ start index))))

(defsetf row-major-aref %set-row-major-aref)

(defun %set-row-major-aref (array index new-value)
  (with-array-data ((data array) (start) (end))
    (declare (ignore end))
    (setf (aref data (+ start index)) new-value)))

(defun svref (simple-vector index)
  "Returns the Index'th element of the given Simple-Vector."
  (svref simple-vector index))

(defun %svset (simple-vector index new)
  (setf (svref simple-vector index) new))

;;; The following function is used when (setf (apply #'svref ...) new
;;; is compiled.

(defun %apply-svset (new simple-vector index)
  (setf (svref simple-vector index) new))

(defun array-has-fill-pointer-p (array)
  "Returns T if the given Array has a fill pointer, or Nil otherwise."
  (and (vectorp array) (array-header-p array)))

(defun fill-pointer (vector)
  "Returns the Fill-Pointer of the given Vector."
  (if (and (vectorp vector) (array-header-p vector))
      (%primitive header-ref vector %array-fill-pointer-slot)
      (error "~S is not an array with a fill-pointer." vector)))

(defun %set-fill-pointer (vector new)
  (declare (fixnum new))
  (if (and (vectorp vector) (array-header-p vector))
      (if (> new (the fixnum (%primitive header-ref vector %array-length-slot)))
	  (error "New fill pointer, ~S, is larger than the length of the vector."
		 new)
	  (%primitive header-set vector %array-fill-pointer-slot new))
      (error "~S is not an array with a fill-pointer." vector)))

(defun vector-push (new-el array)
  "Attempts to set the element of Array designated by the fill pointer
   to New-El and increment fill pointer by one.  If the fill pointer is
   too large, Nil is returned, otherwise the new fill pointer value is 
   returned."
  (if (array-header-p array)
      (let ((fill-pointer (%primitive header-ref array %array-fill-pointer-slot)))
	(declare (fixnum fill-pointer))
	(cond ((= fill-pointer
		  (the fixnum (%primitive header-ref array %array-length-slot)))
	       nil)
	      (t (%primitive header-set array %array-fill-pointer-slot
			     (1+ fill-pointer))
		 (with-array-data ((data array) (start) (end))
		   (declare (ignore end))
		   (setf (aref data (+ fill-pointer start)) new-el))
		 fill-pointer)))
      (error "~S: Object has no fill pointer." array)))

(defun vector-push-extend (new-el array &optional (extension (length array)))
  "Like Vector-Push except that if the fill pointer gets too large, the
   Array is extended rather than Nil being returned."
  (declare (fixnum extension))
  (if (array-header-p array)
      (let ((length (%primitive header-ref array %array-length-slot))
	    (fill-pointer (%primitive header-ref array %array-fill-pointer-slot)))
	(declare (fixnum length fill-pointer))
	(with-array-data ((data array) (start) (end))
	  (declare (ignore end))
	  (if (= fill-pointer length)
	      (do* ((new-index 0 (1+ new-index))
		    (new-length (let ((l (+ length extension)))
				  (declare (fixnum l))
				  (if (zerop l) 1 l)))
		    (old-index start (1+ old-index))
		    (new-data (make-array (if (zerop new-length) 1 new-length)
					  :element-type (array-element-type
							 array))))
		   ((= new-index length)
		    (setq data new-data)
		    (setq start 0)
		    (set-array-header array data new-length
				      (1+ fill-pointer) start new-length nil))
		(declare (fixnum new-index new-length old-index))
		(setf (aref new-data new-index) (aref data old-index)))
	      (%primitive header-set array
			  %array-fill-pointer-slot (1+ fill-pointer)))
	  (setf (aref data (+ fill-pointer start)) new-el)
	  fill-pointer))
      (error "~S has no fill pointer." array)))

(defun vector-pop (array)
  "Attempts to decrease the fill-pointer by 1 and return the element
   pointer to by the new fill pointer.  If the new value of the fill
   pointer is 0, an error occurs."
  (if (array-header-p array)
      (let ((fill-pointer (%primitive header-ref array %array-fill-pointer-slot)))
	(declare (fixnum fill-pointer))
	(cond ((< fill-pointer 1)
	       (error "Fill-pointer reached 0."))
	      (t
	       (let ((fill-pointer (1- fill-pointer)))
		 (declare (fixnum fill-pointer))
		 (with-array-data ((data array) (start) (end))
		   (declare (ignore end))
		   (%primitive header-set array %array-fill-pointer-slot
			       fill-pointer)
		   (aref data (+ fill-pointer start)))))))
      (error "~S: Object has no fill pointer." array)))


(defun adjust-array (array dimensions &key
			   (element-type (array-element-type array))
			   (initial-element nil initial-element-p)
			   initial-contents fill-pointer
			   displaced-to displaced-index-offset)
  "Adjusts the Array's dimensions to the given Dimensions and stuff."
  (unless (listp dimensions) (setq dimensions (list dimensions)))
  (cond ((not (array-header-p array))
	 (error "~S is not an adjustable array." array))
	((/= (the fixnum (length (the list dimensions)))
	     (the fixnum (array-rank array)))
	 (error "Number of dimensions not equal to rank of array."))
	((not (subtypep element-type (array-element-type array)))
	 (error "New element type, ~S, is incompatible with old."
		element-type)))
  (let ((array-rank (length (the list dimensions))))
    (declare (fixnum array-rank))
    (when (and fill-pointer (> array-rank 1))
      (error "Multidimensional arrays can't have fill pointers."))
    (cond (initial-contents
	   (if (or initial-element-p displaced-to)
	       (error "Initial contents may not be specified with ~
		       the :initial-element or :displaced-to option."))
	   (let* ((array-size (array-linear-length dimensions))
		  (array-data (data-vector-from-inits
			       dimensions array-size array-rank element-type
			       initial-contents initial-element
			       initial-element-p)))
	     (set-array-header array array-data array-size
			       (get-new-fill-pointer array array-size
						     fill-pointer)
			       0 dimensions nil)))
	  (displaced-to
	   (when initial-element ;no initial-contents supplied is already known
	       (error "The :initial-element option may not be specified ~
		       with :displaced-to."))
	   (unless (subtypep element-type (array-element-type displaced-to))
	     (error "One can't displace an array of type ~S into another of ~
		     type ~S." element-type (array-element-type displaced-to)))
	   (let ((displacement (or displaced-index-offset 0))
		 (array-size (array-linear-length dimensions)))
	     (declare (fixnum displacement array-size))
	     (if (< (the fixnum (array-total-size displaced-to))
		    (the fixnum (+ displacement array-size)))
		 (error "The :displaced-to array is too small."))
	     (set-array-header array displaced-to array-size
			       (get-new-fill-pointer array array-size
						     fill-pointer)
			       displacement dimensions t)))
	  ((= array-rank 1)
	   (let ((old-length (%primitive header-ref array %array-length-slot))
		 (new-length (car dimensions))
		 new-data)
	     (declare (fixnum old-length new-length))
	     (with-array-data ((old-data array) (old-start)
			       (old-end old-length))
	       (cond ((or (%displacedp array) (< old-length new-length))
		      (setf new-data
			    (data-vector-from-inits
			     dimensions new-length array-rank element-type
			     initial-contents initial-element
			     initial-element-p))
		      (replace new-data old-data
			       :start2 old-start :end2 old-end))
		      (t (setf new-data
			       (%primitive shrink-vector old-data new-length))))
	       (set-array-header array new-data new-length
				 (get-new-fill-pointer array new-length
						       fill-pointer)
				 0 dimensions nil))))
	  (t
	   (let ((old-length (%primitive header-ref array %array-length-slot))
		 (new-length (array-linear-length dimensions)))
	     (declare (fixnum old-length new-length))
	     (with-array-data ((old-data array) (old-start)
			       (old-end old-length))
	       (declare (ignore old-end))
	       (let ((new-data (if (or (%displacedp array)
				       (> new-length old-length))
				   (data-vector-from-inits
				    dimensions new-length array-rank
				    element-type () initial-element
				    initial-element-p)
				   old-data)))
		 (zap-array-data old-data (array-dimensions array) old-start
				 new-data dimensions new-length element-type
				 initial-element initial-element-p)
		 (set-array-header array new-data new-length
				   new-length 0 dimensions nil)))))))
  array)

(defun get-new-fill-pointer (old-array new-array-size fill-pointer)
  (cond ((not fill-pointer)
	 (%primitive header-ref old-array %array-fill-pointer-slot))
	((numberp fill-pointer)
	 fill-pointer)
	(t new-array-size)))

(defun shrink-vector (vector new-size)
  "Destructively alters the Vector, changing its length to New-Size, which
   must be less than or equal to its current size."
  (cond ((array-header-p vector)
	 ;; (%primitive shrink-vector
	 ;;		(%primitive header-ref vector %array-data-slot)
	 ;;		new-size)
	 ;; (%primitive header-set vector %array-length-slot new-size)
	 ;; Instead of shrinking the vector, just set the fill-pointer field.
	 (%primitive header-set vector %array-fill-pointer-slot new-size)
	 vector)
	(t
	 (%primitive shrink-vector vector new-size))))

(defun set-array-header (array data length fill-pointer displacement dimensions
			 &optional displacedp)
  "Fills in array header with provided information.  Returns array."
  (%primitive header-set array %array-data-slot data)
  (%primitive header-set array %array-length-slot length)
  (%primitive header-set array %array-fill-pointer-slot fill-pointer)
  (%primitive header-set array %array-displacement-slot displacement)
  (if (listp dimensions)
      (do ((index %array-first-dim-slot (1+ index))
	   (dims dimensions (cdr dims)))
	  ((null dims))
	(declare (fixnum index))
	(%primitive header-set array index (car dims)))
      (%primitive header-set array %array-first-dim-slot dimensions))
  (%set-array-displacedp array displacedp)
  array)



;;;; ZAP-ARRAY-DATA for ADJUST-ARRAY.

;;; Make a temporary to be used when old-data and new-data are EQ.
;;;
(defvar *zap-array-data-temp* (%primitive alloc-g-vector 1000 t))

(defun zap-array-data-temp (length element-type initial-element
			    initial-element-p)
  (declare (fixnum length))
  (when (> length (the fixnum (length *zap-array-data-temp*)))
    (setf *zap-array-data-temp* (%primitive alloc-g-vector length t)))
  (when initial-element-p
    (unless (typep initial-element element-type)
      (error "~S cannot be used to initialize an array of type ~S."
	     initial-element element-type))
    (fill (the simple-vector *zap-array-data-temp*) initial-element :end length))
  *zap-array-data-temp*)


;;; ZAP-ARRAY-DATA  --  Internal.
;;;
;;; This does the grinding work for ADJUST-ARRAY.  It zaps the data from the
;;; Old-Data in an arrangement specified by the Old-Dims to the New-Data in an
;;; arrangement specified by the New-Dims.  Offset is a displaced offset to be
;;; added to computed indexes of Old-Data.  New-Length, Element-Type,
;;; Initial-Element, and Initial-Element-P are used when Old-Data and New-Data
;;; are EQ; in this case, a temporary must be used and filled appropriately.
;;; When Old-Data and New-Data are not EQ, New-Data has already been filled
;;; with any specified initial-element.
;;;
(defun zap-array-data (old-data old-dims offset new-data new-dims new-length
		       element-type initial-element initial-element-p)
  (declare (list old-dims new-dims))
  (setq old-dims (nreverse old-dims))
  (setq new-dims (reverse new-dims))
  (if (eq old-data new-data)
      (let ((temp (zap-array-data-temp new-length element-type
				       initial-element initial-element-p)))
	(zap-array-data-aux old-data old-dims offset temp new-dims)
	(dotimes (i new-length) (setf (aref new-data i) (aref temp i))))
      (zap-array-data-aux old-data old-dims offset new-data new-dims)))
      

(defun zap-array-data-aux (old-data old-dims offset new-data new-dims)
  (declare (fixnum offset))
  (let ((limits (mapcar #'(lambda (x y)
			    (declare (fixnum x y))
			    (1- (the fixnum (min x y))))
			old-dims new-dims)))
    (macrolet ((bump-index-list (index limits)
		 `(do ((subscripts ,index (cdr subscripts))
		       (limits ,limits (cdr limits)))
		      ((null subscripts) nil)
		    (cond ((< (the fixnum (car subscripts))
			      (the fixnum (car limits)))
			   (rplaca subscripts (1+ (the fixnum (car subscripts))))
			   (return ,index))
			  (t (rplaca subscripts 0))))))
      (do ((index (make-list (length old-dims) :initial-element 0)
		  (bump-index-list index limits)))
	  ((null index))
	(setf (aref new-data (row-major-index-from-dims index new-dims))
	      (aref old-data
		    (+ (the fixnum (row-major-index-from-dims index old-dims))
		       offset)))))))

;;; ROW-MAJOR-INDEX-FROM-DIMS  --  Internal.
;;;
;;; This figures out the row-major-order index of an array reference from a
;;; list of subscripts and a list of dimensions.  This is for internal calls
;;; only, and the subscripts and dim-list variables are assumed to be reversed
;;; from what the user supplied.
;;;
(defun row-major-index-from-dims (rev-subscripts rev-dim-list)
  (do ((rev-subscripts rev-subscripts (cdr rev-subscripts))
       (rev-dim-list rev-dim-list (cdr rev-dim-list))
       (chunk-size 1)
       (result 0))
      ((null rev-dim-list) result)
    (declare (fixnum chunk-size result))
    (setq result (+ result
		    (the fixnum (* (the fixnum (car rev-subscripts))
				   chunk-size))))
    (setq chunk-size (* chunk-size (the fixnum (car rev-dim-list))))))



;;;; Some bit stuff.
 
(defun bit (bit-array &rest subscripts)
  "Returns the bit from the Bit-Array at the specified Subscripts."
  (apply #'aref bit-array subscripts))

(defun %bitset (bit-array &rest stuff)
  (apply #'%aset bit-array stuff))

(defun sbit (simple-bit-array &rest subscripts)
  "Returns the bit from the Simple-Bit-Array at the specified Subscripts."
  (apply #'aref simple-bit-array subscripts))

(defun %sbitset (bit-array &rest stuff)
  (apply #'%aset bit-array stuff))

(defun bit-array-same-dimensions-p (array1 array2)
  (and (= (the fixnum (%primitive header-length array1))
	  (the fixnum (%primitive header-length array2)))
       (do ((index %array-first-dim-slot (1+ index))
	    (length (%primitive header-length array1)))
	   ((= index length) t)
	 (declare (fixnum index length))
	 (if (/= (the fixnum (%primitive header-ref array1 index))
		 (the fixnum (%primitive header-ref array2 index)))
	     (return nil)))))

(defun bit-array-boole (array1 array2 op result-array)
  (if (eq result-array t) (setq result-array array1))
  (cond ((simple-bit-vector-p array1)
	 (let ((length (%primitive vector-length array1)))
	   (declare (fixnum length))
	   (unless (and (simple-bit-vector-p array2)
			(= (the fixnum (%primitive vector-length array2)) length))
	     (error "~S and ~S do not have the same dimensions." array1 array2))
	   (if result-array
	       (unless (and (simple-bit-vector-p result-array)
			    (= (the fixnum (%primitive vector-length result-array))
			       length))
		 (error "~S and ~S do not have the same dimensions."
			array1 result-array))
	       (setq result-array (%primitive alloc-bit-vector length)))
	   (%primitive bit-bash array1 array2 result-array op)))
	(t
	 (unless (bit-array-same-dimensions-p array1 array2)
	   (error "~S and ~S do not have the same dimensions." array1 array2))
	 (if result-array
	     (unless (bit-array-same-dimensions-p array1 result-array)
	       (error "~S and ~S do not have the same dimensions."
		      array1 result-array))
	     (setq result-array (make-array (array-dimensions array1)
					    :element-type '(mod 2))))
	 (with-array-data ((data1 array1) (start1) (end1))
	   (declare (ignore end1))
	   (with-array-data ((data2 array2) (start2) (end2))
	     (declare (ignore end2))
	     (with-array-data ((data3 result-array) (start3) (end3))
	       (declare (ignore end3))
	       (let ((length (%primitive header-ref array1 %array-length-slot)))
		 (declare (fixnum length))
		 (do ((index 0 (1+ index))
		      (index1 start1 (1+ index1))
		      (index2 start2 (1+ index2))
		      (index3 start3 (1+ index3)))
		     ((= index length) result-array)
		   (declare (fixnum index index1 index2 index3))
		   (setf (sbit data3 index3)
			 (boole op (sbit data1 index1)
				(sbit data2 index2))))))))))
  result-array)

(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical AND on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-and result-bit-array))

(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical IOR on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-ior result-bit-array))

(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical XOR on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-xor result-bit-array))

(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical EQV  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-eqv result-bit-array))

(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical NAND  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-nand result-bit-array))

(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical NOR  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-nor result-bit-array))

(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ANDC1 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-andc1 result-bit-array))

(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ANDC2 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-andc2 result-bit-array))

(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ORC1 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-orc1 result-bit-array))

(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ORC2 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-orc2 result-bit-array))

(defun bit-not (bit-array &optional result-bit-array)
  "Performs a bit-wise logical NOT in the elements of the Bit-Array putting
  the results into the Result-Bit-Array."
  (bit-array-boole bit-array bit-array boole-nor result-bit-array))
