;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;
;;; Permutation vectors.
;;;

(in-package 'pcl)

(defmacro instance-slot-index (wrapper slot-name)
  `(let ((pos 0))
     (declare (fixnum pos))
     (block loop
       (dolist (sn (wrapper-instance-slots-layout ,wrapper))
	 (when (eq ,slot-name sn) (return-from loop pos))
	 (incf pos)))))


;;;
;;;
;;;
(defun pv-cache-limit-fn (nlines)
  (default-limit-fn nlines))

(defstruct (isl
	     (:predicate islp)
	     (:constructor make-isl-internal (cache slot-name-lists)))
  (cache *empty-cache* :type cache)
  (slot-name-lists nil :type list))

(defvar *initial-isl* (make-isl-internal nil nil))

; help new slot-value-using-class methods affect fast iv access
(defvar *all-isl-list* nil) 

(defun make-isl (slot-name-lists)
  (let ((isl (make-isl-internal (get-cache (- (length slot-name-lists)
					      (count nil slot-name-lists))
					   t
					   #'pv-cache-limit-fn
					   2)
				slot-name-lists)))
    (push isl *all-isl-list*)
    isl))

(defun make-isl-type-declaration (var)
  `(type isl ,var))

(defvar *slot-name-lists-inner* (make-hash-table :test #'equal))
(defvar *slot-name-lists-outer* (make-hash-table :test #'equal))

(defun intern-slot-name-lists (slot-name-lists)
  (flet ((inner (x) 
	   (or (gethash x *slot-name-lists-inner*)
	       (setf (gethash x *slot-name-lists-inner*) (copy-list x))))
	 (outer (x) 
	   (or (gethash x *slot-name-lists-outer*)
	       (setf (gethash x *slot-name-lists-outer*) (make-isl (copy-list x))))))
    (outer (mapcar #'inner slot-name-lists))))



(defvar *pvs* (make-hash-table :test #'equal))

(defun optimize-slot-value-by-class-p (class slot-name type)
  (let ((slotd (find-slot-definition class slot-name)))
    (and slotd 
	 (or (not (eq *boot-state* 'complete))
	     (slot-accessor-std-p slotd type)))))

(defun lookup-pv (isl wrappers)
  (unless (listp wrappers) (setq wrappers (list wrappers)))
  (let* ((class-slot-p nil)
	 (elements
	  (gathering1 (collecting)
	    (iterate ((slot-names (list-elements (isl-slot-name-lists isl))))
	      (when slot-names
		(let* ((wrapper     (pop wrappers))
		       (class       (wrapper-class wrapper))
		       (class-slots (wrapper-class-slots wrapper)))
		  (dolist (slot-name slot-names)
		    (if (optimize-slot-value-by-class-p class slot-name 'all)
			(let ((index (instance-slot-index wrapper slot-name)))
			  (if index
			      (gather1 index)
			      (let ((cell (assq slot-name class-slots)))
				(if cell
				    (progn (setq class-slot-p t) (gather1 cell))
				    (gather1 nil)))))
			(gather1 nil)))))))))
    (if class-slot-p				;Sure is a shame Common Lisp doesn't
	(make-permutation-vector elements)	;give me the right kind of hash table.
	(or (gethash elements *pvs*)
	    (setf (gethash elements *pvs*) (make-permutation-vector elements))))))

(defun make-permutation-vector (indexes)
  (make-array (length indexes) :initial-contents indexes))

(defun make-pv-type-declaration (var)
  `(type simple-vector ,var))

(defmacro pvref (pv index)
  `(svref ,pv ,index))

;The idea behind pv cells is that we can update them when 
;new slot-value-using-class methods are added, and affect cached methods.
;To do this, we have to be able to find all existing isl-caches.
(defmacro make-pv-cell (pv)
  `(cons ,pv nil))

(defmacro pv-from-pv-cell (pv-cell)
  `(car ,pv-cell))

(defun update-all-isl-caches (class slot-name-list)
  (declare (ignore slot-name-list))
  (dolist (isl *all-isl-list*)
    (map-cache #'(lambda (wrappers pv-cell)
		   (when (if (atom wrappers)
			     (eq class (wrapper-class wrappers))
			     (find class wrappers :key #'wrapper-class))
		     (setf (pv-from-pv-cell pv-cell)
			   (lookup-pv isl wrappers))))
	       (isl-cache isl))))


(defun optimize-generic-function-call (form required-parameters env)
  (declare (ignore env))
  form
  #||
  (let* ((gf-name (car form))
	 (gf (gdefinition gf-name))
	 (arg-info (gf-arg-info gf))
	 (metatypes (arg-info-metatypes arg-info))
	 (nreq (length metatypes))
	 (applyp (arg-info-applyp arg-info)))
    (declare (ignore applyp))
    (if (or (zerop nreq)
	    (not (<= nreq (length (cdr form))))
	    (not (every #'(lambda (arg mt)
			    (declare (ignore mt))
			    (when (and (consp arg) 
				       (eq (car arg) 'the))
			      (setq arg (caddr arg)))
			    (and (symbolp arg)
				 (memq arg required-parameters))
			    (let ((class-name (caddr (variable-declaration 
						      'class arg env))))
			      (and class-name (not (eq 't class-name)))))
			(cdr form) metatypes)))
	form
	form))||#) ;`(maybe-fast-gf-call ,(car form) ,(cdr form))


;; For calls to a gf:
; gf-call-info: (gf call-info-vector . gf-function-vector)
; call-info-vector:     #(call-info1 ... call-infon)
; gf-function-vector:   #(function1 ... functionn)
; --> once an entry is made in call-info-vector, it is never moved or removed
; call-info:            (gf . arg-types)
; arg-type:             a type. `(arg ,n) is not allowed here.

;; For calls from a method:
; method-gf-call-info:  (method-specializers method-call-info-vector . ???)
; arg-type:             a type or `(arg ,n)
; when arg-type is (arg n) the real type is either:
;   the arg's specializer or
;   (wrapper-eq ,wrapper) for a call appearing within a caching dfun gf

; every optimized gf in a method has an entry in the method's method-call-info-vector
; a macro: (get-call-cell mciv-index .all-wrappers.) ->
;          index into the gf-function-vector

;(defmacro maybe-fast-gf-call (gf-name args)
;   nil)


(defun can-optimize-access (form required-parameters env)
  (let ((type (ecase (car form)
		(slot-value 'reader)
		(set-slot-value 'writer)
		(slot-boundp 'boundp)))
	(var (cadr form))
	(slot-name (eval (caddr form)))) ; known to be constant
    (when (and (consp var) (eq 'the (car var)))
      (setq var (caddr var)))
    (when (symbolp var)
      (let* ((rebound? (caddr (variable-declaration 'variable-rebinding var env)))
	     (parameter-or-nil (car (memq (or rebound? var) required-parameters))))
	(when parameter-or-nil
	  (let* ((class-name (caddr (variable-declaration 
				     'class parameter-or-nil env)))
		 (class (find-class class-name nil)))
	    (when (if (and class
			   (memq *the-class-structure-object*
				 (class-precedence-list class)))
		      (optimize-slot-value-by-class-p class slot-name type)
		      (and class-name (not (eq class-name 't))))
	      (cons parameter-or-nil (or class class-name)))))))))

(defun optimize-slot-value (slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form) form
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots :read sparameter slot-name nil)))
      `(accessor-slot-value ,@(cdr form))))

(defun optimize-set-slot-value (slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form new-value) form
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots :write sparameter slot-name new-value)))
      `(accessor-set-slot-value ,@(cdr form))))

(defun optimize-slot-boundp (slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form new-value) form
	(let ((slot-name (eval slot-name-form)))
	  (optimize-instance-access slots :boundp sparameter slot-name new-value)))
      `(accessor-slot-boundp ,@(cdr form))))

;;;
;;; The <slots> argument is an alist, the CAR of each entry is the name of
;;; a required parameter to the function.  The alist is in order, so the
;;; position of an entry in the alist corresponds to the argument's position
;;; in the lambda list.
;;; 
(defun optimize-instance-access (slots read/write sparameter slot-name new-value)
  (let ((class (if (consp sparameter) (cdr sparameter) *the-class-t*))
	(parameter (if (consp sparameter) (car sparameter) sparameter)))
    (if (and (eq *boot-state* 'complete)
	     (classp class)
	     (memq *the-class-structure-object* (class-precedence-list class)))
	(let ((slotd (find-slot-definition class slot-name)))
	  (ecase read/write
	    (:read
	     `(,(slot-definition-defstruct-accessor-symbol slotd) ,parameter))
	    (:write
	     `(setf (,(slot-definition-defstruct-accessor-symbol slotd) ,parameter)
	       ,new-value))
	    (:boundp
	     'T)))
	(let* ((parameter-entry (assq parameter slots))
	       (slot-entry      (assq slot-name (cdr parameter-entry)))
	       (position (posq parameter-entry slots))
	       (pv-offset-form (list 'pv-offset ''.PV-OFFSET.)))
	  (unless parameter-entry
	    (error "Internal error in slot optimization."))
	  (unless slot-entry
	    (setq slot-entry (list slot-name))
	    (push slot-entry (cdr parameter-entry)))
	  (push pv-offset-form (cdr slot-entry))
	  (ecase read/write
	    (:read
	     `(instance-read ,pv-offset-form ,parameter ,position 
		             ',slot-name ',class))
	    (:write
	     `(let ((.new-value. ,new-value)) 
	        (instance-write ,pv-offset-form ,parameter ,position 
		                ',slot-name ',class .new-value.)))
	    (:boundp
	     `(instance-boundp ,pv-offset-form ,parameter ,position 
		               ',slot-name ',class)))))))

(define-walker-template pv-offset) ; These forms get munged by mutate slots.
(defmacro pv-offset (arg) arg)

;; It is safe for these two functions to be wrong.
;; They just try to guess what the most likely case will be.
(defun generate-fast-class-slot-access-p (class-form slot-name-form)
  (let ((class (and (constantp class-form) (eval class-form)))
	(slot-name (and (constantp slot-name-form) (eval slot-name-form))))
    (and (eq *boot-state* 'complete)
	 (standard-class-p class)
	 (not (eq class *the-class-t*)) ; shouldn't happen, though.
	 (let ((slotd (find-slot-definition class slot-name)))
	   (and slotd (classp (slot-definition-allocation slotd)))))))

(defun skip-fast-slot-access-p (class-form slot-name-form type)
  (let ((class (and (constantp class-form) (eval class-form)))
	(slot-name (and (constantp slot-name-form) (eval slot-name-form))))
    (and (eq *boot-state* 'complete)
	 (standard-class-p class)
	 (not (eq class *the-class-t*)) ; shouldn't happen, though.
	 (let ((slotd (find-slot-definition class slot-name)))
	   (and slotd (skip-optimize-slot-value-by-class-p class slot-name type))))))

(defun skip-optimize-slot-value-by-class-p (class slot-name type)
  (let ((slotd (find-slot-definition class slot-name)))
    (and slotd
	 (eq *boot-state* 'complete)
	 (not (slot-accessor-std-p slotd type)))))

(defmacro instance-read (pv-offset parameter position slot-name class)
  (if (skip-fast-slot-access-p class slot-name 'reader)
      `(accessor-slot-value ,parameter ,slot-name)
      (let* ((index (gensym))
	     (value index)
	     (instance parameter)
	     (slots (slot-vector-symbol position)))
	`(locally (declare #.*optimize-speed*)
	   (let ((,index (pvref .PV. ,pv-offset)))
	     (setq ,value (typecase ,index
			    ,(if (generate-fast-class-slot-access-p class slot-name)
				 `(cons (cdr ,index))
				 `(fixnum (%svref ,slots ,index)))
			    (t ',*slot-unbound*)))
	     (if (eq ,value ',*slot-unbound*)
		 (accessor-slot-value ,instance ,slot-name)
		 ,value))))))

(defmacro instance-write (pv-offset parameter position slot-name class new-value)
  (if (skip-fast-slot-access-p class slot-name 'writer)
      `(accessor-set-slot-value ,parameter ,slot-name ,new-value)
      (let* ((index (gensym))
	     (instance parameter)
	     (slots (slot-vector-symbol position)))
	`(locally (declare #.*optimize-speed*)
	   (let ((,index (pvref .PV. ,pv-offset)))
	     (typecase ,index
	       ,(if (generate-fast-class-slot-access-p class slot-name)
		    `(cons (setf (cdr ,index) ,new-value))
		    `(fixnum (setf (%svref ,slots ,index) ,new-value)))
	       (t (accessor-set-slot-value ,instance ,slot-name ,new-value))))))))

(defmacro instance-boundp (pv-offset parameter position slot-name class)
  (if (skip-fast-slot-access-p class slot-name 'boundp)
      `(accessor-slot-boundp ,parameter ,slot-name)
      (let* ((index (gensym))
	     (instance parameter)
	     (slots (slot-vector-symbol position)))
	`(locally (declare #.*optimize-speed*)
	   (let ((,index (pvref .PV. ,pv-offset)))
	     (typecase ,index
	       ,(if (generate-fast-class-slot-access-p class slot-name)
		    `(cons (not (eq (cdr ,index) ',*slot-unbound*)))
		    `(fixnum (not (eq (%svref ,slots ,index) ',*slot-unbound*))))
	       (t (accessor-slot-boundp ,instance ,slot-name))))))))

;;;
;;; This magic function has quite a job to do indeed.
;;;
;;; The careful reader will recall that <slots> contains all of the optimized
;;; slot access forms produced by OPTIMIZE-INSTANCE-ACCESS.  Each of these is
;;; a call to either INSTANCE-READ or INSTANCE-WRITE.
;;;
;;; At the time these calls were produced, the first argument was specified as
;;; the symbol .PV-OFFSET.; what we have to do now is convert those pv-offset
;;; arguments into the actual number that is the correct offset into the pv.
;;;
;;; But first, oh but first, we sort <slots> a bit so that for each argument
;;; we have the slots in alphabetical order.  This canonicalizes the ISL's a
;;; bit and will hopefully lead to having fewer PV's floating around.  Even
;;; if the gain is only modest, it costs nothing.
;;;  
(defun slot-name-lists-from-slots (slots)
  (mapcar #'(lambda (parameter-entry) (mapcar #'car (cdr parameter-entry)))
	  (mutate-slots slots)))

(defun mutate-slots (slots)
  (let ((sorted (sort-slots slots))
	(pv-offset -1))
    (dolist (parameter-entry sorted)
      (dolist (slot-entry (cdr parameter-entry))
	(incf pv-offset)	
	(dolist (form (cdr slot-entry))
	  (setf (cadr form) pv-offset))))
    sorted))

(defun symbol-pkg-name (sym) 
  (let ((pkg (symbol-package sym)))
    (if pkg (package-name pkg) "")))

(defun sort-slots (slots)
  (mapcar #'(lambda (parameter-entry)
	      (cons (car parameter-entry)
		    (sort (cdr parameter-entry)	;slot entries
			  #'(lambda (a b)
			      (if (eq (symbol-package (car a))
				      (symbol-package (car b)))
				  (string-lessp (symbol-name (car a))
						(symbol-name (car b)))
				  (string-lessp (symbol-pkg-name (car a))
						(symbol-pkg-name (car b))))))))
	  slots))


;;;
;;; This needs to work in terms of metatypes and also needs to work for
;;; automatically generated reader and writer functions.
;;; -- Automatically generated reader and writer functions do not use this stuff.

(defun add-pv-binding (method-body plist required-parameters)
  (let* ((isl (getf plist :isl))
	 (isl-cache-symbol (make-symbol "isl-cache")))
    (nconc plist (list :isl-cache-symbol isl-cache-symbol))
    (with-gathering ((slot-variables (collecting))
		     (pv-parameters (collecting)))
      (iterate ((slots (list-elements isl))
		(required-parameter (list-elements required-parameters))
		(i (interval :from 0)))
	       (when slots
		 (gather required-parameter pv-parameters)
		 (gather (slot-vector-symbol i) slot-variables)))
      `((pv-binding ,slot-variables ,pv-parameters ,isl-cache-symbol
	 ,@method-body)))))

(defun make-pv-fn-form (fn-form plist)
  (let ((isl-cache-symbol (getf plist :isl-cache-symbol)))
    `(make-not-for-caching-method-function
       #'(lambda (.all-wrappers.)
	   (declare (special ,isl-cache-symbol))
	   (let ((.ISL. *initial-isl*)
		 (.pv-cell. nil))
	     (declare ,(make-isl-type-declaration '.ISL.))
	     (when .all-wrappers.
	       (setq .isl. ,isl-cache-symbol)
	       (let ((pv-wrappers (pv-wrappers-from-all-wrappers .isl. .all-wrappers.)))
		 (setq .pv-cell. (isl-lookup .isl. pv-wrappers))))
	     ,fn-form)))))

(defmacro pv-binding (slot-vars pv-parameters isl-cache-symbol &body body)
  `(multiple-value-bind (.pv. ,@slot-vars)
       (if .all-wrappers.
	   (values (pv-from-pv-cell .pv-cell.)
		   ,@(mapcar #'(lambda (p) `(get-slots-or-nil ,p)) pv-parameters))
	   (multiple-value-bind (pv-wrappers slots)
	       (pv-wrappers-from-pv-args ,@pv-parameters)
	     (setq .isl. ,isl-cache-symbol)
	     (values-list (list* (pv-from-pv-cell (isl-lookup .isl. pv-wrappers))
				 slots))))
     (declare ,(make-pv-type-declaration '.PV.))
     ,@body))

;;; closure-generators are used only by method-function-for-caching
;;; (in methods.lisp) and make-not-for-caching-method-function.
(defun make-not-for-caching-method-function (closure-generator)
  (let ((function (function-funcall closure-generator nil)))
    #+(and kcl turbo-closure) (si:turbo-closure function)
    (setf (method-function-closure-generator function) closure-generator)
    function))

(defun pv-wrappers-from-pv-args (&rest args)
  (let* ((nkeys (length args))
	 (pv-wrappers (make-list nkeys))
	 (slots (make-list nkeys))
	 w (w-t pv-wrappers) (s-t slots))
    (dolist (arg args)
      (cond ((std-instance-p arg)
	     (setq w (std-instance-wrapper arg))
	     (unless (eq 't (wrapper-state w))
	       (setq w (check-wrapper-validity arg)))
	     (setf (car w-t) w)
	     (setf (car s-t) (std-instance-slots arg)))
	    ((fsc-instance-p arg)
	     (setq w (fsc-instance-wrapper arg))
	     (unless (eq 't (wrapper-state w))
	       (setq w (check-wrapper-validity arg)))
	     (setf (car w-t) w)
	     (setf (car s-t) (fsc-instance-slots arg)))
	    (t (built-in-or-structure-wrapper arg))) ; might be a structure
      (setq w-t (cdr w-t))
      (setq s-t (cdr s-t)))
    (when (= nkeys 1) (setq pv-wrappers (car pv-wrappers)))
    (values pv-wrappers slots)))

(defun pv-wrappers-from-all-wrappers (isl wrappers)
  (let ((nkeys 0)
	(slot-name-lists (isl-slot-name-lists isl)))
    (dolist (sn slot-name-lists)
      (when sn (incf nkeys)))
    (let* ((pv-wrappers (make-list nkeys))
	   (pv-w-t pv-wrappers)
	   (all-std-p t))
      (dolist (sn slot-name-lists)
	(when sn
	  (let ((w (car wrappers)))
	    (unless w ; can-optimize-access prevents this from happening.
	      (error "error in pv-wrappers-from-all-wrappers"))
	    (setf (car pv-w-t) w)
	    (unless (member *the-class-standard-class*
			    (class-precedence-list (wrapper-class w)))
	      (setq all-std-p nil))
	    (setq pv-w-t (cdr pv-w-t))))
	(setq wrappers (cdr wrappers)))
      (when (= nkeys 1) (setq pv-wrappers (car pv-wrappers)))
      pv-wrappers)))

(defun isl-lookup (isl pv-wrappers)
  (let ((cache (isl-cache isl)))
    (or (probe-cache cache pv-wrappers)
	(let* ((pv-cell (make-pv-cell (lookup-pv isl pv-wrappers)))
	       (new-cache (fill-cache cache pv-wrappers pv-cell)))
	  (unless (eq new-cache cache)
	    (setf (isl-cache isl) new-cache)
	    (free-cache cache))
	  pv-cell))))



(defmethod wrapper-fetcher ((class standard-class))
  'std-instance-wrapper)

(defmethod slots-fetcher ((class standard-class))
  'std-instance-slots)

(defmethod raw-instance-allocator ((class standard-class))
  '%%allocate-instance--class)

