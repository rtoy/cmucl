;;; Tests for PCL, taken from src/pcl/rt.
;;;
;;; It's clear that the tests used mk defsystem to load the tests, but
;;; it's not clear if the tests were compiled or not before running.

(defpackage "PCL-TESTS"
  (:use "COMMON-LISP" "PCL" "LISP-UNIT"))

(in-package "PCL-TESTS")

(defmacro deftest (name form &rest values)
  (let ((results (gensym "RESULTS-")))
    `(define-test ,name
	 (:tag :pcl)
	 (let ((,results (multiple-value-list ,form)))
	   (assert-equalp ,results
			  ',values)))))

(defmacro define-compiled-test (name form &rest values)
  `(progn
     (defun ,name () ,form)
     (deftest ,name (,name) ,@values)))

;; ctor.lisp
(deftest plist-keys.0
    (pcl::plist-keys '())
  nil)

(deftest plist-keys.1
    (pcl::plist-keys '(:a 1 :b 2))
  (:a :b))

(deftest plist-keys.2
    (multiple-value-bind (result condition)
	(ignore-errors (pcl::plist-keys '(:a)))
      (values result (typep condition 'condition)))
  nil
  t)
      
(deftest make-instance->constructor-call.0
    (pcl::make-instance->constructor-call '(make-instance 'foo a x))
  nil)

(deftest make-instance->constructor-call.1
    (pcl::make-instance->constructor-call '(make-instance foo :a x))
  nil)

(deftest make-instance->constructor-call.2
    (pcl::make-instance->constructor-call '(make-instance 'foo x))
  nil)

(deftest make-instance->constructor-call.4
    (pcl::make-instance->constructor-call '(make-instance 1))
  nil)

(deftest make-instance->constructor-call.5
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t ())

(deftest make-instance->constructor-call.6
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x 1 :y 2)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t ())

(deftest make-instance->constructor-call.7
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x x :y 2)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t (x))

(deftest make-instance->constructor-call.8
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x x :y y)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t (x y))

(deftest make-instance->constructor-call.9
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x x :y 1)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t (x))

(deftest make-instance->constructor-call.10
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x x :y 1 :z z)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t (x z))

(deftest make-ctor.0
    (let ((ctor (pcl::make-ctor '(pcl::ctor bar) 'bar '(:x 1 :y 2))))
      (values (pcl::ctor-function-name ctor)
	      (pcl::ctor-class-name ctor)
	      (pcl::ctor-initargs ctor)))
  (pcl::ctor bar)
  bar
  (:x 1 :y 2))

(defclass foo ()
  ((a :initarg :a :initform 1)
   (b :initarg :b :initform 2)))

(defun call-generator (generator function-name class-name args)
  (declare (ignore function-name))
  (let* ((ctor
	   (pcl::make-ctor (list 'pcl::ctor class-name) class-name args))
	 (class (find-class class-name))
	 (proto (pcl::class-prototype class))
	 (ii (pcl::compute-applicable-methods
	      #'initialize-instance (list proto)))
	 (si (pcl::compute-applicable-methods
	      #'shared-initialize (list proto t))))
    (setf (pcl::ctor-class ctor) class)
    (if (eq generator #'pcl::fallback-generator)
	(funcall generator ctor)
	(funcall generator ctor ii si))))
     
(deftest fallback-generator.0
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '(:a 0 :b 1))))
      (values (second fn)
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  ()
  pcl::standard-class
  (:a 0 :b 1))

(deftest fallback-generator.1
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '(:a 0))))
      (values (second fn)
	      (first (third fn))
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  ()
  make-instance
  pcl::standard-class
  (:a 0))

(deftest fallback-generator.2
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '())))
      (values (second fn)
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  ()
  pcl::standard-class
  ())

(deftest fallback-generator.3
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '(:a .p0.))))
      (values (second fn)
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  (.p0.)
  pcl::standard-class
  (:a .p0.))

(deftest fallback-generator.4
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '(:a a :b b))))
      (values (second fn)
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  (a b)
  pcl::standard-class
  (:a a :b b))

;;; These depend on the actual slot definition location computation,
;;; which may be different in my PCL than in the CVS PCL.

(deftest compute-initarg-locations.0
    (let ((class (find-class 'foo)))
      (pcl::compute-initarg-locations class '(:a :b)))
  ((:a (0 . t)) (:b (1 . t))))

(defclass foo2 (foo)
  ((c :initarg :a)))

(deftest compute-initarg-locations.1
    (let ((class (find-class 'foo2)))
      (pcl::compute-initarg-locations class '(:a :b)))
  ((:a (0 . t) (2 . t)) (:b (1 . t))))

(defclass foo3 (foo)
  ((c :initarg :a :allocation :class)))

;;;
;;; This test must be compiled for the case that PCL::+SLOT-UNBOUND+
;;; is a symbol macro calling PCL::MAKE-UNBOUND-MARKER, otherwise
;;; we'll get a complaint that C::%%PRIMITIVE is not defined.
;;;
(define-compiled-test compute-initarg-locations.2
    (let ((class (find-class 'foo3)))
      (subst 'unbound pcl::+slot-unbound+
	     (pcl::compute-initarg-locations class '(:a :b))))
  ((:a (0 . t) ((c . unbound) . t)) (:b (1 . t))))

(defclass foo4 ()
  ((a :initarg :a :initarg :both)
   (b :initarg :b :initarg :both)))

(deftest compute-initarg-locations.3
    (let ((class (find-class 'foo4)))
      (pcl::compute-initarg-locations class '(:both :a :b)))
  ((:both (0 . t) (1 . t)) (:a) (:b)))

(deftest compute-initarg-locations.4
    (let ((class (find-class 'foo4)))
      (pcl::compute-initarg-locations class '(:a :both)))
  ((:a (0 . t)) (:both (1 . t))))

(deftest slot-init-forms.0
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo '(:a a :b b))))
      (setf (pcl::ctor-class ctor) (find-class 'foo))
      (pcl::slot-init-forms ctor nil))
  (let ()
      (declare (ignorable) (optimize (safety 3)))
      (setf (svref pcl::.slots. 0) (the t a))
      (setf (svref pcl::.slots. 1) (the t b)))
   nil)

(deftest slot-init-forms.1
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo '(:a a))))
      (setf (pcl::ctor-class ctor) (find-class 'foo))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t a))
    (setf (svref pcl::.slots. 1) (the t '2)))
  nil)

(defclass foo5 ()
  ((a :initarg :a :initform 0)
   (b :initarg :b)))

(deftest slot-init-forms.2
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo5 '(:a a))))
      (setf (pcl::ctor-class ctor) (find-class 'foo5))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t a))
    (setf (svref pcl::.slots. 1) pcl::+slot-unbound+))
  nil)

(defclass foo5a ()
  ((a :initarg :a :initform 0)
   (b :initarg :b :initform 0)))

(deftest slot-init-forms.2a
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo5a '())))
      (setf (pcl::ctor-class ctor) (find-class 'foo5a))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t '0))
    (setf (svref pcl::.slots. 1) (the t '0)))
  nil)

(defclass foo6 ()
  ((a :initarg :a :initform 0 :allocation :class)
   (b :initarg :b)))

(deftest slot-init-forms.3
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo6 '(:a a))))
      (setf (pcl::ctor-class ctor) (find-class 'foo6))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) pcl::+slot-unbound+)
    (setf (cdr '(a . 0)) (the t a)))
  nil)

(defun foo ()
  (error "should never be called"))

(defclass foo7 ()
  ((a :initarg :a :initform (foo))
   (b :initarg :b)))

(deftest slot-init-forms.4
    (let* ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo7 '())))
      (setf (pcl::ctor-class ctor) (find-class 'foo7))
      (let ((form (pcl::slot-init-forms ctor nil)))
	(destructuring-bind (let vars declare setf1 setf2) form
	  (declare (ignore let vars declare))
	  (values setf2 (second setf1) (first (third (third setf1)))
		  (functionp (second (third (third setf1))))))))
  (setf (svref pcl::.slots. 1) pcl::+slot-unbound+)
  (svref pcl::.slots. 0)
  funcall
  t)

(deftest slot-init-forms.5
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo '(:a '(foo)))))
      (setf (pcl::ctor-class ctor) (find-class 'foo))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t '(foo)))
    (setf (svref pcl::.slots. 1) (the t '2)))
  nil)

(deftest slot-init-forms.6
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo '(:a 'x))))
      (setf (pcl::ctor-class ctor) (find-class 'foo))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t 'x))
    (setf (svref pcl::.slots. 1) (the t '2)))
  nil)

(defmethod bar1 ((x integer))
  (* x 2))

(defmethod bar2 ((x integer)) x)
(defmethod bar2 :around ((x integer)) x)

(deftest around-or-nonstandard-primary-method-p.0
    (pcl::around-or-nonstandard-primary-method-p
     (pcl::compute-applicable-methods #'bar2 (list 1)))
  t)

(defmethod bar3 ((x integer)) x)
(defmethod bar3 :after ((x integer)) x)

(deftest around-or-nonstandard-primary-method-p.1
    (pcl::around-or-nonstandard-primary-method-p
     (pcl::compute-applicable-methods #'bar3 (list 1)))
  nil)

(deftest optimizing-generator.0
    (let ((fn (call-generator #'pcl::optimizing-generator
			      'make-foo 'foo '(:a 0 :b 1))))
      (second fn))
  ())

(defun construct (class-name initargs &rest args)
  (let* ((form (call-generator #'pcl::optimizing-generator
			       'some-function-name
			       class-name
			       initargs))
	 (fn (pcl::compile-lambda form)))
    (apply fn args)))

(deftest optimizing-generator.1
    (with-slots (a b) (construct 'foo '(:a 0 :b 1))
      (values a b))
  0 1)

(deftest optimizing-generator.2
    (with-slots (a b) (construct 'foo '())
      (values a b))
  1 2)

(defclass g1 ()
  ((a :initform 0)
   (b)))

(deftest optimizing-generator.3
    (let ((instance (construct 'g1 '())))
      (values (slot-value instance 'a)
	      (slot-boundp instance 'b)))
  0 nil)

;; Test for default-initargs bug.
(defclass g2 ()
  ((a :initarg :aa)))

(defmethod initialize-instance :after ((f g2) &key aa)
  (princ aa))

(defclass g3 (g2)
  ((b :initarg :b))
  (:default-initargs :aa 5))

(deftest defaulting-initargs.1
    (with-output-to-string (*standard-output*)
      (make-instance 'g3))
  "5")

;; defclass.lisp
(deftest defclass-subtypep.0
    (progn
      (defclass st0 () ())
      (defclass st1 () ())
      (subtypep 'st1 'st0))
  nil t)

(deftest defclass-subtypep.1
    (progn
      (defclass st1 (st0) ())
      (subtypep 'st1 'st0))
  t t)

(deftest defclass-subtypep.2
    (progn
      (defclass st1 () ())
      (subtypep 'st1 'st0))
  nil t)
     
(defvar *instance* nil)
(defvar *update-instance-result* nil)

(defclass st2 ()
  ((a :initform 0 :accessor a)))

(defclass st3 ()
  ((b :initform 0 :accessor b)))

(deftest update-instance-for-redefined-class.0
    (progn
      (setq *instance* (make-instance 'st3))
      t)
  t)

(defmethod update-instance-for-redefined-class :after
    ((instance st3) added-slots discarded-slots property-list &rest initargs)
  (setq *update-instance-result*
	(list instance added-slots discarded-slots property-list initargs)))

(deftest update-instance-for-redefined-class.1
    (progn
      (defclass st3 (st2)
	((b :initform 0 :accessor b)))
      (values (slot-value *instance* 'b)
	      (eq *instance* (first *update-instance-result*))
	      (rest *update-instance-result*)))
  0 t ((a) nil nil nil))

(deftest update-instance-for-redefined-class.2
    (progn
      (defclass st3 ()
	((b :initform 0 :accessor b)))
      (values (slot-value *instance* 'b)
	      (eq *instance* (first *update-instance-result*))
	      (rest *update-instance-result*)))
  0 t (nil (a) (a 0) nil))

(deftest defclass-sxhash.0
    (let ((i1 (make-instance 'st2))
	  (i2 (make-instance 'st2)))
      (/= (sxhash i1) (sxhash i2)))
  t)

(deftest generic-function-sxhash.0
    (/= (sxhash #'allocate-instance)
	(sxhash #'make-instance))
  t)

(deftest defclass-redefinition.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass rd0 () ())
	  (defclass rd1 (rd0) ())
	  (defclass rd2 () ())
	  (defclass rd0 (rd2) ())
	  (make-instance 'rd1))
      (values (not (null r)) (null c)))
  t t)

;;; This failed to compile in an old version, that's why it's here.

(deftest defclass-inherited-class-slots.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ics0 ()
	    ((a :allocation :class :accessor ics0-a)))
	  (defclass ics1 (ics0)
	    ())
	  (make-instance 'ics1))
      (values (not (null r)) (null c)))
  t t)

(defmacro define-defclass-syntax-test (name class-body &rest options)
  `(deftest ,name
       (multiple-value-bind (r c)
	   (ignore-errors
	     (defclass dc0 ()
	       ,class-body ,@options))
	 (declare (ignore r))
	 (typep c 'error))
     t))

;; CLHS: allocation should be :class or :instance
(define-defclass-syntax-test defclass.0 ((a :allocation :foo)))

;; Reader names should be symbols.
(define-defclass-syntax-test defclass.1 ((a :reader (setf a))))

;;; initarg names must be symbols.
(define-defclass-syntax-test defclass.2 ((a :initarg 1)))

;; Duplicate :default-initargs is an error.
(define-defclass-syntax-test defclass.3 ()
  (:default-initargs :a 1)
  (:default-initargs :b 2))

;; Duplicate :metaclass.
(define-defclass-syntax-test defclass.4 ()
  (:metaclass pcl::funcallable-standard-class)
  (:metaclass 1))

;; class option that is not implemented locally -> error
(define-defclass-syntax-test defclass.5 ()
  (:foo t))

(deftest defclass-structure-class.0
  (multiple-value-bind (r c)
      (ignore-errors
	(defclass dscl.0 ()
	  (a b)
	  (:metaclass pcl::structure-class))
	t)
    (values r (null c)))
  t t)

(deftest defclass-structure-class.1
  (multiple-value-bind (r c)
      (ignore-errors
	(make-instance 'dscl.0)
	t)
    (values r (null c)))
  t t)

;;;
;;; The change of DFR1 from forward-referenced to standard class
;;; caused problems at some point, which were fixed by passing
;;; initargs to CHANGE-CLASS in ENSURE-CLASS-USING-CLASS.
;;;
(deftest defclass-forward-referenced-class.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr0 (dfr1 dfr2) ())
	  (defclass dfr1 (dfr3 dfr4) ())
	  t)
      (values r (null c)))
  t t)

(deftest defclass-forward-referenced-class.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr.c1 (dfr.c2) ())
	  (defclass dfr.c2 (dfr.c3) ())
	  (defclass dfr.c3 () ())
	  (make-instance 'dfr.c1)
	  t)
      (values r (null c)))
  t t)

;;;
;;; TYPEP and SUBTYPEP used to fail with forward-referenced/unfinalized
;;; classes.
;;;
(deftest defclass-types.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr5 (dfr6) ())
	  (typep t (find-class 'dfr6)))
      (values r (null c)))
  nil t)

(deftest defclass-types.2
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr7 (dfr8) ())
	  (multiple-value-list
	   (subtypep (find-class 'dfr7) (find-class 'dfr8))))
      (values r (null c)))
  (t t) t)

(deftest defclass-types.3
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr7 (dfr8) ())
	  (multiple-value-list
	   (subtypep (find-class 'dfr8) (find-class 'dfr7))))
      (values r (null c)))
  (nil t) t)

(deftest defclass-types.4
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr9 (dfr10) ())
	  (defclass dfr11 (dfr9 dfr12) ())
	  (append
	   (multiple-value-list
	    (subtypep (find-class 'dfr9) (find-class 'dfr10)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr11) (find-class 'dfr10)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr11) (find-class 'dfr9)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr11) (find-class 'dfr12)))))
      (values r (null c)))
  (t t t t t t t t) t)

(deftest defclass-types.5
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr13 () ())
	  (defclass dfr14 (dfr15 dfr13) ())
	  (defclass dfr16 (dfr14 dfr17) ())
	  (append
	   (multiple-value-list
	    (subtypep (find-class 'dfr16) (find-class 'dfr14)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr16) (find-class 'dfr17)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr16) (find-class 'dfr15)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr16) (find-class 'dfr13)))))
      (values r (null c)))
  (t t t t t t t t) t)

(deftest defclass-types.6
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr20 (dfr21) ())
	  (defclass dfr21 (dfr22) ())
	  (append 
	   (multiple-value-list
	    (subtypep (find-class 'dfr20) (find-class 'dfr21)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr21) (find-class 'dfr22)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr20) (find-class 'dfr22)))))
      (values r (null c)))
  (t t t t t t) t)

;; defmethod.lisp
(defmethod dm0 (x)
  x)

(defmethod dm1 (x &rest y)
  (list x y))

(defmethod dm2 (x &optional y)
  (list x y))

(defmacro define-defmethod-test (name method qual lambda-list
				 &rest values)
  `(deftest ,name
     (multiple-value-bind (r c)
	 (ignore-errors
	   (defmethod ,method ,@(when qual `(,qual)) ,lambda-list
	     #+cmu (declare (optimize (ext:inhibit-warnings 3)))
	     nil))
       (values (typep r 'method)
	       (typep c 'error)
	       (length (pcl:generic-function-methods #',method))))
     ,@values))

(defmacro define-defmethod-test-1 (name method qual lambda-list)
  `(define-defmethod-test ,name ,method ,qual ,lambda-list nil t 1))

(define-defmethod-test-1 defmethod.0 dm0 nil (x y))
(define-defmethod-test-1 defmethod.1 dm0 nil (x &rest y))
(define-defmethod-test-1 defmethod.2 dm0 nil (x &key y))
(define-defmethod-test-1 defmethod.4 dm0 :before (x y))
(define-defmethod-test-1 defmethod.5 dm0 :before (x &rest y))
(define-defmethod-test-1 defmethod.6 dm0 :before (x &key y))
(define-defmethod-test defmethod.7 dm0 nil (x) t nil 1)
    
(define-defmethod-test-1 defmethod.10 dm1 nil (x y))
(define-defmethod-test-1 defmethod.11 dm1 nil (x))
(define-defmethod-test defmethod.12 dm1 nil (x &key y) t nil 1)
(define-defmethod-test defmethod.13 dm1 nil (x &key y z) t nil 1)
(define-defmethod-test defmethod.14 dm1 nil (x &rest y) t nil 1)

(define-defmethod-test-1 defmethod.20 dm2 nil (x))
(define-defmethod-test-1 defmethod.21 dm2 nil (x &optional y z))
(define-defmethod-test-1 defmethod.22 dm2 nil (x &key y))

;;;
;;; A forward-referenced class used as specializer signaled an
;;; error at some point.
;;;
(deftest defmethod-forwared-referenced.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dm.3 () ())
	  (defclass dm.4 (dm.forward) ())
	  (defmethod dm.5 ((x dm.3)) x)
	  (defmethod dm.5 ((x dm.4)) x)
	  t)
      (values r (null c)))
  t t)
      
(deftest defmethod-forwared-referenced.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dm.3 () ())
	  (defclass dm.4 (dm.forward) ())
	  (defmethod dm.5 ((x dm.3)) x)
	  (defmethod dm.5 ((x dm.4)) x)
	  (dm.5 (make-instance 'dm.3))
	  t)
      (values r (null c)))
  t t)

(deftest defmethod-metacircle.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dmm.0 () ())
	  (defclass dmm.1 () ())
	  (defclass dmm.0+1 (dmm.0 dmm.1) ())
	  (defmethod dmm.0 ((x dmm.0) (y dmm.1)) 1)
	  (defmethod dmm.0 ((x dmm.1) (y dmm.0)) 2)
	  (dmm.0 (make-instance 'dmm.0+1) (make-instance 'dmm.0+1))
	  (defmethod dmm.0 ((x dmm.0+1) (y dmm.0+1)) 3)
	  (dmm.0 (make-instance 'dmm.0+1) (make-instance 'dmm.0+1)))
      (values r (null c)))
  3 t)

(deftest defmethod-setf-fdefinition.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defgeneric dsf.0 (x))
	  (defmethod dsf.0 ((x integer)) x)
	  (setf (fdefinition 'dsf.1) #'dsf.0)
	  (defmethod dsf.1 ((x string)) x)
	  (list (length (mop:generic-function-methods #'dsf.0))
		(equal (mop:generic-function-methods #'dsf.1)
		       (mop:generic-function-methods #'dsf.0))))
      (values r (null c)))
  (2 t) t)

(deftest defmethod-setf-fdefinition.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (defgeneric dsf.2 (x))
	  (defmethod dsf.2 ((x integer)) x)
	  (setf (fdefinition 'dsf.3) #'dsf.2)
	  (defmethod dsf.3 ((x integer)) x)
	  (list (length (mop:generic-function-methods #'dsf.2))
		(equal (mop:generic-function-methods #'dsf.3)
		       (mop:generic-function-methods #'dsf.2))))
      (values r (null c)))
  (1 t) t)

;; find-method.lisp
(defmethod fm0 (x y)
  (list x y))

(deftest find-method.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (find-method #'fm0 nil (list t)))
      (values r (typep c 'error)))
  nil t)

(deftest find-method.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (find-method #'fm0 nil (list t t)))
      (values (typep r 'method) (typep c 'error)))
  t nil)


;; inline-access.lisp
(defun test-walk (form test-function &optional env)
  (let ((result nil))
    (flet ((walk-function (form context env)
	     (declare (ignore context))
	     (when (and (consp form) (eq (car form) 'test))
	       (push (funcall test-function env) result))
	     form))
      (walker:walk-form form env #'walk-function)
      (nreverse result))))

(defmacro define-declaration-test (name declaration test &key values)
  `(deftest ,name
       (test-walk '(defun dummy () ,declaration (test))
		  (lambda (env) ,test))
     ,@values))

(define-declaration-test slot-declaration.0
    (declare (ext:slots (slot-boundp xx)))
  (pcl::slot-declaration env 'slot-boundp 'xx)
  :values ((t)))

(define-declaration-test slot-declaration.1
    (declare (ext:slots (inline xx)))
  (pcl::slot-declaration env 'inline 'xx)
  :values ((t)))

(define-declaration-test slot-declaration.2
    (declare (ext:slots (inline (xx))))
  (pcl::slot-declaration env 'inline 'xx)
  :values ((t)))

(define-declaration-test slot-declaration.3
    (declare (ext:slots (inline (xx a))))
  (pcl::slot-declaration env 'inline 'xx 'a)
  :values ((t)))

(define-declaration-test slot-declaration.4
    (declare (ext:slots (inline (xx a))))
  (pcl::slot-declaration env 'inline 'xx 'b)
  :values ((nil)))

(define-declaration-test slot-declaration.5
    (declare (ext:slots (inline (xx a) yy)))
  (pcl::slot-declaration env 'inline 'yy)
  :values ((t)))

(define-declaration-test slot-declaration.6
    (declare (ext:slots (inline (xx a) (yy a))))
  (pcl::slot-declaration env 'inline 'yy 'a)
  :values ((t)))

(define-declaration-test slot-declaration.7
    (declare (ext:slots (inline (xx a) (yy a))))
  (pcl::slot-declaration env 'inline 'yy 'b)
  :values ((nil)))

(deftest global-slot-declaration.0
    (progn
      (proclaim '(ext:slots (slot-boundp gsd)))
      (not (null (pcl::slot-declaration nil 'slot-boundp 'gsd))))
  t)

(deftest global-slot-declaration.1
    (progn
      (proclaim '(ext:slots (inline (gsd gsd-a))))
      (not (null (pcl::slot-declaration nil 'inline 'gsd 'gsd-a))))
  t)

(deftest auto-compile-declaration.0
    (progn
      (proclaim '(ext:auto-compile acd))
      (pcl::auto-compile-p 'acd nil nil))
  t)

(deftest auto-compile-declaration.1
    (progn
      (proclaim '(ext:auto-compile acd))
      (pcl::auto-compile-p 'acd '(:around) '(t t)))
  t)

(deftest auto-compile-declaration.2
    (progn
      (proclaim '(ext:not-auto-compile acd))
      (proclaim '(ext:auto-compile (acd :around (t t))))
      (values (pcl::auto-compile-p 'acd nil nil)
	      (pcl::auto-compile-p 'acd nil '(t t))
	      (pcl::auto-compile-p 'acd '(:around) '(t t))))
  nil nil t)

(deftest auto-compile-declaration.3
    (progn
      (proclaim '(ext:auto-compile acd))
      (proclaim '(ext:not-auto-compile (acd :around (t t))))
      (values (pcl::auto-compile-p 'acd nil nil)
	      (pcl::auto-compile-p 'acd nil '(t t))
	      (pcl::auto-compile-p 'acd '(:around) '(t t))))
  t t nil)

(deftest auto-compile-declaration.4
    (progn
      (proclaim '(ext:auto-compile))
      (proclaim '(ext:not-auto-compile acd))
      (values (pcl::auto-compile-p 'foo nil nil)
	      (pcl::auto-compile-p 'acd nil '(t t))
	      (pcl::auto-compile-p 'acd '(:around) '(t t))))
  t nil nil)

(deftest auto-compile-declaration.5
    (progn
      (proclaim '(ext:auto-compile (setf acd)))
      (pcl::auto-compile-p '(setf acd) '(:around) '(t t)))
  t)


(declaim (ext:slots (inline sacc.0)))

(defclass sacc.0 ()
  ((a :initform 0 :initarg :a :accessor sacc.0-a)))

(defclass sacc.1 (sacc.0)
  ((b :initform 0 :initarg :b :accessor sacc.1-b)
   (a :initform 0 :initarg :a :accessor sacc.0-a)))

(defmethod sacc.0.0 ((x sacc.0))
  (slot-value x 'a))

(defmethod sacc.0.1 ((x sacc.0))
  (sacc.0-a x))

(defmethod sacc.0.2 ((x sacc.0) nv)
  (setf (slot-value x 'a) nv))

(defmethod sacc.0.3 ((x sacc.0) nv)
  (setf (sacc.0-a x) nv))

(defun method-using-inline-access-p (class-name method-name qualifiers
				     specializers)
  (let ((method (find-method (fdefinition method-name) qualifiers
			     specializers)))
    (car (member class-name (pcl::plist-value method 'pcl::inline-access)
		 :test #'eq))))

(deftest inline-access-p.0
    (and (method-using-inline-access-p 'sacc.0 'sacc.0.0 nil '(sacc.0))
	 (method-using-inline-access-p 'sacc.0 'sacc.0.1 nil '(sacc.0))
	 (method-using-inline-access-p 'sacc.0 'sacc.0.2 nil '(sacc.0 t))
	 (method-using-inline-access-p 'sacc.0 'sacc.0.3 nil '(sacc.0 t)))
  sacc.0)

(deftest inline-access-p.1
    (let ((methods (pcl::methods-using-inline-slot-access
		    (pcl::find-class 'sacc.0))))
      (length methods))
  4)

(deftest inline-access.0
    (sacc.0.0 (make-instance 'sacc.0))
  0)

(deftest inline-access.1
    (let ((instance (make-instance 'sacc.0 :a 11)))
      (values (sacc.0.0 instance)
	      (sacc.0.1 instance)))
  11 11)

(deftest inline-access.2
    (let ((instance (make-instance 'sacc.0 :a 11)))
      (sacc.0.2 instance 10)
      (slot-value instance 'a))
  10)

(deftest inline-access.3
    (let ((instance (make-instance 'sacc.0 :a 11)))
      (sacc.0.3 instance 10)
      (slot-value instance 'a))
  10)

(defmacro define-warning-test (name (value) &body body)
  `(deftest ,name
       (let (warning)
	 (flet ((note-warning (c)
		  (declare (ignore c))
		  (setq warning t)
		  (muffle-warning)))
	   (handler-bind ((warning #'note-warning))
	     ,@body)
	   warning))
     ,value))

(define-warning-test warn.0 (t) (warn "Test the test"))  
(define-warning-test warn.1 (nil) nil)

(define-warning-test inline-warn.0 (nil)
  (defclass sacc.0 ()
    ((a :initform 0 :initarg :a :accessor sacc.0-a))))

(define-warning-test inline-warn.1 (t)
  (defclass sacc.0 ()
    ((a :initform 0 :initarg :a :accessor sacc.0-a)
     (b :initform 0))))

(define-warning-test inline-warn.2 (t)
  (progn
    (defmethod inline-warn.2.method ((x sacc.1))
      (declare (pcl::slots (inline sacc.1)))
      (slot-value x 'b))
    (defclass sacc.0 ()
      ((a :initform 0 :initarg :a :accessor sacc.0-a)))))
      

;; make-instance.lisp
;;; *********************
;;; MAKE-INSTANCE  ******
;;; *********************

;;; Test forms in DEFTEST are not compiled, that is, a compiler
;;; macro won't be used in them.  Also, we want tests using
;;; both the optimized constructor functions, and the default.

(eval-when (:load-toplevel :compile-toplevel :execute)
(defmacro define-mi-test (name form &key values opt-values)
  (let ((optimized-name
	 (let ((*print-case* :upcase)
	       (*print-pretty* nil)
	       (*print-gensym* t))
	   (intern (format nil "~S.OPT" name))))
	(optimized-values (or opt-values values)))
    `(progn
       (defun ,name ()
	 (macrolet ((mi (&rest args)
		      `(funcall #'make-instance ,@args)))
	   ,form))
       (defun ,optimized-name ()
	 (macrolet ((mi (&rest args)
		      `(make-instance ,@args)))
	   ,form))
       (deftest ,name (,name) ,@values)
       (deftest ,optimized-name (,optimized-name)
	 ,@optimized-values))))
)
    

(defclass m1 ()
  ((a :initarg :a :initarg :both :initform 1)
   (b :initarg :b :initarg :both :initform 2)))

(define-mi-test make-instance.0
    (with-slots (a b) (mi 'm1)
      (values a b))
  :values (1 2))
    
(define-mi-test make-instance.1
    (with-slots (a b) (mi 'm1 :a 3)
      (values a b))
  :values (3 2))
    
(define-mi-test make-instance.2
    (with-slots (a b) (mi 'm1 :b 3)
      (values a b))
  :values (1 3))

(define-mi-test make-instance.3
    (with-slots (a b) (mi 'm1 :b 3 :a 4)
      (values a b))
  :values (4 3))
    
(define-mi-test make-instance.4
    (with-slots (a b) (mi 'm1 :both (list nil))
      (eq a b))
  :values (t))

(defclass m2 (m1)
  ((a :initarg :a :initform 3)))

;;; Overriding slot in subclass -> new initform should be used.

(define-mi-test make-instance.5
    (with-slots (a b) (mi 'm2)
      (values a b))
  :values (3 2))

;;; :BOTH should be inherited by slot A.

(define-mi-test make-instance.6
    (with-slots (a b) (mi 'm2 :both 11)
      (values a b))
  :values (11 11))

(defclass m3 (m2)
  ((a :allocation :class :initform nil)))

;;; Class slot should not be overwritten when there's no initarg for it.
;;; Note that slot A overrides an instance slot A in M2 which itself
;;; overrides an instance slot in M1.

(define-mi-test make-instance.7
    (progn
      (setf (slot-value (pcl:class-prototype (pcl:find-class 'm3)) 'a) 1)
      (with-slots (a b) (mi 'm3)
	(values a b)))
  :values (1 2))

;;; Class slot should be written when there is an initarg for it.

(define-mi-test make-instance.8
    (with-slots (a) (mi 'm3 :a 11)
      a)
  :values (11))

;;; Class slot should be written when there is an initarg for it.

(define-mi-test make-instance.9
    (with-slots (a b) (mi 'm3 :both 12)
      (values a b))
  :values (12 12))

(define-mi-test make-instance.10
    (with-slots (a b) (mi 'm3 :both 13)
      (values a b))
  :values (13 13))

;;; Invalid initialization arguments

(define-mi-test make-instance.11
    (multiple-value-bind (r c)
	(ignore-errors (mi 'm3 :hansi t))
      (values r (typep c 'condition)))
  :values (nil t))

(define-mi-test make-instance.12
    (multiple-value-bind (r c)
	(ignore-errors (mi 'm3 :hansi t :allow-other-keys t))
      (values (slot-value r 'b) (typep c 'condition)))
  :values (2 nil))

;;; Default initargs

(defclass m5 (m1)
  ()
  (:default-initargs :a 'a :b 'b))

(define-mi-test make-instance.13
    (with-slots (a b) (mi 'm5)
      (values a b))
  :values (a b))

(defclass m6 (m5)
  ()
  (:default-initargs :a 'c))

(define-mi-test make-instance.14
    (with-slots (a b) (mi 'm6)
      (values a b))
  :values (c b))

(defclass m7 (m6)
  ((a :allocation :class :initform nil)))

(define-mi-test make-instance.15
    (with-slots (a b) (mi 'm7)
      (values a b))
  :values (c b))

;;; Lexical environment.

(let ((x 0))
  (defclass m8 ()
    ((a :initform (incf x))))
  (defun reset-counter ()
    (setq x 0)))

(define-mi-test make-instance.16
    (progn
      (reset-counter)
      (loop for i below 5
	    collect (slot-value (mi 'm8) 'a)))
  :values ((1 2 3 4 5)))

(defclass m9 ()
  ((a :initarg :a)
   (b :initarg :b)
   (c :initarg :c)
   (d :initarg :d)))

(define-mi-test make-instance.17
    (let* ((x 'x)
	   (instance (mi 'm9 :a () :b x :c '(baz bar foo)
			 :d (lambda () ()))))
      (with-slots (a b c) instance
	(values a b c)))
  :values (nil x (baz bar foo)))

;; After and before methods.

(defclass m10 ()
  ((a :initform 0 :initarg :a)
   (b :initarg :b)
   (c :initform 2 :initarg :c))
  (:default-initargs :c 1))

(defvar *result* ())

(defmethod initialize-instance :before ((x m10) &rest args)
  (declare (ignore args))
  (push (list 'm10 :before (slot-boundp x 'a)
	      (slot-boundp x 'b) (slot-boundp x 'c))
	*result*))

(define-mi-test make-instance.18
    (progn
      (setq *result* ())
      (with-slots (a b c) (mi 'm10 :b 42)
	(values *result* a b c)))
  :values (((m10 :before nil nil nil)) 0 42 1))

(defclass m11 (m10)
  ()
  (:default-initargs :c 11))

(defmethod initialize-instance :before ((x m11) &rest args)
  (declare (ignore args))
  (push (list 'm11 :before (slot-boundp x 'a)
	      (slot-boundp x 'b)
	      (slot-boundp x 'c))
	*result*))

(defmethod initialize-instance :after ((x m11) &rest args)
  (declare (ignore args))
  (push (list 'm11 :after (slot-boundp x 'a)
	      (slot-boundp x 'b)
	      (slot-boundp x 'c))
	*result*))

(define-mi-test make-instance.19
    (progn
      (setq *result* ())
      (with-slots (a b c) (mi 'm11 :b 42)
	(values *result* a b c)))
  :values (((m11 :after t t t)
	    (m10 :before nil nil nil)
	    (m11 :before nil nil nil))
	   0 42 11))

(defclass m12 (m10)
  ()
  (:default-initargs :c 13))

(defmethod initialize-instance :before ((x m12) &rest args)
  (declare (ignore args))
  (setf (slot-value x 'a) 77))

(define-mi-test make-instance.20
    (progn
      (setq *result* ())
      (with-slots (a b c) (mi 'm12 :b 42)
	(values *result* a b c)))
  :values (((m10 :before t nil nil))
	   77 42 13))

(define-mi-test make-instance.21
    (progn
      (setq *result* ())
      (with-slots (a b c) (mi 'm12 :b 41 :c 67)
	(values *result* a b c)))
  :values (((m10 :before t nil nil))
	   77 41 67))

;;; :ALLOW-OTHER-KEYS

(define-mi-test make-instance.22
    (let ((obj (ignore-errors (mi 'm12 :b 41 :allow-other-keys t))))
      (when obj
	(with-slots (a b c) obj
	  (values a b c))))
  :values (77 41 13))


(define-mi-test make-instance.23
    (let ((obj (ignore-errors (mi 'm12 :b 41 :x 11 :allow-other-keys t))))
      (when obj
	(with-slots (a b c) obj
	  (values a b c))))
  :values (77 41 13))

(define-mi-test make-instance.24
    (multiple-value-bind (r c)
	(ignore-errors (mi 'm12 :b 41 :x 11))
      (values r (typep c 'condition)))
  :values (nil t))

(define-mi-test make-instance.25
    (multiple-value-bind (r c)
	(ignore-errors (mi 'm12 :b 41 :x 11 :allow-other-keys nil))
      (values r (typep c 'condition)))
  :values (nil t))

;; Create a constructor, than rename the package of the class it was
;; defined for.

(defpackage "%CTOR"
  (:use "COMMON-LISP"))

(in-package "%CTOR")

(defclass p1 ()
  ((a :initform 0)))

(defun f1 ()
  (make-instance 'p1))

(in-package "PCL-TESTS")

(define-mi-test make-instance.26
    (progn
      (rename-package "%CTOR" "%CTOR2")
      (let* ((f (find-symbol "F1" "%CTOR2"))
	     (a (find-symbol "A" "%CTOR2"))
	     (i (funcall f)))
	(prog1
	    (slot-value i a)
	  (rename-package "%CTOR2" "%CTOR"))))
  :values (0))

(defclass stru.0 ()
  ((a :initarg :a :accessor a-accessor)
   (b :initform 2 :reader b-reader))
  (:metaclass structure-class))

(defclass stru.1 (stru.0)
  ((c :initarg :c :writer c-writer :accessor c-accessor))
  (:metaclass structure-class))

(define-mi-test make-instance.27
    (with-slots (a b) (mi 'stru.0)
      (values a b))
  :values (nil 2))

(define-mi-test make-instance.28
    (with-slots (a b) (mi 'stru.0 :a 1)
      (values a b))
  :values (1 2))

(define-mi-test make-instance.29
    (with-slots (a b c) (mi 'stru.1)
      (values a b c))
  :values (nil 2 nil))

(define-mi-test make-instance.30
    (with-slots (a b c) (mi 'stru.1 :a 1 :c 3)
      (values a b c))
  :values (1 2 3))

(deftest make-instance.31
    (let ((*m30* nil))
      (declare (special *m30*))
      (defclass m30 () ())
      (defclass m31 (m30) ())
      (defun f () (make-instance 'm31))
      (compile 'f)
      (f)
      (defmethod initialize-instance :before ((x m30) &rest args)
	(declare (ignore args))
	(declare (special *m30*))
	(setq *m30* t))
      (f)
      *m30*)
  t)

(defclass mi13 ()
  ((s1 :initarg :s1a :initarg :s1b :reader s1)
   (s2 :initarg :s2 :reader s2)))

(define-mi-test make-instance.32
    (with-slots (s1 s2) 
	(make-instance 'mi13 :s2 'a :s1a 'b :s2 'x :s1a 'y :s1b 'z)
      (values s1 s2))
  :values (b a))

;; (setf find-class), class redefinitions

;; method-combination.lisp
;;; ********************************
;;; Method Group Specifiers ********
;;; ********************************

(define-method-combination mgs0 (x)
  ((primary () :required t))
  (progn
    x
    `(call-method ,(first primary))))

;;; This should simply not signal an error as it did in 18d.

(deftest method-group-specifiers.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defgeneric mgs0 (obj)
	    (:method-combination mgs0 1))
	  (defmethod mgs0 (obj)
	    obj)
	  (mgs0 1))
      (values r c))
  1 nil)


;;; **************************
;;; :generic-function  *******
;;; **************************


;;; *******************
;;; :arguments  *******
;;; *******************

(defvar *result* nil)

(defvar *mca0-value*
  (define-method-combination mca0 ()
    ((methods *))
    (:arguments x y &optional opt)
    (:generic-function gf)
    `(progn
       (setq *result* (list (pcl:generic-function-name ,gf) ,x ,y ,opt))
       (call-method ,(first methods)))))

(defgeneric mca0 (a)
  (:method-combination mca0)
  (:method (a) a))

(defgeneric mca1 (a b)
  (:method-combination mca0)
  (:method (a b) (list a b)))

(defgeneric mca2 (a &optional b)
  (:method-combination mca0)
  (:method (a &optional b) (list a b)))

(defgeneric mca3 (&optional b)
  (:method-combination mca0)
  (:method (&optional b) b))

(deftest method-combination.0
    *mca0-value*
  mca0)

(deftest method-combination-arguments.0
    (multiple-value-bind (r c)
	(ignore-errors (mca0 1) *result*)
      (values r (null c)))
  (mca0 1 nil nil) t)

(deftest method-combination-arguments.1
    (multiple-value-bind (r c)
	(ignore-errors (mca1 1 2) *result*)
      (values r (null c)))
  (mca1 1 2 nil) t)

(deftest method-combination-arguments.2
    (multiple-value-bind (r c)
	(ignore-errors (mca2 1) *result*)
      (values r (null c)))
  (mca2 1 nil nil) t)

(deftest method-combination-arguments.3
    (multiple-value-bind (r c)
	(ignore-errors (mca2 1 2) *result*)
      (values r (null c)))
  (mca2 1 nil 2) t)

(deftest method-combination-arguments.4
    (multiple-value-bind (r c)
	(ignore-errors (mca3) *result*)
      (values r (null c)))
  (mca3 nil nil nil) t)

(deftest method-combination-arguments.5
    (multiple-value-bind (r c)
	(ignore-errors (mca3 1) *result*)
      (values r (null c)))
  (mca3 nil nil 1) t)

(define-method-combination mca1 ()
  ((methods *))
  (:arguments x y &rest r)
  (:generic-function gf)
  `(progn
     (setq *result* (list (pcl:generic-function-name ,gf) ,x ,y ,r))
     (call-method ,(first methods))))

(defgeneric mca1.0 (&rest b)
  (:method-combination mca1)
  (:method (&rest b) b))

(deftest method-combination-arguments.6
    (multiple-value-bind (r c)
	(ignore-errors (mca1.0) *result*)
      (values r (null c)))
  (mca1.0 nil nil nil) t)

(deftest method-combination-arguments.7
    (multiple-value-bind (r c)
	(ignore-errors (mca1.0 1) *result*)
      (values r (null c)))
  (mca1.0 nil nil (1)) t)

(define-method-combination mca2 ()
  ((methods *))
  (:arguments &key a b)
  (:generic-function gf)
  `(progn
     (setq *result* (list (pcl:generic-function-name ,gf) ,a ,b))
     (call-method ,(first methods))))

(defgeneric mca2.0 (&key a b)
  (:method-combination mca2)
  (:method (&key (a 0) (b 1)) (list a b)))

(deftest method-combination-arguments.8
    (multiple-value-bind (r c)
	(ignore-errors (mca2.0) *result*)
      (values r (null c)))
  (mca2.0 nil nil) t)

(deftest method-combination-arguments.9
    (multiple-value-bind (r c)
	(ignore-errors (mca2.0 :a 1) *result*)
      (values r (null c)))
  (mca2.0 1 nil) t)

(deftest method-combination-arguments.10
    (multiple-value-bind (r c)
	(ignore-errors (mca2.0 :b 1) *result*)
      (values r (null c)))
  (mca2.0 nil 1) t)

(deftest method-combination-arguments.11
    (multiple-value-bind (r c)
	(ignore-errors (mca2.0 :b 1 :a 0) *result*)
      (values r (null c)))
  (mca2.0 0 1) t)

(define-method-combination mca3 ()
   ((methods *))
   (:arguments &whole w x &key k)
   (:generic-function gf)
   `(progn
      (setq *result* (list (pcl:generic-function-name ,gf) ,w ,x ,k))
      (call-method ,(first methods))))

(defgeneric mca3.0 (x &key k)
  (:method-combination mca3)
  (:method (x &key k) (list x k)))

(deftest method-combination-arguments.12
    (multiple-value-bind (r c)
	(ignore-errors (mca3.0 1) *result*)
      (values r (null c)))
  (mca3.0 (1) 1 nil) t)

(deftest method-combination-arguments.13
    (multiple-value-bind (r c)
	(ignore-errors (mca3.0 1 :k 2) *result*)
      (values r (null c)))
  (mca3.0 (1 :k 2) 1 2) t)

;; methods.lisp
;;; Old PCL has a bug wrt rebinding a parameter around
;;; CALL-NEXT-METHOD.

(deftest methods.0
    (progn
      (defclass mt0 ()
	())
      (defmethod mt0 ((m mt0) x)
	x)
      (defmethod mt0 :around ((m mt0) x)
	(let ((x (1+ x)))
	  #+cmu (declare (optimize (ext:inhibit-warnings 3)))
	  (call-next-method)))
      (mt0 (make-instance 'mt0) 42))
  42)

;; pv.lisp
;;;**************************
;;; With Optimization  ******
;;; *************************

#+gerds-pcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq pcl::*optimize-gf-calls-p* t))

(defclass pv0 ()
  ((a :accessor pv0-a :initform 0)))

(defmethod pv0.0 ((x pv0))
  1)

(defmethod pv0.1 ((x pv0) &rest r)
  (car r))

(defmethod pv0.2 ((x pv0) &key k)
  k)

(defmethod pv0.3 ((x pv0) &optional o)
  o)

(defmethod pv0.4 ((x pv0) (y pv0))
  1)

(defmethod call-pv0 ((x pv0))
  (list (pv0.0 x)
	(pv0.1 x 2)
	(pv0.2 x :k 3) (pv0.2 x)
	(pv0.3 x 1) (pv0.3 x)
	(pv0.4 x x)))

(deftest pv-gf-call-optimized.0
    (ignore-errors (call-pv0 (make-instance 'pv0)))
  (1 2 3 nil 1 nil 1))

(defclass pv0.1 (pv0) ())

(defmethod pv0.0 ((x pv0.1))
  (call-next-method))

(defmethod pv0.1 ((x pv0.1) &rest r)
  (declare (ignorable r))
  (call-next-method))

(defmethod pv0.2 ((x pv0.1) &key k)
  (declare (ignorable k))
  (call-next-method))

(defmethod pv0.3 ((x pv0.1) &optional o)
  (declare (ignorable o))
  (call-next-method))

(defmethod pv0.4 ((x pv0.1) (y pv0.1))
  (call-next-method))

(defmethod call-pv0 ((x pv0.1))
  (call-next-method))

(deftest pv-gf-call-optimized.1
    (ignore-errors (call-pv0 (make-instance 'pv0.1)))
  (1 2 3 nil 1 nil 1))

(deftest pv-gf-call-optimized.2
    (ignore-errors (call-pv0 (make-instance 'pv0)))
  (1 2 3 nil 1 nil 1))


;;;*****************************
;;; Without Optimization  ******
;;; ****************************

#+gerds-pcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq pcl::*optimize-gf-calls-p* nil))

(defclass pv1 ()
  ((a :accessor pv1-a :initform 0)))

(defmethod pv1.0 ((x pv1))
  1)

(defmethod pv1.1 ((x pv1) &rest r)
  (car r))

(defmethod pv1.2 ((x pv1) &key k)
  k)

(defmethod pv1.3 ((x pv1) &optional o)
  o)

(defmethod call-pv1 ((x pv1))
  (list (pv1.0 x)
	(pv1.1 x 2)
	(pv1.2 x :k 3) (pv1.2 x)
	(pv1.3 x 1) (pv1.3 x)))

(deftest pv-gf-call.1
    (call-pv1 (make-instance 'pv1))
  (1 2 3 nil 1 nil))


;; reinitialize-instance.lisp
(deftest reinitialize-instance.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri0 () ((a :initarg :a)))
	  (reinitialize-instance (make-instance 'ri0) :a 1))
      (values (null r) (typep c 'error)))
  nil nil)

(deftest reinitialize-instance.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri1 () ())
	  (reinitialize-instance (make-instance 'ri1) :a 1))
      (values (null r) (typep c 'error)))
  t t)

(deftest reinitialize-instance.2
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri2 () ())
	  (defmethod shared-initialize ((x ri2) slots &rest initargs &key a)
	    (declare (ignore slots initargs a)))
	  (reinitialize-instance (make-instance 'ri2) :a 1))
      (values (null r) (typep c 'error)))
  nil nil)

(deftest reinitialize-instance.3
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri3 () ())
	  (defmethod reinitialize-instance :after ((x ri3) &rest initargs
						   &key a)
	    (declare (ignore initargs a)))
	  (reinitialize-instance (make-instance 'ri3) :a 1))
      (values (null r) (typep c 'error)))
  nil nil)

(deftest reinitialize-instance.4
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri4 () ())
	  (defmethod reinitialize-instance :after ((x ri4) &rest initargs
						   &key a &allow-other-keys)
	    (declare (ignore initargs a)))
	  (reinitialize-instance (make-instance 'ri4) :a 1 :b 2))
      (values (null r) (typep c 'error)))
  nil nil)

(deftest reinitialize-instance.5
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri5 () ())
	  (reinitialize-instance (make-instance 'ri4)
				 :a 1 :b 2 :allow-other-keys t))
      (values (null r) (typep c 'error)))
  nil nil)

;; slot-accessors.lisp
(defclass sa0 ()
  ((a :accessor a-of :initarg :a)))

(deftest slot-accessor.0
    (let ((instance (make-instance 'sa0 :a 0)))
      (a-of instance))
  0)

(deftest slot-accessor.1
    (let ((instance (make-instance 'sa0)))
      (setf (a-of instance) 1)
      (a-of instance))
  1)

(defmethod sa0.0 ((x sa0))
  (a-of x))

(deftest slot-accessor.2
    (let ((instance (make-instance 'sa0)))
      (setf (a-of instance) 2)
      (sa0.0 instance))
  2)

;;; Redefining the class should update the PV table cache of
;;; method SA0.0 so that is reads the right slot.

(deftest slot-accessor.3
    (progn
      (defclass sa0 ()
	((c :accessor c-of)
	 (a :accessor a-of :initarg :a)
	 (b :accessor b-of)))
      (sa0.0 (make-instance 'sa0 :a 42)))
  42)

(defclass sa1 (sa0)
  ((b :accessor a-of :initarg :b)))
      
(deftest slot-accessor.4
    (let ((instance (make-instance 'sa1 :b 0)))
      (sa0.0 instance))
  0)

(defclass sa2 (sa0)
  ())

(defmethod (setf a-of) (new-value (obj sa2))
  (setf (slot-value obj 'a) (* 2 new-value)))

(defmethod sa2.0 ((obj sa2))
  (setf (a-of obj) 42))

(deftest slot-accessor.5
    (let ((instance (make-instance 'sa2)))
      (sa2.0 instance))
  84)

(defclass sa3 ()
  ())

(defmethod (setf foo-of) (n (obj sa3))
  n)

(defmethod sa3.0 ((obj sa3))
  (setf (foo-of obj) 11))

(deftest slot-accessor.6
    (let ((instance (make-instance 'sa3)))
      (sa3.0 instance))
  11)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sa4 ()
    ((a :initform 0 :accessor sa4-a))))

(defmethod sa4.0 ((x sa4))
  (sa4-a x))

(deftest slot-accessor.7
    (sa4.0 (make-instance 'sa4))
  0)

(deftest slot-accessor.8
    (progn
      (defun sa4-a (x)
	(declare (ignore x))
	11)
      (prog1
	  (sa4.0 (make-instance 'sa4))
	(fmakunbound 'sa4-a)))
  11)

;; slot-boundp.lisp
(defclass sbp0 ()
  ((a :initarg :a :initform 0)
   (b :initarg :b)
   (c :allocation :class)))

(defmethod sbp0.0 ((x sbp0) slot)
  (null (slot-boundp x slot)))

(deftest slot-boundp.0
    (null (slot-boundp (make-instance 'sbp0) 'a))
  nil)

(define-compiled-test slot-boundp.1
    (null (slot-boundp (make-instance 'sbp0) 'a))
  nil)

(deftest slot-boundp.2
    (null (slot-boundp (make-instance 'sbp0) 'b))
  t)

(define-compiled-test slot-boundp.3
    (multiple-value-bind (r c)
	(ignore-errors (slot-boundp (make-instance 'sbp0) 'b))
      (values (null r) c))
  t nil)

(deftest slot-boundp.4
    (null (slot-boundp (make-instance 'sbp0) 'c))
  t)

(define-compiled-test slot-boundp.5
    (null (slot-boundp (make-instance 'sbp0) 'c))
  t)

(deftest slot-boundp.6
    (sbp0.0 (make-instance 'sbp0) 'b)
  t)

(deftest slot-boundp.7
    (sbp0.0 (make-instance 'sbp0 :a 2) 'a)
  nil)

;; slot-missing.lisp
;;; in method (pv table optimization)
;;; in compiled defun
;;; uncompiled.

(defmacro define-sm-test (name (instance class) access &rest values)
  (let* ((*print-case* :upcase)
	 (*print-pretty* nil)
	 (*print-gensym* t)
	 (method-name (intern (format nil "~S.METHOD" name)))
	 (method-test (intern (format nil "~S.METHOD-TEST" name)))
	 (compiled-test (intern (format nil "~S.COMPILED" name))))
    `(progn
       (defmethod ,method-name ((,instance ,class))
	 ,access)
       (deftest ,name
	   (multiple-value-bind (r c)
	       (let ((,instance (make-instance ',class)))
		 (ignore-errors ,access))
	     (values r (typep c 'condition)))
	 ,@values)
       (deftest ,method-test
	   (multiple-value-bind (r c)
	       (let ((,instance (make-instance ',class)))
		 (ignore-errors (,method-name ,instance)))
	     (values r (typep c 'condition)))
	 ,@values)
       (define-compiled-test ,compiled-test
	   (multiple-value-bind (r c)
	       (let ((,instance (make-instance ',class)))
		 (ignore-errors ,access))
	     (values r (typep c 'condition)))
	 ,@values))))

(defclass sm0 () ())

(define-sm-test slot-missing.0 (instance sm0)
  (slot-value instance 'a)
  nil t)

(define-sm-test slot-missing.1 (instance sm0)
  (setf (slot-value instance 'a) 1)
  nil t)

(define-sm-test slot-missing.2 (instance sm0)
  (slot-boundp instance 'a)
  nil t)

(defclass sm1 () ())

(defvar *sm-result* nil)

(defmethod slot-missing (class (obj sm1) slot-name operation
			 &optional new-value)
  (setq *sm-result* (list slot-name operation new-value)))

(define-sm-test slot-missing.3 (instance sm1)
  (progn
    (slot-value instance 'a)
    *sm-result*)
  (a slot-value nil) nil)

(define-sm-test slot-missing.4 (instance sm1)
  (progn
    (setf (slot-value instance 'a) 1)
    *sm-result*)
  (a setf 1) nil)

(define-sm-test slot-missing.5 (instance sm1)
  (progn
    (slot-boundp instance 'a)
    *sm-result*)
  (a slot-boundp nil) nil)

;; slot-type.lisp
#+gerds-pcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq pcl::*use-slot-types-p* t))

;;; Check that we check slot types, at least sometimes.

(defclass stype ()
  ((a :type fixnum :initform 0 :initarg :a)))

(defmethod stype.0 ((obj stype))
  (slot-value obj 'a))

(defmethod stype.1 ((obj stype) value)
  (setf (slot-value obj 'a) value))

(deftest slot-type.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (stype.0 (make-instance 'stype :a 1)))
      (values r (null c)))
  1 t)

(deftest slot-type.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (stype.0 (make-instance 'stype :a 1.0)))
      (values r (typep c 'error)))
  nil t)

(deftest slot-type.2
    (multiple-value-bind (r c)
	(ignore-errors
	  (stype.1 (make-instance 'stype) 1))
      (values r (typep c 'error)))
  1 nil)

(deftest slot-type.3
    (multiple-value-bind (r c)
	(ignore-errors
	  (stype.1 (make-instance 'stype) 1.0))
      (values r (typep c 'error)))
  nil t)

(deftest slot-type.4
    (multiple-value-bind (r c)
	(ignore-errors
	  (setf (slot-value (make-instance 'stype) 'a) "string"))
      (values r (typep c 'error)))
  nil t)

;; slot-value.lisp
(defclass sv0 ()
  ((a :allocation :class :initarg :a :initform 0)))

(defun sv0.0 ()
  (let* ((x (random 10))
	 (obj (make-instance 'sv0 :a x)))
    (eql x (slot-value obj (identity 'a)))))

;;; In previous versions of PCL (18d for example), the above
;;; slot-value fails when the class is redefined.
	      
(deftest slot-value.0
    (sv0.0)
  t)

(deftest slot-value.1
    (progn
      (defclass sv0 ()
	((a :allocation :class :initarg :a :initform 0)))
      t)
  t)

(deftest slot-value.2
    (sv0.0)
  t)
      
