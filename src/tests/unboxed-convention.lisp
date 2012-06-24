;; Tests for typed calling convention.

(eval-when (:compile-toplevel)
  (setq c::*check-consistency* t))

(in-package :cl-user)

(defun fid (x) 
  (declare (double-float x) 
	   (c::calling-convention :typed)
	   )
  x)

(defun test-fid-1 ()
  (assert (= (fid 1d0) 1d0)))

(defun f+ (x y)
  (declare (double-float x y)
	   (c::calling-convention :typed))
  (+ x y))

(defun sum-prod (x y z u v w)
  (declare (double-float x y z u v w)
	   (c::calling-convention :typed))
  (values (+ x y z u v w)
	  (* x y z u v w)))

(defun test-sum-prod-1 ()
  (multiple-value-bind (sum prod) (sum-prod 2d0 3d0 4d0 5d0 6d0 7d0)
    (assert (= sum (+ 2d0 3d0 4d0 5d0 6d0 7d0)))
    (assert (= prod (* 2d0 3d0 4d0 5d0 6d0 7d0)))))

(defun test-sum-prod-2 ()
  (multiple-value-bind (sum) (sum-prod 2d0 3d0 4d0 5d0 6d0 7d0)
    (assert (= sum (+ 2d0 3d0 4d0 5d0 6d0 7d0)))))

(defun test-sum-prod-3-aux (x y z u v w)
  (sum-prod x y z u v w))

(defun test-sum-prod-3 ()
  (multiple-value-bind (sum prod) (test-sum-prod-3-aux 2d0 3d0 4d0 5d0 6d0 7d0)
    (assert (= sum (+ 2d0 3d0 4d0 5d0 6d0 7d0)))
    (assert (= prod (* 2d0 3d0 4d0 5d0 6d0 7d0)))))

(defun id (x)
  (declare (c::calling-convention :typed))
  x)

(defun test-id-1 ()
  (assert (eql (id 1) 1)))

(defun test-id-2 ()
  (assert (eql (id 1d0) 1d0)))

(defun test-id-3 ()
  (assert (equal (multiple-value-list (id 1d0)) '(1d0))))

;; This one has both boxed and unboxed arguments.
(defun cons-sum (o1 f1 o2 f2)
  (declare (double-float f1 f2)
	   (c::calling-convention :typed))
  (values (cons o1 o2) (+ f1 f2)))

(defun test-cons-sum-1 ()
  (multiple-value-bind (cons sum) (cons-sum 1 2d0 3 4d0)
    (assert (equal cons '(1 . 3)))
    (assert (= sum (+ 2d0 4d0)))))

(defun ffib (x)
  (declare (double-float x)
	   (c::calling-convention :typed))
  (the double-float
    (cond ((= x 0) 0d0)
	  ((= x 1) 1d0)
	  (t (+ (ffib (- x 1))
		(ffib (- x 2)))))))

;; (time (ffib 30d0))

(defun test-ffib-1 ()
  (assert (= (ffib 0d0) 0))
  (assert (= (ffib 1d0) 1))
  (assert (= (ffib 2d0) 1))
  (assert (= (ffib 3d0) 2))
  (assert (= (ffib 4d0) 3))
  (assert (= (ffib 5d0) 5))
  (assert (= (ffib 6d0) 8))
  (assert (= (ffib 7d0) 13))
  (assert (= (ffib 8d0) 21)))

;; (test-ffib-1)
  

;; SUM will be redefined with different types to exercise the linker a
;; bit.
(defun sum (f1 f2)
  (declare (double-float f1 f2)
	   (c::calling-convention :typed))
  (+ f1 f2))

(defun test-sum-1 ()
  (assert (= (sum 2d0 3d0) 5d0)))

(defun sum (f1 f2)
  (declare (c::calling-convention :typed))
  (+ f1 f2))

(defun test-sum-2 ()
  (assert (= (sum 2d0 3d0) 5d0)))

(defun test-sum-3 ()
  (handler-case (progn (sum 2 3)
		       (assert nil))
    (type-error (c) 
      (assert (equal (type-error-datum c) 3))
      (assert (eq (type-error-expected-type c) 'double-float)))))

(defun sum (f1 f2)
  (declare (double-float f2)
	   (c::calling-convention :typed))
  (the double-float
    (+ f1 f2)))

(defun test-sum-4 ()
  (assert (= (sum 2d0 3d0) 5d0)))

(defun test-sum-5 ()
  (assert (= (sum 2 3d0) 5d0)))

(defun test-sum-6 ()
  (handler-case (progn 
		  (sum #c(0 1) 3d0) 
		  (assert nil))
    (type-error (c)
      (assert (equal (type-error-datum c) #c(3d0 1d0)))
      (assert (eq (type-error-expected-type c) 'double-float)))))

(defun sum (f1 f2)
  (declare (double-float f2))
  (the double-float
    (+ f1 f2)))

(defun test-sum-7 ()
  (assert (= (sum 2 3d0) 5d0)))

;; (ext:info function kernel::linkage 'sum)

(defun wild (f x y)
  (declare (type function f)
	   (double-float x y)
	   (c::calling-convention :typed))
  (funcall f x y))

(defun test-wild-1 ()
  (assert (= (wild #'+ 3d0 5d0) 8d0)))

(defun test-wild-2 ()
  (assert (equal (multiple-value-list (wild #'values 3d0 5d0))
		 '(3d0 5d0))))


(defun opt-result (x y)
  (declare (double-float x y)
	   (c::calling-convention :typed))
  (if (zerop x)
      y
      (values x y)))

(defun test-opt-result-1 ()
  (assert (= (opt-result 0d0 3d0) 3d0)))

(defun test-opt-result-2 ()
  (assert (= (opt-result 1d0 3d0) 1d0)))

(defun test-opt-result-3 ()
  (assert (equal (multiple-value-list (opt-result 1d0 3d0))
		 '(1d0 3d0))))

(defun test-opt-result-3 ()
  (assert (equal (multiple-value-list (opt-result 0d0 3d0))
		 '(3d0))))

;;(defun opt-arg (x &optional (y 0d0))
;;  (declare (double-float x y)
;;	   (c::calling-convention :typed))
;;  (+ x y))

(declaim (inline inlined-fun))
(defun inlined-fun (obj)
  (declare (c::calling-convention :typed))
  obj)

(defun test-inlined-fun-1 ()
  (assert (eq (inlined-fun 'x) 'x)))

(defun unused-arg-fun (x)
  (declare (ignore x))
  (declare (c::calling-convention :typed))
  nil)

(defun test-unused-arg-fun-1 ()
  (assert (eq (unused-arg-fun 'x) nil)))

(let ((state 0))
  (defun closure ()
    (declare (c::calling-convention :typed))
    (mod (incf state) 2)))

(defun test-closure-1 ()
  (assert (member (closure) '(0 1)))
  (assert (member (closure) '(0 1))))

(defun self-ref ()
  (declare (c::calling-convention :typed))
  #'self-ref)

(defun test-self-ref-1 ()
  (assert (eq #'self-ref (funcall (self-ref)))))

(defun many-args (a b c d e f g h i j k l m n o p)
  (declare (c::calling-convention :typed))
  (list a b c d e f g h i j k l m n o p))

(defun test-many-args-1 ()
  (assert (equal (many-args 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p)
		 '(a b c d e f g h i j k l m n o p))))

;; (compile-file "/tmp/x.lisp" :trace-file "/tmp/x.trace" :progress t)


(defun many-results (a b c d e f g h i j k l m n o p)
  (declare (c::calling-convention :typed))
  (values m n o p a b c d e f g h i j k l))

(defun test-many-results-1 ()
  (assert (equal (multiple-value-list
		  (many-results 
		   'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p))
		 '(m n o p a b c d e f g h i j k l))))
  
#+(or)
(defun pcl::pcl-funcallable-instance-slots (object)
  (declare ;;(type pcl::pcl-funcallable-instance object)
	   (c::calling-convention :typed))
  (kernel:%funcallable-instance-info object 0)) 

;; (c::clear-info function c::calling-convention 'pcl::pcl-funcallable-instance-slots)

;; (c::info function calling-convention 'pcl::pcl-funcallable-instance-slots)

(defun 6args (a b c d e f g)
  (declare (c::calling-convention :typed))
  (list a b c d e f g))

(defun set-arg ()
  (let (a)
    (setq a nil)
    (6args nil nil nil a a a nil)))

(defun 2values ()
  (declare (c::calling-convention :typed))
  (values 1 2))

(defun call-1-or-2-values (x)
  (declare (c::calling-convention :typed))
  (or x
      (2values)))


(defun test-call-1-or-2-values-1 ()
  (assert (equal (multiple-value-list (call-1-or-2-values 1))
		 '(1))))

(defun test-call-1-or-2-values-2 ()
  (assert (equal (multiple-value-list (call-1-or-2-values nil))
		 '(1 2))))

(defun deleted-fun (x)
  (labels ((d ()
	     (declare (c::calling-convention :typed))))
    #'d
    x))

(defun gf-fun (x)
  (declare (c::calling-convention :typed))
  x)

;;(defun call-gf-fun (x)
;;  (gf-fun x))
;;
;;(defgeneric gf-fun (x))
;;(defmethod gf-fun (x)
;;  x)

  
#+(or)
(defun foo ()
  (labels ((sum (x y) (+ x y)))
    (declare (ftype (function (double-float double-float) double-float) sum))
    (list (sum 2d0 4d0)
	  (sum 2 4))))

(defun tests ()
  (test-fid-1)
  (test-sum-prod-1)
  (test-sum-prod-2)
  (test-sum-prod-3)
  (test-id-1)
  (test-id-2)
  (test-id-3)
  (test-cons-sum-1)
  (test-ffib-1)
  (test-sum-1)
  (test-sum-2)
  (test-sum-3)
  (test-sum-4)
  (test-sum-5)
  (test-sum-6)
  (test-sum-7)
  (test-wild-1)
  (test-wild-2)
  (test-opt-result-1)
  (test-opt-result-2)
  (test-opt-result-3)
  (test-inlined-fun-1)
  (test-unused-arg-fun-1)
  (test-closure-1)
  (test-self-ref-1)
  (test-many-args-1)
  (test-many-results-1)
  (test-call-1-or-2-values-1)
  (test-call-1-or-2-values-2)
  )

;; (tests)
