
;;; This file may be needed to get from before 1.3.7 to current as
;;; of 18-Feb-97. Good luck!

;;; These preceed 1.3.7
#+x86(proclaim '(notinline kernel:%tan kernel:%atan kernel:%atan2))
#+x86(in-package :x86)
#+x86
(eval-when (compile load eval)
(defconstant conditions
  '((:o . 0)
    (:no . 1)
    (:b . 2) (:nae . 2) (:c . 2)
    (:nb . 3) (:ae . 3) (:nc . 3)
    (:eq . 4) (:e . 4) (:z . 4)
    (:ne . 5) (:nz . 5)
    (:be . 6) (:na . 6)
    (:nbe . 7) (:a . 7)
    (:s . 8)
    (:ns . 9)
    (:p . 10) (:pe . 10)
    (:np . 11) (:po . 11)
    (:l . 12) (:nge . 12)
    (:nl . 13) (:ge . 13)
    (:le . 14) (:ng . 14)
    (:nle . 15) (:g . 15))))

;;; Yaybe move some symbols to the C or X86 packages. This must
;;; be done BEFORE exports is loaded to prevent package conflicts
#+x86
(macrolet ((zap (str)
	     `(let ((sym (find-symbol ,str :lisp)))
		(when sym
		  (unintern sym (symbol-package sym))
		  (import sym :x86)))))
  ;;; Moved all these to the x86 package.
  (zap "*ALLOCATION-POINTER*")
  (zap "*BINDING-STACK-POINTER*")
  (zap "*X86-CGC-ACTIVE-P*")
  (zap "*INTERNAL-GC-TRIGGER*")
  (zap "*STATIC-BLUE-BAG*"))

#+x86
(macrolet ((zap (str)
	     `(let ((sym (find-symbol ,str :x86)))
		(when sym
		  (unintern sym (symbol-package sym))
		  (import sym :c)
		  (export sym :c)))))
  (zap "ALLOCATE-DYNAMIC-CODE-OBJECT")
  (zap "ALLOC-ALIEN-STACK-SPACE")
  (zap "DEALLOC-ALIEN-STACK-SPACE"))

#+x86
(eval-when (compile load eval)
  (let ((ht (c::backend-template-names c:*backend*)))
    (unless (gethash 'c::allocate-dynamic-code-object ht)
      (setf (gethash 'c::allocate-dynamic-code-object ht)
	    (gethash 'vm::allocate-dynamic-code-object ht)))))

;;; Ok, now lets pick up any changes in exports
(load "target:code/exports.lisp")
