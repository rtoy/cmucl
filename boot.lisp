(load "target:code/exports.lisp")

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

;;; needed for current
#+x86
(eval-when (compile load eval)
  (let ((ht (c::backend-template-names c:*backend*)))
    (unless (gethash 'c::allocate-dynamic-code-object ht)
      (setf (gethash 'c::allocate-dynamic-code-object ht)
	    (gethash 'vm::allocate-dynamic-code-object ht)))))
#+x86
(let ((sym (find-symbol "ALLOCATE-DYNAMIC-CODE-OBJECT" :x86)))
  (when sym
    (unintern sym (symbol-package sym))
    (import sym :c)
    (export sym :c)))
#+x86
(progn
;;; Moved all these to the x86 package.
  (let ((sym (find-symbol "*ALLOCATION-POINTER*" :lisp)))
    (unintern sym (symbol-package sym))
    (import sym :x86))
  (let ((sym (find-symbol "*BINDING-STACK-POINTER*" :lisp)))
    (unintern sym (symbol-package sym))
    (import sym :x86))
  (let ((sym (find-symbol "*X86-CGC-ACTIVE-P*" :lisp)))
    (unintern sym (symbol-package sym))
    (import sym :x86))
  (let ((sym (find-symbol "*INTERNAL-GC-TRIGGER*" :lisp)))
    (unintern sym (symbol-package sym))
    (import sym :x86))
  (let ((sym (find-symbol "*STATIC-BLUE-BAG*" :lisp)))
    (unintern sym (symbol-package sym))
    (import sym :x86)))
