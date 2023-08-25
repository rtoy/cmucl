(in-package #:sacla-lisp-unit)
(define-test sacla-must-eval.1 (:tag :sacla)
 (assert-true (= (funcall (lambda (x) (+ x 3)) 4) 7)))
(define-test sacla-must-eval.2 (:tag :sacla)
 (assert-true (= (funcall (lambda (&rest args) (apply #'+ args)) 1 2 3 4) 10)))
(define-test sacla-must-eval.3 (:tag :sacla)
 (assert-true (functionp (lambda (&rest args) (apply #'+ args)))))
(define-test sacla-must-eval.4 (:tag :sacla)
 (assert-true (functionp (macro-function 'lambda))))
(define-test sacla-must-eval.5 (:tag :sacla)
 (assert-true
  (every #'special-operator-p
         '(block catch
            eval-when
            flet
            function
            go
            if
            labels
            let
            let*
            load-time-value
            locally
            macrolet
            multiple-value-call
            multiple-value-prog1
            progn
            progv
            quote
            return-from
            setq
            symbol-macrolet
            tagbody
            the
            throw
            unwind-protect))))
(define-test sacla-must-eval.6 (:tag :sacla)
 (assert-true (not (special-operator-p 'car))))
(define-test sacla-must-eval.7 (:tag :sacla)
 (assert-true (not (special-operator-p 'cdr))))
(define-test sacla-must-eval.8 (:tag :sacla)
 (assert-true (not (special-operator-p 'cond))))
(define-test sacla-must-eval.9 (:tag :sacla)
 (assert-true (not (special-operator-p 'values))))

