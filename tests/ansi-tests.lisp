(defpackage :ansi-tests-tests
  (:use :cl :lisp-unit))

(in-package :ansi-tests-tests)

(define-test misc.645.eval-dead-nlx-blocks
    (:tag :issues)
  ;; The interpreter's COMPILE-FOR-EVAL deleted unreachable blocks in a
  ;; single forward pass over the DFO.  Since deleting a block can mark
  ;; blocks earlier in the DFO for deletion, DELETE-P blocks survived
  ;; into later phases, which must not examine them.
  ;;
  ;; Both of these must go through EVAL, not COMPILE, to exercise the
  ;; bug; COMPILE runs IR1-OPTIMIZE and was never affected.
  ;;
  ;; MISC.645: the dead block held the exit of the CMUCL-DEBUG-CATCH-TAG
  ;; escape function.  ENVIRONMENT-ANALYZE built an NLX entry stub for
  ;; it and INSERT-CLEANUP-CODE got a NIL successor, failing with
  ;; "NIL is not of type C::CBLOCK".
  (assert-eql 0
              (eval '((lambda (a)
                        (declare (notinline abs isqrt))
                        (declare (optimize (ext:inhibit-warnings 3)))
                        (declare (optimize (debug 3) (safety 1) (space 2)
                                           (compilation-speed 1) (speed 0)))
                        (progn
                          (tagbody
                             (prog2 a 0
                               (labels ((%f9 (&key &allow-other-keys) (go 3)))
                                 (%f9)))
                             (isqrt (abs (unwind-protect 0)))
                           3)
                          a))
                      0))))

(define-test misc.187.eval-dead-nlx-blocks
    (:tag :issues)
  ;; MISC.187: the dead block held a REF to the escape function of the
  ;; CATCH in an unreachable &optional default init form, and
  ;; ANNOTATE-COMPONENT-FOR-EVAL asserted no leaf is an :ESCAPE
  ;; function.
  (assert-eql -6321798384
              (apply (eval '#'(lambda (a b c)
                                (declare (notinline))
                                (declare (optimize (safety 3)))
                                (declare (optimize (speed 0)))
                                (declare (optimize (debug 0)))
                                (flet ((%f7 (&optional (f7-1 (catch 'ct7 0))
                                                       (f7-2 0))
                                         c))
                                  (let ((v8
                                         (flet ((%f14 (f14-1
                                                       &optional (f14-2 (%f7 b)))
                                                  0))
                                           0)))
                                    (%f7 b)))))
                     '(2374299 70496 -6321798384))))

