(defpackage :ansi-tests-tests
  (:use :cl :lisp-unit))

(in-package :ansi-tests-tests)

(define-test misc.645.eval-dead-nlx-blocks
    (:tag :issues)
  ;; The interpreter's COMPILE-FOR-EVAL deleted unreachable blocks in a
  ;; single forward pass over the DFO.  Since deleting a block can mark
  ;; earlier blocks for deletion, DELETE-P blocks survived into
  ;; ENVIRONMENT-ANALYZE, which then tried to build an NLX entry stub for
  ;; an exit in one of them and failed with "NIL is not of type CBLOCK".
  ;; Note this must go through EVAL, not COMPILE, to exercise the bug.
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
