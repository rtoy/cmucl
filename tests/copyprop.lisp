;;; Tests for copy propagation.

(defpackage :copyprop-tests
  (:use :cl :lisp-unit))

(in-package "COPYPROP-TESTS")

;;; INIT-COPY-SETS used to skip its Kill scan for a MOVE whose result was
;;; itself an eligible copy TN, on the assumption that such a TN is written
;;; only once and so cannot invalidate anything.  That is wrong for a
;;; variable's home TN: it holds the incoming argument before the SETQ writes
;;; it, and a copy made from it beforehand is invalidated by that write.  The
;;; stale copy survived in Out and reached the use in a successor block, so
;;; the reference to G below read A's register after it had been clobbered.
;;;
;;; The special binding matters only because it forces the use of G into a
;;; block separate from the copy and the SETQ; PROPAGATE-COPIES kills
;;; correctly within a block, so the bug is only visible across one.
;;;
;;; Copy propagation runs when SPEED >= COMPILATION-SPEED, and DEBUG 3 keeps
;;; the XEP and the entry lambda from being merged, which is what leaves the
;;; home TN with a single VOP write.  Both are needed to reproduce.

(define-test copyprop.setq-across-special-bind
  (:tag :issues)
  (assert-eql
   -808
   (funcall
    (compile nil '(lambda (a)
		   (declare (type (integer 6 1273) a))
		   (declare (optimize (space 0) (safety 0) (debug 3)
				      (compilation-speed 2) (speed 3)))
		   (lognot (let ((*print-base* *print-base*))
			     (let ((g a))
			       (setq a 522)
			       g)))))
    807)))

(define-test copyprop.setq-across-restart-bind
  (:tag :issues)
  (assert-eql
   -808
   (funcall
    (compile nil '(lambda (a)
		   (declare (type (integer 6 1273) a))
		   (declare (optimize (space 0) (safety 0) (debug 3)
				      (compilation-speed 2) (speed 3)))
		   (lognot (restart-bind nil
			     (let ((g a))
			       (setq a 522)
			       g)))))
    807)))

;;; The original random-tester form (MISC.643), which reaches the same shape
;;; through the expansion of SHIFTF.

(define-test copyprop.misc.643
  (:tag :issues)
  (assert-eql
   -808
   (funcall
    (compile nil '(lambda (a)
		   (declare (type (integer 6 1273) a))
		   (declare (optimize (space 0) (safety 0) (debug 3)
				      (compilation-speed 2) (speed 3)))
		   (logorc2 0 (restart-bind nil (shiftf a 522)))))
    807)))
