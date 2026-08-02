;;; Tests for copy propagation.

(defpackage :copyprop-tests
  (:use :cl :lisp-unit))

(in-package "COPYPROP-TESTS")

;;; Copy propagation deletes a MOVE when every reference to its destination
;;; can instead read the source, which requires the source to be unchanged
;;; between the move and the reference.  INIT-COPY-SETS never looked for such
;;; changes at a MOVE whose own destination was an eligible copy TN, on the
;;; assumption that a TN written once cannot invalidate anything.  That is
;;; false for a variable's home TN: it already holds the incoming argument
;;; before a SETQ moves a new value into it, so a copy taken from it earlier
;;; is invalidated by that SETQ even though the SETQ is its only MOVE.
;;;
;;; In COPYPROP.SETQ-ACROSS-SPECIAL-BIND, the move of A into G was recorded as
;;; a copy, the SETQ of A was not recorded as invalidating it, and the
;;; reference to G was rewritten to read A directly, so LOGNOT saw 522 rather
;;; than the argument.  All three tests here are that same shape.
;;;
;;; PROPAGATE-COPIES always looked for invalidations, so the wrong code only
;;; appeared when the reference was in a different block from the copy and the
;;; SETQ -- here separated by the cleanup for the special binding.  DEBUG 3 is
;;; also needed, since it keeps the XEP and the entry lambda from being
;;; merged, which is what leaves A's home TN with a single MOVE.  Copy
;;; propagation itself only runs when SPEED >= COMPILATION-SPEED.

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
