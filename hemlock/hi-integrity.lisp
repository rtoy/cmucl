;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Written by Skef Wholey
;;;
;;; Hack to check a buffer's integrity.
;;;
(in-package 'hemlock-internals)

(defun checkit (&optional (buffer (current-buffer)))
  "Returns NIL if the buffer's region is OK, or a losing line if it ain't.
  If a malformed mark is found in the mark list it is returned as the 
  second value."
  (do ((line (mark-line (buffer-start-mark buffer)) (line-next line))
       (previous nil line)
       (lines nil (cons line lines)))
      ((null line) nil)
    (unless (eq (line-%buffer line) buffer)
      (format t "~%Oh, Man!  It's in the wrong buffer!~%")
      (return line))
    (when (member line lines)
      (format t "~%Oh, Man!  It's circular!~%")
      (return line))
    (unless (eq previous (line-previous line))
      (format t "~%Oh, Man!  A back-pointer's screwed up!~%")
      (return line))
    (when (and previous (>= (line-number previous) (line-number line)))
      (format t "~%Oh, Man!  A line number is screwed up!~%")
      (return line))
    (let ((res
	   (do ((m (line-marks line) (cdr m)))
	       ((null m) nil)
	     (unless (<= 0 (mark-charpos (car m)) (line-length line))
	       (format t "~%Oh, Man!  A mark is pointing into hyperspace!~%")
	       (return (car m)))
	     (unless (memq (mark-%kind (car m))
			   '(:left-inserting :right-inserting))
	       (format t "~%Oh, Man!  A mark's type is bogus!.~%")
	       (return (car m)))
	     (unless (eq (mark-line (car m)) line)
	       (format t "~%Oh, Man!  A mark's line pointer is messed up!~%")
	       (return (car m))))))
      (when res
	(return (values line res))))))
