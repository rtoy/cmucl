;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/config.lisp,v 1.3 1993/06/08 13:04:40 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Utility to load subsystems and save a new core.
;;;
(in-package "USER")


(block abort
  (let ((output-file #p"library:lisp.core")
	(load-clx t)
	(load-hemlock t)
	(other ()))
    (loop
      (fresh-line)
      (format t " 1: specify result file (currently ~S)~%"
	      (namestring output-file))
      (format t " 2: toggle loading of the CLX X library, currently ~
		 ~:[dis~;en~]abled.~%"
	      load-clx)
      (format t " 3: toggle loading the Hemlock editor, currently ~
		 ~:[dis~;en~]abled.~
		 ~:[~%    (would force loading of CLX.)~;~]~%"
	      load-hemlock load-clx)
      (format t " 4: specify some site-specific file to load.~@
		 ~@[    Current files:~%~{      ~S~%~}~]"
	      (mapcar #'namestring other))
      (format t " 5: configure according to current options.~%")
      (format t " 6: abort the configuration process.~%")
      (format t "~%Option number: ")
      (force-output)
      (flet ((file-prompt (prompt)
	       (format t prompt)
	       (force-output)
	       (pathname (string-trim " 	" (read-line)))))
	(let ((res (ignore-errors (read-from-string (read-line)))))
	  (case res
	    (1
	     (setq output-file (file-prompt "Result core file name: ")))
	    (2
	     (unless (setq load-clx (not load-clx))
	       (setq load-hemlock nil)))
	    (3
	     (when (setq load-hemlock (not load-hemlock))
	       (setq load-clx t)))
	    (4
	     (setq other
		   (append other
			   (list (file-prompt "File(s) to load ~
					       (can have wildcards): ")))))
	    (5 (return))
	    (6
	     (format t "~%Aborted.~%")
	     (return-from abort))))))
    
    (gc-off)
    (when load-clx
      (setf *features* (delete :no-clx *features* :test #'eq))
      (load "library:subsystems/clx-library"))
    (when load-hemlock
      (setf *features* (delete :no-hemlock *features* :test #'eq))
      (load "library:subsystems/hemlock-library"))
    (dolist (f other) (load f))
    
    (setq *info-environment*
	  (list* (make-info-environment :name "Working")
		 (compact-info-environment (first *info-environment*)
					   :name "Auxiliary")
		 (rest *info-environment*)))
    
    (when (probe-file output-file)
      (multiple-value-bind
	  (ignore old new)
	  (rename-file output-file
		       (concatenate 'string (namestring output-file)
				    ".BAK"))
	(declare (ignore ignore))
	(format t "~&Saved ~S as ~S.~%" (namestring old) (namestring new))))
    
    ;;
    ;; Enable the garbage collector.  But first fake it into thinking that
    ;; we don't need to garbage collect.  The save-lisp is going to call
    ;; purify so any garbage will be collected then.
    (setf lisp::*need-to-collect-garbage* nil)
    (gc-on)
    ;;
    ;; Save the lisp.
    (save-lisp output-file)))

(quit)
