;;; -*- Package: User -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/Attic/snapshot-update.lisp,v 1.1 1992/01/07 17:24:11 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    A hack to generate a log of the changes since a particular snapshot.  We
;;; generate a shell script and run it to avoid many calls to run-program.
;;;
(in-package "USER")

(defun snapshot-updates (&key (snapshot-file "RCSSNAP")
			      (output
			       (make-pathname
				:defaults (pathname-name snapshot-file)
				:type "updates")))
  "Write to :OUTPUT a summary of the RCS change long entries since the
   since the specified :SNAPSHOT was made.  :OUTPUT is passed through to
   run-program, and should be a stream or pathname."
  (with-open-file (script "/tmp/update-script" :direction :output)
    (with-open-file (in snapshot-file :direction :input)
      (loop
	(let ((line (read-line in nil nil)))
	  (unless line (return))
	  (let* ((tab (position #\tab line))
		 (dot (position #\. line :from-end t)))
	    (format script "rlog -r~A~D- ~A~%"
		    (subseq line (1+ tab) (1+ dot))
		    (1+ (parse-integer (subseq line (1+ dot))))
		    (subseq line 0 tab)))))))

  (let ((cmd (format nil
		     "cd ~A; csh /tmp/update-script | ~
		      sed -n -e '/^RCS file:/p' -e '/^------/,/^======/p' | ~
		      sed -e '/^RCS file:/{;:again\\~@
		          N;s/^RCS file.*\nRCS file/RCS file/;t again\\~@
		          }'"
		     (directory-namestring (truename snapshot-file)))))
    (run-program "csh" (list "-c" cmd) :output output
		 :if-output-exists :supersede)))
