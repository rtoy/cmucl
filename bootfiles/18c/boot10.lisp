;; bootstrap changes in the way FASL-FILE-VERSION is checked
;;
;; Given that FASL files are incompatible between releases, the
;; FASL-FILE-NUMBER was set to #x18d on all platforms, so that users
;; loading old compiled lisp files into their core will see a useful
;; error message, rather than unexplained lossage.
;;
;; This check is made when loading a FASL file into core, in the
;; function FOP-CODE-FORMAT. We work around the check so that it is
;; possible to build from 18c, without requiring a cross-compile.
;;
;; Additionally, the FASL file format was changed slightly: the FASL
;; version number is written as uint32, rather than as a single octet.
;;
;; You will encounter a continuable error during the first worldload
;; in the function CHECK-VERSION, saying that the FASL file version
;; numbers are incompatible. In the debugger prompt, say
;;
;; 0] (setq cl::*skip-fasl-file-version-check* t)
;;
;; then select the CONTINUE restart. The rest of the build should
;; proceed without problems.


(in-package :cl)

(defparameter *skip-fasl-file-version-check* t)

;; override original definition from code/load.lisp
(define-fop (fop-code-format 57 :nope)
  (let ((implementation (read-arg 1))
	(version (read-arg 1)))
    (declare (ignore implementation version))
    (warn "FOP-CODE-FORMAT ignoring incompatible FASL file versions during rebuild")))

;; override original definition from compiler/generic/new-genesis.lisp 
(define-fop (cold-fop-code-format 57 :nope)
  (let ((implementation (read-arg 1))
	(version (read-arg 4)))
    (declare (ignore implementation version))
    (warn "COLD-FOP-CODE-FORMAT ignoring incompatible FASL file versions during rebuild")))


(in-package :c)

;; this is a DEFCONSTANT, so can't SETQ
(setf (symbol-value 'byte-fasl-file-version) #x18d)

(defun open-fasl-file (name where &optional byte-p)
  (declare (type pathname name))
  (let* ((stream (open name :direction :output
		       :if-exists :new-version
		       :element-type '(unsigned-byte 8)))
	 (res (make-fasl-file :stream stream)))
    (multiple-value-bind
	(version f-vers f-imp)
	(if byte-p
	    (values "Byte code"
		    byte-fasl-file-version
		    (backend-byte-fasl-file-implementation *backend*))
	    (values (backend-version *backend*)
		    (backend-fasl-file-version *backend*)
		    (backend-fasl-file-implementation *backend*)))
      (format stream
	      "FASL FILE output from ~A.~@
	       Compiled ~A on ~A~@
	       Compiler ~A, Lisp ~A~@
	       Targeted for ~A, FASL version ~D~%"
	      where
	      (ext:format-universal-time nil (get-universal-time))
	      (machine-instance) compiler-version
	      (lisp-implementation-version)
	      version f-vers)
      ;;
      ;; Terminate header.
      (dump-byte 255 res)
      ;;
      ;; Specify code format
      (dump-fop 'lisp::fop-code-format res)

      (dump-byte f-imp res)
      ;; !! following  line is changed
      (dump-unsigned-32 f-vers res))
    res))
