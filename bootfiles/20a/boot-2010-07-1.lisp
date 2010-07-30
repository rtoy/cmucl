;; Simple bootstrap file to add the documentation slot to the EFX and
;; EXTERNAL-FORMAT structure.

(in-package "STREAM")

(ext:without-package-locks

  (handler-bind ((error (lambda (c)
			  (declare (ignore c))
			  (invoke-restart 'kernel::clobber-it))))
    (defstruct (external-format
		 (:conc-name ef-)
		 (:print-function %print-external-format)
		 (:constructor make-external-format (name efx composingp documentation
							  &optional slots slotd)))
      (name (ext:required-argument) :type (or keyword cons) :read-only t)
      (efx (ext:required-argument) :type efx :read-only t)
      (composingp (ext:required-argument) :type boolean :read-only t)
      (slots #() :type simple-vector :read-only t)
      (slotd nil :type list :read-only t)
      (documentation nil :type (or null string) :read-only t))))

(in-package "INTL")

(export '(with-textdomain))

(ext:without-package-locks

(defmacro with-textdomain ((old-domain new-domain) &body body)
  `(progn
     (intl:textdomain ,new-domain)
     ,@body
     (intl:textdomain ,old-domain)))
)

;; Executable feature works on sparc!
#+(or sparc (and x86 darwin))
(pushnew :executable *features*)
