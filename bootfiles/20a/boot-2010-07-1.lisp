;; Simple bootstrap file to add the documentation slot to the EFX and
;; EXTERNAL-FORMAT structure.

(in-package "STREAM")

(ext:without-package-locks
  (handler-bind ((error (lambda (c)
			  (declare (ignore c))
			  (invoke-restart 'kernel::clobber-it))))
    #+nil
    (defstruct efx
      (octets-to-code #'%efni :type function :read-only t)
      (code-to-octets #'%efni :type function :read-only t)
      (flush-state nil :type (or null function) :read-only t)
      (copy-state nil :type (or null function) :read-only t)
      (cache nil :type (or null simple-vector))
      (min 1 :type kernel:index :read-only t)
      (max 1 :type kernel:index :read-only t)
      (documentation nil :type (or null string) :read-only t)))

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