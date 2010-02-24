;; Bootstrap new external-format structure.
;;
;; Just needed so we don't have to answer a cerror with clobber-it
;; when compiling pcl.

(in-package "STREAM")

(ext:without-package-locks
 (handler-bind ((error (lambda (c)
			 (declare (ignore c))
			 (invoke-restart 'kernel::clobber-it))))
   (defstruct (external-format
		(:conc-name ef-)
		(:print-function %print-external-format)
		(:constructor make-external-format (name composingp
							 &optional slots slotd
							 octets-to-code
							 code-to-octets)))
     (name (ext:required-argument) :type (or keyword cons) :read-only t)
     (composingp (ext:required-argument) :type boolean :read-only t)
     (slots #() :type simple-vector :read-only t)
     (slotd nil :type list :read-only t)
     (octets-to-code #'%efni :type function :read-only t)
     (code-to-octets #'%efni :type function :read-only t)
     (cache (make-array +ef-max+ :initial-element nil)))
   ))