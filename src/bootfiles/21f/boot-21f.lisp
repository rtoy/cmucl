(setf lisp::*enable-package-locked-errors* nil)

(in-package "C")
;; deftype information
(define-info-type type lambda-list list nil)
(define-info-type source-location deftype (or form-numbers null) nil)

(in-package "LISP")
(defmacro deftype (name arglist &body body)
  "Syntax like DEFMACRO, but defines a new type."
  (unless (symbolp name)
    (simple-program-error (intl:gettext "~S -- Type name not a symbol.") name))
  (and lisp::*enable-package-locked-errors*
       (symbol-package name)
       (ext:package-definition-lock (symbol-package name))
       (restart-case
           (error 'lisp::package-locked-error
                  :package (symbol-package name)
                  :format-control (intl:gettext "defining type ~A")
                  :format-arguments (list name))
         (continue ()
           :report (lambda (stream)
		     (write-string (intl:gettext "Ignore the lock and continue") stream)))
         (unlock-package ()
           :report (lambda (stream)
		     (write-string (intl:gettext "Disable package's definition-lock then continue") stream))
           (setf (ext:package-definition-lock (symbol-package name)) nil))
         (unlock-all ()
           :report (lambda (stream)
		     (write-string (intl:gettext "Unlock all packages, then continue") stream))
           (lisp::unlock-all-packages))))
  (let ((whole (gensym "WHOLE-")))
    (multiple-value-bind (body local-decs doc)
			 (parse-defmacro arglist whole body name 'deftype
					 :default-default ''*)
      (when doc
	(intl::note-translatable intl::*default-domain* doc))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (set-deftype-source-location ',name (c::source-location))
	 (%deftype ',name
		   ',arglist
		   #'(lambda (,whole)
		       ,@local-decs
		       (block ,name ,body))
		   ,@(when doc `(,doc)))))))

(defun set-deftype-source-location (name source-location)
  (setf (info :source-location :deftype name) source-location))

(defun %deftype (name lambda-list expander &optional doc)
  (when (info declaration recognized name)
    (error (intl:gettext "Deftype already names a declaration: ~S.") name))
  (ecase (info type kind name)
    (:primitive
     (when *type-system-initialized*
       (error (intl:gettext "Illegal to redefine standard type: ~S.") name)))
    (:instance
     (warn (intl:gettext "Redefining class ~S to be a DEFTYPE.") name)
     (undefine-structure (layout-info (%class-layout (kernel::find-class name))))
     (setf (class-cell-class (find-class-cell name)) nil)
     (setf (info type compiler-layout name) nil)
     (setf (info type kind name) :defined))
    (:defined)
    ((nil)
     (setf (info type kind name) :defined)))

  (setf (info type expander name) expander)
  (setf (info type lambda-list name) lambda-list)
  (when doc
    (setf (documentation name 'type) doc))
  ;; ### Bootstrap hack -- we need to define types before %note-type-defined
  ;; is defined.
  (when (fboundp 'c::%note-type-defined)
    (c::%note-type-defined name))
  name)
