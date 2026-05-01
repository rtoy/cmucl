;; Bootstrap file for issue #495 to add more information when
;; describing user-defined types.
;;
(setf lisp::*enable-package-locked-errors* nil)

(in-package "C")
;; New deftype information.
;;
;; We define a :lambda-list for types where we can store the
;; lambda-list of the deftype.  We also add a :defype source-location
;; for deftypes so we don't use :defvar for this.
(define-info-type type lambda-list list nil)
(define-info-type source-location deftype (or form-numbers null) nil)

(in-package "LISP")
;; New deftype macro to saves the lambda-list of the type.  %deftype
;; is updated to take the extra arg for the lambda-list.
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


;; Fix #497.  Need these new definitions here, including
;; def-ir1-translator to get defmacros from setup.lisp processed
;; correctly.
(define-info-type function macro-arglist list nil)
(defun c::%defmacro (name definition lambda-list doc)
  (assert (eval:interpreted-function-p definition))
  (setf (eval:interpreted-function-name definition) name)
  (setf (eval:interpreted-function-arglist definition) lambda-list)
  (c::%%defmacro name definition lambda-list doc))

;; lambda-list and doc are optional here so we can bootstrap these
;; changes.  The final implementation has 4 required args in this
;; order.  The kernel.core might have slots in the wrong place, but
;; should get fixed up later.
(defun c::%%defmacro (name definition &optional lambda-list doc)
  (when (and (or (stringp lambda-list) (null lambda-list))
	     (null doc))
    (setf doc lambda-list
	  lambda-list nil))
  (clear-info function where-from name)
  (setf (macro-function name) definition)
  (setf (documentation name 'function) doc)
  (when lambda-list
    (setf (info :function :macro-arglist name) lambda-list))
  name)

(in-package "C")
(def-ir1-translator %defmacro ((name def lambda-list doc) start cont
			       :kind :function)
  (let ((name (eval name))
	(def (second def))) ; Don't want to make a function just yet...

    (let* ((*current-path* (revert-source-path 'defmacro))
	   (fun (ir1-convert-lambda def name 'defmacro)))
      (setf (leaf-name fun) (list :macro name))
      (setf (functional-arg-documentation fun) (eval lambda-list))
      ;; Save the macro lambda-list so it can be retrieved later.
      (ir1-convert start cont `(%%defmacro ',name ,fun ',(eval lambda-list) ,doc)))

    (when *compile-print*
      (compiler-mumble (intl:gettext "~&; Converted ~S.~%") name))))


;;; For #498:  add source-location info for defmacro
(define-info-type source-location defmacro (or form-numbers null) nil)  
