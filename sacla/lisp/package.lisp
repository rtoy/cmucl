;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: package.lisp,v 1.21 2004/09/02 06:59:43 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defstruct (package
	     (:constructor %make-package)
	     (:print-object print-package)
	     (:predicate packagep))
  ""
  (%name nil :type (or string null))
  (%nicknames nil :type list)
  (%shadowing-symbols nil :type list)
  (%use-list nil :type list)
  (%used-by-list nil :type list)
  (internal-symbols (make-hash-table :test 'equal) :type hash-table)
  (external-symbols (make-hash-table :test 'equal) :type hash-table))

(defun print-package (package stream)
  (format stream "#<~A package (sumire)>" (package-%name package)))

(defvar *keyword-package* (%make-package :%name "KEYWORD" :%nicknames '())
  "")

(defvar *cl-package* (%make-package :%name "COMMON-LISP" :%nicknames (list "CL"))
  "")

(defvar *cl-user-package*
  (%make-package :%name "COMMON-LISP-USER" :%nicknames (list "CL-USER")
		 :%use-list (list *cl-package*))
  "")

(setf (package-%used-by-list *cl-package*) (list *cl-user-package*))

(defvar *package* *cl-user-package*
  "The current package.")

(defvar *all-packages* (list *cl-user-package* *cl-package* *keyword-package*)
  "")

(define-condition non-existent-package-name-error (package-error) ())

(defun %package (designator)
  (or (find-package designator)
      (and (typep designator 'package-designator)
	   (error 'non-existent-package-name-error :package designator))
      (error 'type-error :datum designator :expected-type 'package-designator)))

(defun %package-list (designator)
  (mapcar #'%package (%list designator)))

(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *package* (%package ',name))))

(defun list-all-packages ()
  (copy-list *all-packages*))

(defun find-registered-package (name)
  (block this
    (dolist (package *all-packages* nil)
      (when (string= name (package-%name package))
	(return-from this package))
      (dolist (nickname (package-%nicknames package))
	(when (string= name nickname)
	  (return-from this package))))))

(defun find-package (name)
  (if (packagep name)
      name
      (find-registered-package (string name))))

(defun unuse-package (packages-to-unuse &optional (package *package*))
  (let ((packages-to-unuse (%package-list packages-to-unuse))
	(package (%package package)))
    (dolist (unuse packages-to-unuse t)
      (setf (package-%use-list package)
	    (remove unuse (package-%use-list package)))
      (setf (package-%used-by-list unuse)
	    (remove package (package-%used-by-list unuse))))))

(defun package-name (package)
  (copy-seq (package-%name (%package package))))

(defun package-nicknames (package)
  (copy-list (package-%nicknames (%package package))))

(defun package-shadowing-symbols (package)
  (copy-list (package-%shadowing-symbols (%package package))))

(defun package-use-list (package)
  (copy-list (package-%use-list (%package package))))

(defun package-used-by-list (package)
  (copy-list (package-%used-by-list (%package package))))

(define-condition package-name-error (simple-error)
  ((name :type string :reader package-name-error-name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "A package named ~S already exists."
                     (package-name-error-name condition)))))

(defun rename-package (package new-name &optional new-nicknames)
  (let* ((package (%package package))
	 (new-name (string new-name))
	 (new-nicknames (string-list new-nicknames)))
    (dolist (name (cons new-name new-nicknames))
      (let ((found (find-package name)))
	(when (and found (not (eq package found)))
	  (error 'package-name-error :name name))))
    (setf (package-%name package) new-name
	  (package-%nicknames package) new-nicknames)
    package))

(defun find-symbol (name &optional (package *package*))
  (let ((name (string name))
	(package (%package package)))
    (multiple-value-bind (symbol registered-p)
	(gethash name (package-external-symbols package))
      (if registered-p
	  (values symbol :external)
	  (multiple-value-bind (symbol registered-p)
	      (gethash name (package-internal-symbols package))
	    (if registered-p
		(values symbol :internal)
		(dolist (used (package-use-list package) (values nil nil))
		  (multiple-value-bind (symbol registered-p)
		      (gethash name (package-external-symbols used))
		    (when registered-p
		      (return (values symbol :inherited)))))))))))

(defun find-all-symbols (name)
  (let ((name (string name))
	(all-symbols nil))
    (dolist (package (list-all-packages) (remove-duplicates all-symbols))
      (multiple-value-bind (symbol status) (find-symbol name package)
	(case status ((:internal :external) (push symbol all-symbols)))))))

(defun accessible-symbol-p (symbol package)
  (multiple-value-bind (symbol-found status)
      (find-symbol (symbol-name symbol) package)
    (and (eq symbol symbol-found) status)))

(defun present-symbol-p (symbol package)
  (multiple-value-bind (symbol-found status)
      (find-symbol (symbol-name symbol) package)
    (and (eq symbol-found symbol) (find status '(:internal :external)))))

(define-condition unintern-would-reveal-name-conflict-error (package-error)
  ((symbol :type symbol :reader unintern-error-symbol :initarg :symbol)))

(defun unintern (symbol &optional (package *package*))
  (flet ((conflicting-inherited-symbols (name package)
	   (let ((symbols nil))
	     (dolist (used-package (package-use-list package))
	       (multiple-value-bind (symbol-found foundp)
		   (gethash name (package-external-symbols used-package))
		 (when foundp
		   (pushnew symbol-found symbols))))
	     (if (cdr symbols) symbols nil))))
    (let* ((name (symbol-name symbol))
	   (package (%package package))
	   (present-p (present-symbol-p symbol package)))
      (when present-p
	(when (member symbol (package-shadowing-symbols package))
	  (when (conflicting-inherited-symbols name package)
	    (error 'unintern-would-reveal-name-conflict-error
		   :symbol symbol :package package))
	  (setf (package-%shadowing-symbols package)
		(remove symbol (package-%shadowing-symbols package))))
	(remhash name (ecase present-p
			(:internal (package-internal-symbols package))
			(:external (package-external-symbols package))))
	(when (eq (symbol-package symbol) package)
	  (setf (symbol-package symbol) nil))
	t))))


(defun shadowing-import (symbol-list-designator &optional (package *package*))
  (let ((symbol-list (symbol-list symbol-list-designator))
	(package (%package package)))
    (dolist (symbol symbol-list t)
      (let ((name (symbol-name symbol)))
	(multiple-value-bind (symbol-found status) (find-symbol name package)
	  (let ((present-p (member status '(:internal :external))))
	    (when (and present-p (not (eq symbol symbol-found)))
	      (unintern symbol-found package))
	    (unless (and present-p (eq symbol symbol-found))
	      (setf (gethash name (package-internal-symbols package)) symbol))
	    (pushnew symbol (package-%shadowing-symbols package))))))))

(define-condition import-would-cause-shadowing-error (package-error)
  ((symbol :type symbol :reader import-error-symbol :initarg :symbol)))

(defun import (symbol-list-designator &optional (package *package*))
  (let ((symbol-list (symbol-list symbol-list-designator))
	(package (%package package)))
    (dolist (symbol symbol-list t)
      (let ((name (symbol-name symbol)))
	(multiple-value-bind (symbol-found status) (find-symbol name package)
	  (cond
	    ((and status (not (eq symbol symbol-found)))
	     (cerror "Import this symbol with shadowing-import."
		     'import-would-cause-shadowing-error
		     :package package :symbol symbol)
	     (shadowing-import symbol package))
	    ((and (member status '(:internal :external))
		  (eq symbol symbol-found))
	     ;; The spec says `If the symbol is already present
	     ;; in the importing package, import has no effect.'
	     )
	    (t
	     (setf (gethash name (package-internal-symbols package)) symbol)
	     (when (null (symbol-package symbol))
	       (setf (symbol-package symbol) package)))))))))

(define-condition use-package-would-cause-name-conflict-error (package-error)
  ((names :type list :reader use-package-error-names :initarg :names)
   (package-to-use :type package :reader use-package-error-package-to-use
		   :initarg :package-to-use)))

(defun check-use-package-name-conflict (using-package package-to-use)
  (let* ((conflicting-names nil)
	 (shadows (package-shadowing-symbols using-package))
	 (user-tables (cons (package-internal-symbols using-package)
			    (cons (package-external-symbols using-package)
				  (mapcar #'package-external-symbols
					  (package-use-list using-package)))))
	 (fat-user
	  (> (reduce #'+ user-tables :key #'hash-table-count)
	     (hash-table-count (package-external-symbols package-to-use))))
	 (tables (if fat-user
		     (list (package-external-symbols package-to-use))
		     user-tables))
	 (package (if fat-user using-package package-to-use)))
    (mapc #'(lambda (table)
	      (maphash
	       #'(lambda (name symbol)
		   (multiple-value-bind (symbol-found status)
		       (find-symbol name package)
		     (when (and status
				(not (eq symbol symbol-found))
				(not (member name shadows :test #'string=)))
		       (push name conflicting-names))))
	       table))
	  tables)
    (when conflicting-names
      (restart-case (error 'use-package-would-cause-name-conflict-error
			   :names conflicting-names :package package
			   :package-to-use package-to-use)
	(continue ()
	  :report "Shadowing-import the conflicting symbols."
	  (shadowing-import (mapcar #'(lambda (name)
					(find-symbol name package-to-use))
				    conflicting-names)
			    package))))))

(defun use-package (package-to-use-list &optional (package *package*))
  (let ((package-to-use-list (%package-list package-to-use-list))
	(package (%package package)))
    (dolist (package-to-use package-to-use-list t)
      (cond
	((member package-to-use (package-use-list package)))
	((eq package-to-use *keyword-package*)
	 (warn "The keyword package cannot be used by other packages."))
	((eq package-to-use package)
	 (warn "A package cannot use-package itself."))
	(t (check-use-package-name-conflict package package-to-use)
	   (push package-to-use (package-%use-list package))
	   (push package (package-%used-by-list package-to-use)))))))

(defun make-package (name &key nicknames use)
  (let ((package
	 (%make-package
	  :%name (cond
		   ((not (find-package name)) (string name))
		   (t (cerror "Return the existing package."
			      'package-name-error :name name)
		      (return-from make-package (find-package name))))
	  :%nicknames (mapcan
		       #'(lambda (nickname)
			   (cond
			     ((string= nickname name) nil)
			     ((find-package nickname)
			      (cerror "Don't use this nickname."
				      'package-name-error :name nickname))
			     (t (list (string nickname)))))
		       nicknames))))
    (use-package use package)
    (pushnew package *all-packages*)
    package))

(define-condition non-accessible-symbol-error (package-error)
  ((symbol :type symbol
	   :reader non-accessible-symbol-error-symbol :initarg :symbol)))
(define-condition export-would-cause-conflict-in-user-package-error
    (package-error)
  ((symbol :type symbol :reader export-error-symbol :initarg :symbol)
   (user-package :type package
		 :reader export-error-user-package :initarg :user-package)))

(defun export (symbol-list-designator &optional (package *package*))
  (let ((symbol-list (symbol-list symbol-list-designator))
	(package (%package package))
	status)
    (dolist (symbol symbol-list t)
      (loop until (setq status (accessible-symbol-p symbol package))
	    do
	    (cerror "Import this symbol." 'non-accessible-symbol-error
		    :package package :symbol symbol)
	    (import (list symbol) package))
      (unless (eq status :external)
	(let ((name (symbol-name symbol)))
	  (dolist (user (package-used-by-list package))
	    (loop
	     (multiple-value-bind (symbol-found status) (find-symbol name user)
	       (when (or (null status) (eq symbol symbol-found))
		 (return))
	       (cerror "Shadowing-import the symbol in the user package."
		       'export-would-cause-conflict-in-user-package-error
		       :package package :user-package user :symbol symbol)
	       (shadowing-import (list symbol) user))))
	  (when (eq status :inherited)
	    (import (list symbol) package))
	  (remhash name (package-internal-symbols package))
	  (setf (gethash name (package-external-symbols package)) symbol))))))
    
(defun unexport (symbol-list-designator &optional (package *package*))
  (let ((symbol-list (symbol-list symbol-list-designator))
	(package (%package package))
	status)
    (dolist (symbol symbol-list t)
      (unless (setq status (accessible-symbol-p symbol package))
	(cerror "Import this symbol." 'non-accessible-symbol-error
		:package package :symbol symbol))
      (when (eq status :external)
	(remhash (symbol-name symbol) (package-external-symbols package))
	(setf (gethash (symbol-name symbol) (package-internal-symbols package))
	      symbol)))))

(defun intern (name &optional (package *package*))
  (let ((name (string name))
	(package (%package package)))
    (multiple-value-bind (symbol status) (find-symbol name package)
      (if status
	  (values symbol status)
	  (let ((symbol (make-symbol name)))
	    (import (list symbol) package)
	    (when (eq package *keyword-package*)
	      (export (list symbol) package)
	      (setf (symbol-value symbol) symbol))
	    (values symbol nil))))))

(defun shadow (symbol-names &optional (package *package*))
  (let ((symbol-names (string-list symbol-names))
	(package (%package package)))
    (dolist (name symbol-names t)
      (multiple-value-bind (symbol status) (find-symbol name package)
	(when (or (not status) (eq status :inherited))
	  (setq symbol (make-symbol name))
	  (setf (symbol-package symbol) package)
	  (setf (gethash name (package-internal-symbols package)) symbol))
	(pushnew symbol (package-%shadowing-symbols package))))))

(defun hash-table-values (table)
  (let ((values nil))
    (with-hash-table-iterator (get table)
      (loop (multiple-value-bind (more k v) (get)
	      (declare (ignore k))
	      (unless more (return))
	      (push v values))))
    values))
  
(defun package-symbol-tables (package type)
  (ecase type
    (:internal (list (package-internal-symbols package)))
    (:external (list (package-external-symbols package)))
    (:inherited (mapcar #'package-external-symbols
			(package-use-list package)))))

(define-condition package-symbol-types-error (program-error)
  ((types :reader symbol-types-error-types :initarg :types)))

(defun shadowed-name-p (name package)
  (member name (package-shadowing-symbols package) :test #'string=))

(defun package-iterator (package &rest symbol-types)
  (unless symbol-types (error 'package-symbol-types-error :types symbol-types))
  (unless package (return-from package-iterator (constantly nil)))
  (let* ((package-list (%package-list package))
	 (package (pop package-list))
	 (type (first symbol-types))
	 (type-list (rest symbol-types))
	 (iterator (hash-table-iterator (package-symbol-tables package type))))
    #'(lambda ()
	(loop
	 (multiple-value-bind (more name symbol) (funcall iterator)
	   (cond
	     (more
	      (unless (and (eq type :inherited) (shadowed-name-p name package))
		(return (values more symbol type package))))
	     (t
	      (cond
		(type-list (setq type (pop type-list)))
		(package-list (setq type      (first symbol-types)
				    type-list (rest symbol-types)
				    package (pop package-list)))
		(t (return nil)))
	      (setq iterator
		    (hash-table-iterator (package-symbol-tables package
								type))))))))))

(defmacro with-package-iterator ((name package-list-form &rest symbol-types)
				 &body body)
  (unless symbol-types (error 'package-symbol-types-error :types symbol-types))
  (let ((iterator (gensym)))
    `(let ((,iterator (package-iterator ,package-list-form ,@symbol-types)))
      (macrolet ((,name () '(funcall ,iterator)))
	,@body))))

(defmacro do-package-symbols ((var package result-form &rest type-list)
			      &body body)
  (let ((get (gensym))
	(more (gensym))
	(type (gensym))
	(pkg (gensym)))
    (multiple-value-bind (declarations forms) (declarations-and-forms body)
      `(with-package-iterator (,get ,package ,@type-list)
	(loop
	 (multiple-value-bind (,more ,var ,type ,pkg) (,get)
	   (declare (ignore ,type ,pkg))
	   ,@declarations
	   (unless ,more (return ,result-form))
	   (tagbody
	      ,@forms)))))))

(defmacro do-symbols ((var &optional (package-form '*package*)
			   result-form)
		      &body body)
  (let ((package (gensym)))
    `(let ((,package (%package ,package-form)))
      (do-package-symbols (,var ,package ,result-form
			   :external :internal :inherited)
	,@body))))

(defmacro do-external-symbols ((var &optional (package-form '*package*)
				    result-form)
			       &body body)
  (let ((package (gensym)))
    `(let ((,package (%package ,package-form)))
      (do-package-symbols (,var ,package ,result-form :external)
	,@body))))

(defmacro do-all-symbols ((var &optional result-form) &body body)
  (let ((package (gensym))
	(body-function (gensym)))
    (multiple-value-bind (declarations forms) (declarations-and-forms body)
      `(block nil
	(flet ((,body-function (,var)
		 (declare (ignorable ,var))
		 ,@declarations
		 (tagbody ,@forms)))
	  (dolist (,package (list-all-packages) (let ((,var nil))
						  (declare (ignorable ,var))
						  ,@declarations
						  ,result-form))
	    (do-symbols (,var ,package nil)
	      (,body-function ,var))))))))

(define-condition deleting-package-used-by-others-error (package-error)
  ())

(defun delete-package (package)
  (let ((package (or (find-package package)
		     (return-from delete-package
		       (cerror "Return NIL." 'non-existent-package-name-error
			       :package package)))))
    (when (package-name package)
      (when (package-used-by-list package)
	   (cerror "Remove dependency in other packages."
		   'deleting-package-used-by-others-error :package package)
	   (dolist (user (package-used-by-list package))
	     (unuse-package package user)))
      (unuse-package (package-use-list package) package)
      (do-symbols (symbol package)
	(unintern symbol package))
      (setf (package-%name package) nil)
      (setq *all-packages* (remove package *all-packages*))
      t)))

(define-condition unsupported-defpackage-option-error (program-error)
  ((option :reader unsupported-defpackage-option-error-option :initarg :option)))

(define-condition non-accessible-symbol-name-error (package-error)
  ((name :type string
	 :reader non-accessible-symbol-name-error-name :initarg :name)))

(defun %accessible-symbols (name-list package)
  (mapcar #'(lambda (name)
	      (loop
	       (multiple-value-bind (symbol status)
		   (find-symbol name package)
		 (when status
		   (return symbol))
		 (cerror "Intern this symbol." 'non-accessible-symbol-name-error
			 :package package :name name)
		 (intern (string name) package))))
	  name-list))

(defun check-disjoint(&rest args)
  ;; An arg is (:key . set)
  (do ((list args (cdr list)))
      ((endp list))
    (loop
      with x = (car list)
      for y in (rest list)
      for z = (remove-duplicates (intersection (cdr x)(cdr y) :test #'string=))
      when z do (error 'program-error
		       :format-control "Parameters ~S and ~S must be disjoint ~
					but have common elements ~%   ~S"
		       :format-arguments (list (car x)(car y) z)))))

(defmacro defpackage (package-name &rest options)
  (let ((package-name (string package-name))
	forms nicknames shadow shadowing-import-from use import-from intern
	export documentation size)
    (loop
     for (key . values) in options
     do
     (case key
       (:nicknames     (setq nicknames (append nicknames values)))
       (:documentation (setq documentation (first values)))
       (:shadow        (setq shadow (append shadow values)))
       (:shadowing-import-from (push values shadowing-import-from))
       (:use           (setq use (append use values)))
       (:import-from   (push values import-from))
       (:intern        (setq intern (append intern values)))
       (:export        (setq export (append export values)))
       (:size          (setq size (first values)))
       (t (error 'unsupported-defpackage-option :option (cons key values)))))
    (check-disjoint `(:intern ,@intern)
		    `(:import-from ,@(apply #'append (mapcar #'rest
							     import-from)))
		    `(:shadow ,@shadow)
		    `(:shadowing-import-from
		      ,@(apply #'append (mapcar #'rest shadowing-import-from))))
    (check-disjoint `(:intern ,@intern) `(:export  ,@export))
    
    (push `(let ((package (find-package ,package-name)))
	    (if package
		(rename-package package
				,package-name (union ',(string-list nicknames)
						     (package-nicknames package)
						     :test #'string=))
		(make-package ,package-name :nicknames ',nicknames :use nil)))
	  forms)
    (when documentation
      (push `(setf (documentation ,package-name 'package) ,documentation) forms))
    (when shadow (push `(shadow ',(string-list shadow) ,package-name) forms))
    (when shadowing-import-from
      (loop for (from . names) in shadowing-import-from
	    do (push `(let ((names ',(string-list names)))
		       (shadowing-import (%accessible-symbols names ',from)
			,package-name))
		     forms)))
    (when use (push `(use-package ',use ,package-name) forms))
    (when import-from
      (loop for (from . names) in import-from
	    do (push `(let ((names ',(string-list names)))
		       (import (%accessible-symbols names ',from) ,package-name))
		     forms)))
    (when intern
      (dolist (symbol intern)
	(push `(intern ',symbol ,package-name) forms)))
    (when export
      (push `(export
	      (mapcar #'(lambda (name) (intern name ,package-name)) ',export)
	      ,package-name)
	    forms))
    (push `(find-package ,package-name) forms)
    `(eval-when (:load-toplevel :compile-toplevel :execute)
      ,@(nreverse forms))))

;;;
(defun clone-package-system ()
  (let ((src-list (mapcar #'cl:find-package
			  '("CL" "CL-USER" "KEYWORD" "TESTBED"))))
    (dolist (src src-list)
      (format t "Cloning the package ~S~%" src)
      (let* ((name (cl:package-name src))
	     (nicknames (cl:package-nicknames src))
	     (dest (or (tb::find-package name)
		       (tb::make-package name :nicknames nicknames))))
	(cl:with-package-iterator (get src :internal :external)
	  (loop
	   (multiple-value-bind (more symbol status package) (get)
	     (declare (ignore status package))
	     (unless more (return))
	     ;;(format t "shadowing symbols = ~S~%" (cl:package-shadowing-symbols src))
	     (if (member symbol (cl:package-shadowing-symbols src))
		 (shadowing-import (list symbol) dest)
		 (progn
		   ;;(format t "calling import~%")
		   (import (list symbol) dest)
		   ;;(format t "called import~%"
		   )))))
	(cl:do-external-symbols (symbol src)
	  (export (list symbol) dest))))
    (setq *package* (find-package (cl:package-name cl:*package*)))))

