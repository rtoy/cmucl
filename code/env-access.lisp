;;; -*- Mode: Lisp; Package: C; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;

(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/env-access.lisp,v 1.1 2007/08/02 16:11:17 rtoy Exp $")

;;;
;;; **********************************************************************
;;;
;;;   The environment access functions specified in Common Lisp the
;;;   Language, 2nd edition.
;;;

(in-package "EXT")

(export '(variable-information
	  function-information
	  declaration-information
	  parse-macro))


(in-package "C")

(defun variable-information (var &optional env)
  "Return three values. The first indicates a binding kind of VAR; the
second is True if there is a local binding of VAR; the third is an
alist of declarations that apply to the apparent binding of VAR."
  (let* ((*lexical-environment* (or env (make-null-environment)))
         (info (lexenv-find var variables)))
    (etypecase info
      (leaf
       (let ((type (type-specifier
		    (type-intersection
		     (leaf-type info)
		     (or (lexenv-find info type-restrictions)
			 *universal-type*)))))
	 (etypecase info
	   (lambda-var
	    (values :lexical t
		    `((ignore . ,(lambda-var-ignorep info))
		      (type . ,type)
		      (dynamic-extent . ,(lambda-var-dynamic-extent info)))))
	   (global-var
	    (values :special t
		    `((type . ,type))
		    ))
	   (constant
	    (values :constant nil
		    `((type . ,type))
		    )))))
      (cons
       (values :symbol-macro t
	       nil))
      (null
       (values (ecase (info variable kind var)
		 (:special :special)
		 (:constant :constant)
		 (:macro :symbol-macro)
		 (:global nil))
	       nil
	       `(
		 (type . ,(type-specifier
			   (info variable type var)))))))))

(defun declaration-information (declaration-name &optional env)
  (let ((env (or env (make-null-environment))))
    (case declaration-name
      (optimize
       (let ((cookie (lexenv-cookie env)))
	 (list (list 'speed (cookie-speed cookie))
	       (list 'safety (cookie-safety cookie))
	       (list 'compilation-speed (cookie-cspeed cookie))
	       (list 'space (cookie-space cookie))
	       (list 'debug (cookie-debug cookie))
	       (list 'inhibit-warnings (cookie-brevity cookie))
	       (list 'float-accuracy (cookie-float-accuracy cookie)))
         ))
      (ext:optimize-interface
       (let ((cookie (lexenv-interface-cookie env)))
	 (list (list 'speed (cookie-speed cookie))
	       (list 'safety (cookie-safety cookie))
	       (list 'compilation-speed (cookie-cspeed cookie))
	       (list 'space (cookie-space cookie))
	       (list 'debug (cookie-debug cookie))
	       (list 'inhibit-warnings (cookie-brevity cookie))
	       (list 'float-accuracy (cookie-float-accuracy cookie)))))
      (t (error "Unsupported declaration ~S." declaration-name)))))

(defun parse-macro (name lambda-list body &optional env)
  (declare (ignore env))
  (let ((whole (gensym "WHOLE-"))
	(environment (gensym "ENVIRONMENT-")))
    (multiple-value-bind (body decls)
        (lisp::parse-defmacro lambda-list whole body name
			      'parse-macro
			      :environment environment)
      `(lambda (,whole ,environment)
         ,@decls
         ,body))))

(defun function-information (function &optional env)
  (flet ((inlinealist (i)
	   (ecase i
	     (:inline
	      (list '(inline . inline)))
	     (:notinline
	      (list '(inline . notinline)))
	     (:maybe-inline
	      (list '(inline . maybe-inline)))
	     ((nil)
	      nil))))
    (let* ((*lexical-environment* (or env (make-null-environment)))
	   (info (lexenv-find-function function)))
      (etypecase info
	(clambda
	 (let ((type (type-specifier
		      (type-intersection
		       (leaf-type info)
		       (or (lexenv-find info type-restrictions)
			   *universal-type*)))))
	   (values :function
		   t
		   (nconc (if (functional-dynamic-extent info)
			      (list '(dynamic-extent . t)))
			  (inlinealist (functional-inlinep info))
			  (if (not (eq type 'function))
			      (list `(ftype  . ,type)))))))
	(cons
	 (values :macro t nil))
	(null
	 (multiple-value-bind (kind kindp)
	     (info function kind function)
	   (cond  (kindp
		   (ecase kind
		     (:macro
		      (values :macro nil nil))
		     (:special-form
		      (values :special-form nil nil))
		     (:function
		      (values
		       :function
		       nil
		       (nconc (list `(ftype . ,(type-specifier (info function type function))))
			      (inlinealist (info function inlinep function)))))))
		  (t
		   (if (eq kind :function)
		       (values
			:function
			nil
			(nconc (list `(ftype . ,(type-specifier (info function type function))))
			       (inlinealist (info function inlinep function))))
		   (values nil nil nil))))))
	(defined-function
	 (let ((type (type-specifier
		      (type-intersection
		       (defined-function-type info)
		       (or (lexenv-find info type-restrictions)
			   *universal-type*)))))
	   (values :function
		   nil
		   (nconc (if (not (eq type 'function))
			      (list `(ftype . ,type)))
			  (inlinealist (defined-function-inlinep info ))))))))))

#+(or)
(defun augment-environment (env &key variable symbol-macro function macro declare)
  (when (or macro symbol-macro)
    (setq env (copy-structure env)))
  (when macro
    (setf (lexenv-functions env)
          (nconc
           (loop for (name def) in macro
              collect (cons name (cons 'sys::macro def)))
           (lexenv-functions env))))
  (when symbol-macro
    (setf (lexenv-variables env)
          (nconc
           (loop for (name def) in symbol-macro
              collect (cons name (cons 'sys::macro def)))
           (lexenv-variables env))))
  (if (not (or variable function declare))
      env
      (handler-bind (((or style-warning)
                      #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'muffle-warning))))
        (eval-in-lexenv
         `(flet ,(loop for fn in function collect `(,fn ()))
            (let ,variable
              (declare ,@declare)
              (env)))
         env))))
