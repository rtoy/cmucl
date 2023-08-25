;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: condition.lisp,v 1.11 2004/08/19 06:26:06 yuji Exp $
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



;; 9.1.1 Condition Types
;; http://www.lispworks.com/reference/HyperSpec/Body/09_aa.htm
;; CONDITION is defined in clos.lisp.
(define-condition warning () ())
(define-condition style-warning (warning) ())
(define-condition serious-condition () ())
(define-condition storage-condition (serious-condition) ())
(define-condition error (serious-condition) ())
(define-condition type-error (error)
  ((datum :initarg :datum :accessor type-error-datum)
   (expected-type :initarg :expected-type :accessor type-error-expected-type))
  (:report (lambda (condition stream)
             (format stream "~S is not of type ~S"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))
(define-condition package-error (error)
  ((package :initarg :package :accessor package-error-package)))
(define-condition control-error (error) ())
(define-condition print-not-readable (error)
  ((object :initarg :object :accessor print-not-readable-object)))
(define-condition program-error (error) ())
(define-condition file-error (error)
  ((pathname :initarg :pathname :accessor file-error-pathname)))
(define-condition stream-error (error)
  ((stream :initarg :stream :accessor stream-error-stream)))
(define-condition end-of-file (stream-error) ())
(define-condition parse-error (error) ())
(define-condition reader-error (parse-error stream-error) ())
(define-condition cell-error (error)
  ((name :initarg :name :accessor cell-error-name)))
(define-condition unbound-variable (cell-error) ())
(define-condition undefined-function (cell-error) ())
(define-condition unbound-slot (cell-error)
  ((instance :initarg :incetance :accessor unbound-slot-instance)))
(define-condition arithmetic-error (error)
  ((operation :initarg :operation :accessor arithmetic-error-operation)
   (operands  :initarg :operands  :accessor arithmetic-error-operands)))
(define-condition division-by-zero (arithmetic-error) ())
(define-condition floating-point-inexact (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())
(define-condition floating-point-overflow (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())
(define-condition simple-condition (condition)
  ((format-control :initarg :format-control
                   :accessor simple-condition-format-control)
   (format-arguments :initarg :format-arguments
                     :accessor simple-condition-format-arguments))
  (:report (lambda (condition stream)
             (apply #'format stream
                    (simple-condition-format-control condition)
                    (simple-condition-format-arguments condition)))))
(define-condition simple-warning (simple-condition warning) ())
(define-condition simple-error (simple-condition error) ())
(define-condition simple-type-error (simple-condition type-error) ())
;; non standard
(define-condition simple-program-error (simple-condition program-error) ())



;; utilities
(defun existing-condition-name-p (object)
  (and (symbolp object) (subtypep object 'condition)))
(deftype condition-designator ()
  '(or string condition (satisfies existing-condition-name-p)))
(defun condition (datum arguments
                   &optional (default-simple-condition 'simple-condition))
  (typecase datum
    (condition (when arguments
		 (error 'type-error :datum arguments :expected-type 'null))
	       datum)
    (string (make-condition default-simple-condition
			    :format-control
                            (concatenate 'string *message-prefix* datum)
                            :format-arguments arguments))
    ((satisfies existing-condition-name-p)
     (apply #'make-condition datum arguments))
    (t (error 'type-error :datum datum :expected-type 'condition-designator))))



;; 9.1.4 Handling Conditions
;; http://www.lispworks.com/reference/HyperSpec/Body/09_ad.htm
(defvar *handler-clusters* '())
(defmacro handler-bind (bindings &body forms)
  ;; binding::= (type handler)
  ;; type    --- a type specifier.
  ;; handler --- a form; evaluated to produce a handler-function.
  `(let ((*handler-clusters*
          (cons (list ,@(mapcar #'(lambda (binding)
                                    (destructuring-bind (type handler) binding
                                      `(cons ',type ,handler)))
                                bindings))
		*handler-clusters*)))
    ,@forms))

(defun handler-case-bindings-and-body (block-tag condition-variable
                                       handler-case-clauses)
  (loop for clause in handler-case-clauses
        for (typespec (var) . rest) = clause
        for clause-tag = (gensym)
        collect `(,typespec #'(lambda (temp)
                                ,(if var
                                     `(setq ,condition-variable temp)
                                     '(declare (ignore temp)))
                                (go ,clause-tag)))
          into bindings
        append `(,clause-tag (return-from ,block-tag
                               (let ,(when var `((,var ,condition-variable)))
                                 ,@rest)))
          into body
        finally (return (values bindings body))))

(defmacro handler-case (form &rest clauses)
  (let ((no-error-clause (assoc ':no-error clauses)))
    (if no-error-clause
        (let ((normal-return (gensym "NORMAL-RETURN-"))
              (error-return  (gensym "ERROR-RETURN-")))
          `(block ,error-return
	    (multiple-value-call #'(lambda ,@(cdr no-error-clause))
	      (block ,normal-return
		(return-from ,error-return
		  (handler-case (return-from ,normal-return ,form)
		    ,@(remove no-error-clause clauses)))))))
        (let ((block-tag (gensym))
              (condition (gensym)))
          (multiple-value-bind (bindings body)
              (handler-case-bindings-and-body block-tag condition clauses)
            `(block ,block-tag
              (let ((,condition nil))
                (declare (ignorable ,condition))
                (tagbody
                   (handler-bind ,bindings
                     (return-from ,block-tag ,form))
                   ,@body))))))))

(defmacro ignore-errors (&body body)
  `(handler-case (progn ,@body)
    (error (condition) (values nil condition))))



;; 9.1.4.2 Restarts
;; http://www.lispworks.com/reference/HyperSpec/Body/09_adb.htm
(defstruct (restart (:print-object print-restart))
  (name nil :type symbol)
  (function (required-argument) :type function)
  report-function
  interactive-function
  (test-function #'(lambda (condition) (declare (ignore condition)) t)))
(defun print-restart (restart stream)
  (format stream "#<~A restart (sacla)>" (restart-name restart)))

(defvar *restart-clusters* 'nil)
(defmacro restart-bind (restart-specs &body forms)
  "Execute FORMS in a dynamic environment where specified restarts are ~
   in effect."
  ;; restart-spec::= (name function {key-val-pair}*)
  ;; key-val-pair::= :interactive-function interactive-function |  
  ;;                 :report-function report-function |  
  ;;                 :test-function test-function 
  (flet ((make-restart-form (spec)
           (destructuring-bind (name function . initargs) spec
             `(make-restart :name ',name :function ,function ,@initargs))))
    `(let ((*restart-clusters* (cons (list ,@(mapcar #'make-restart-form
                                                     restart-specs))
                                     *restart-clusters*)))
      ,@forms)))

(defvar *condition-restarts* 'nil)
(defmacro with-condition-restarts (condition-form restarts-form &body forms)
  "Execute FORMS in an environment where restarts are associated with ~
   a condition."
  `(let ((*condition-restarts* (acons ,condition-form ,restarts-form
				      *condition-restarts*)))
    ,@forms))

(defmacro with-restarts (restarts-form form &environment env)
  "Associate a condition to be signaled by FORM with restarts of RESTARTS-FORM."
  (flet ((signaling-form-p (form)
           (and (consp form) (member (car form) '(cerror error signal warn)))))
    (let ((form (macroexpand form env)))
      (if (not (signaling-form-p form))
          form
          (let* ((condition (gensym))
                 (signaler (car form))
                 (datum (if (eq signaler 'cerror) (third form) (second form)))
                 (args (if (eq signaler 'cerror) (cdddr form) (cddr form)))
                 (default-condition (ecase signaler
                                      ((cerror error) 'simple-error)
                                      (warning 'simple-warning)
                                      (signal 'simple-condition))))
            `(let ((,condition (condition ,datum ',args ',default-condition)))
              (with-condition-restarts ,condition
                ,restarts-form
                ,(if (eq signaler 'cerror)
                     `(cerror ,(second form) ,condition)
                     `(,signaler ,condition)))))))))

(defun restart-case-bindings-and-body (block-tag args-var restart-case-clauses)
  ;; restart-case-clause::= (case-name lambda-list
  ;; [[:interactive interactive-expression | :report report-expression |
  ;;   :test test-expression]] declaration* form*)
  (loop for clause in restart-case-clauses
        for (case-name lambda-list . tail) = clause
        for clause-tag = (gensym)
        for initargs =
          (loop for plist on tail by #'cddr
                for (key value) = plist
                and names = '(:interactive :report :test) then (remove key names)
                if (member key names)
                  if (eq key :interactive)
                    append `(:interactive-function #',value)
                  else if (eq key :report)
                    append `(:report-function
                             #',(if (not (stringp value))
                                    value
                                    `(lambda (stream)
                                      (write-string ,value stream))))
                  else if (eq key :test) append `(:test-function #',value)
                else do (loop-finish)
                finally (setq tail plist))
        collect `(,case-name
                  #'(lambda (&rest rest) (setq ,args-var rest) (go ,clause-tag))
                  ,@initargs) into bindings
        append `(,clause-tag
                 (return-from ,block-tag
                   (apply #'(lambda ,lambda-list ,@tail) ,args-var))) into body
        finally (return (values bindings body))))

(defmacro restart-case (restartable-form &body clauses)
  "Eval RESTARTABLE-FORM in an environment with restarts specified by CLAUSES."
  (let ((block-tag (gensym))
        (args (gensym)))
    (multiple-value-bind (bindings body)
        (restart-case-bindings-and-body block-tag args clauses)
      `(block ,block-tag
        (let ((,args nil))
          (declare (ignorable ,args))
          (tagbody
             (restart-bind ,bindings
               (return-from ,block-tag
                 (with-restarts (first *restart-clusters*) ,restartable-form)))
             ,@body))))))

(defmacro with-simple-restart ((restart-name format-string &rest format-arguments)
			       &body forms)
  `(restart-case (progn ,@forms)
    (,restart-name ()
     :report (lambda (stream) (format stream ,format-string ,@format-arguments))
     (values nil t))))

(defun compute-restarts (&optional condition)
  "Compute a list of the restarts which are currently active."
  (let ((visibles nil)
	(ignorables nil))
    (dolist (assoc *condition-restarts*)
      (if (eq (car assoc) condition)
	  (setq visibles (append (cdr assoc) visibles))
	  (setq ignorables (append (cdr assoc) ignorables))))
    (flet ((visible-p (restart)
             (and (or (null condition)
                      (member restart visibles)
                      (not (member restart ignorables)))
                  (funcall (restart-test-function restart) condition))))
      (loop for restart in (mapcan #'copy-list *restart-clusters*)
            if (visible-p restart) collect restart))))

(defun find-restart (id &optional condition)
  "Search for a particular restart in the current dynamic environment."
  (if (restart-p id)
      (if (funcall (restart-test-function id) condition)
	  id
	  nil)
      (find id (compute-restarts condition) :key #'restart-name)))

(defun restart (designator)
  (or (find-restart designator)
      (error "Restart ~S is not active." designator)))

(defun invoke-restart (restart-designator &rest values)
  (let ((restart (restart restart-designator)))
    (apply (restart-function restart) values)))

(defun invoke-restart-interactively (restart-designator)
  (let* ((restart (restart restart-designator))
	 (interactive-function (restart-interactive-function restart)))
    (apply (restart-function restart) (if interactive-function
					  (funcall interactive-function)
					  '()))))

(defun abort (&optional condition)
  (let ((restart (find-restart 'abort condition)))
    (when restart
      (invoke-restart 'abort))
    (error 'control-error)))

(defun muffle-warning (&optional condition)
  (let ((restart (find-restart 'muffle-warning condition)))
    (when restart
      (invoke-restart 'muffle-warning))
    (error 'control-error)))

(defun continue (&optional condition)
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defun store-value (value &optional condition)
  (let ((restart (find-restart 'store-value condition)))
    (when restart
      (invoke-restart 'store-value value))))

(defun use-value (value &optional condition)
  (let ((restart (find-restart 'use-value condition)))
    (when restart
      (invoke-restart 'use-value value))))



(defvar *break-on-signals* 'nil)
(defun break (&optional (format-control "Break") &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger (make-condition 'simple-condition
				       :format-control format-control
				       :format-arguments format-arguments))))
  nil)
(defun signal (datum &rest arguments)
  (let ((condition (condition datum arguments))
	(*handler-clusters* *handler-clusters*))
    (when (typep condition *break-on-signals*)
      (break "~A~%Break entered because of *BREAK-ON-SIGNALS*." condition))
    (loop while *handler-clusters*
          do (dolist (handler (pop *handler-clusters*))
               (when (typep condition (car handler))
                 (funcall (cdr handler) condition))))
    nil))

(defun error (datum &rest arguments)
  (let ((condition (condition datum arguments 'simple-error)))
    (signal condition)
    (invoke-debugger condition)))

(defun cerror (continue-format-control datum &rest arguments)
  (restart-case (error (condition datum arguments 'simple-error))
    (continue ()
      :report (lambda (stream)
		(apply #'format stream continue-format-control arguments))))
  nil)

(defmacro check-type (place typespec &optional string)
  (declare (ignorable string))
  `(loop
    until (typep ,place ',typespec)
    do (restart-case (error 'type-error :datum ,place :expected-type ',typespec)
	 (store-value (value)
	   :report (lambda (stream) (store-value-report stream ',place))
	   :interactive store-value-interactive
	   (setf ,place value)))))

(defun warn (datum &rest arguments)
  (let ((condition (condition datum arguments 'simple-warning)))
    (check-type condition warning)
    (restart-case (signal condition)
      (muffle-warning ()
	:report "Skip warning."
	(return-from warn nil)))
    (format *error-output* "~&Warning:~%~A~%" condition)
    nil))



(defun assert-report (names stream)
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}." (length names) names)
      (format stream ".")))
(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~%~
                    Do you want to supply a new value? "
		   name value)
	 (format *query-io* "~&Type a form to be evaluated:~%")
	 (flet ((read-it () (eval (read *query-io*))))
	   (if (symbolp name)		;help user debug lexical variables
	       (progv (list name) (list value) (read-it))
	       (read-it))))
	(t value)))
(defmacro assert (test-form &optional places datum-form &rest argument-forms)
  `(loop
    (when ,test-form (return nil))
    (restart-case (error ,@(if datum-form
			       `(,datum-form ,@argument-forms)
			       `("The assertion ~S failed." ',test-form)))
      (continue ()
	:report (lambda (stream) (assert-report ',places stream))
	,@(mapcar #'(lambda (place)
		      `(setf ,place (assert-prompt ',place ,place)))
		  places)))))

;;Function INVOKE-DEBUGGER
;;Variable *DEBUGGER-HOOK*
;;
;;Defined in clos.lisp
;;    Macro DEFINE-CONDITION
;; Function MAKE-CONDITION
;; Function INVALID-METHOD-ERROR
;; Function METHOD-COMBINATION-ERROR
