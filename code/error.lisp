;;; -*- Package: conditions; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; This is a condition system for CMU Common Lisp.
;;; It was originally taken from some prototyping code written by KMP@Symbolics
;;; and massaged for our uses.
;;;

(in-package "CONDITIONS")

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* t))

(in-package "LISP")
(export '(break error warn cerror
	  ;;
	  ;; The following are found in Macros.Lisp:
	  check-type assert etypecase ctypecase ecase ccase
	  ;;
	  ;; These are all the new things to export from "LISP" now that this
	  ;; proposal has been accepted.
	  *break-on-signals* *debugger-hook* signal handler-case handler-bind
	  ignore-errors define-condition make-condition with-simple-restart
	  restart-case restart-bind restart-name restart-name find-restart
	  compute-restarts invoke-restart invoke-restart-interactively abort
	  continue muffle-warning store-value use-value invoke-debugger restart
	  condition warning serious-condition simple-condition simple-warning
	  simple-error simple-condition-format-string
	  simple-condition-format-arguments storage-condition stack-overflow
	  storage-exhausted type-error type-error-datum
	  type-error-expected-type simple-type-error program-error
	  control-error stream-error stream-error-stream end-of-file file-error
	  file-error-pathname cell-error unbound-variable undefined-function
	  arithmetic-error arithmetic-error-operation arithmetic-error-operands
	  package-error package-error-package division-by-zero
	  floating-point-overflow floating-point-underflow))


(in-package "CONDITIONS")

;;;; Keyword utilities.

(eval-when (eval compile load)

(defun parse-keyword-pairs (list keys)
  (do ((l list (cddr l))
       (k '() (list* (cadr l) (car l) k)))
      ((or (null l) (not (member (car l) keys)))
       (values (nreverse k) l))))

(defmacro with-keyword-pairs ((names expression &optional keywords-var)
			      &body forms)
  (let ((temp (member '&rest names)))
    (unless (= (length temp) 2)
      (error "&rest keyword is ~:[missing~;misplaced~]." temp))
    (let ((key-vars (ldiff names temp))
          (key-var (or keywords-var (gensym)))
          (rest-var (cadr temp)))
      (let ((keywords (mapcar #'(lambda (x)
				  (intern (string x) ext:*keyword-package*))
			      key-vars)))
        `(multiple-value-bind (,key-var ,rest-var)
             (parse-keyword-pairs ,expression ',keywords)
           (let ,(mapcar #'(lambda (var keyword)
			     `(,var (getf ,key-var ,keyword)))
			 key-vars keywords)
	     ,@forms))))))

) ;eval-when



;;;; Restarts.

(defvar *restart-clusters* '())

(defun compute-restarts ()
  "Return a list of all the currently active restarts ordered from most
   recently established to less recently established."
  (copy-list (apply #'append *restart-clusters*)))

(defun restart-print (restart stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<~S.~X>"
	      (type-of restart) (system:%primitive lisp::make-fixnum restart))
      (restart-report restart stream)))

(defstruct (restart (:print-function restart-print))
  name
  function
  report-function
  interactive-function)

(setf (documentation 'restart-name 'function)
      "Returns the name of the given restart object.")

(defun restart-report (restart stream)
  (funcall (or (restart-report-function restart)
               (let ((name (restart-name restart)))
		 #'(lambda (stream)
		     (if name (format stream "~S" name)
			      (format stream "~S" restart)))))
           stream))

(defmacro restart-bind (bindings &body forms)
  "Executes forms in a dynamic context where the given restart bindings are
   in effect.  Users probably want to use RESTART-CASE.  When clauses contain
   the same restart name, FIND-RESTART will find the first such clause."
  `(let ((*restart-clusters*
	  (cons (list
		 ,@(mapcar #'(lambda (binding)
			       (unless (or (car binding)
					   (member :report-function
						   binding :test #'eq))
				 (warn "Unnamed restart does not have a ~
					report function -- ~S"
				       binding))
			       `(make-restart
				 :name ',(car binding)
				 :function ,(cadr binding)
				 ,@(cddr binding)))
			       bindings))
		*restart-clusters*)))
     ,@forms))

(defun find-restart (name)
  "Returns the first restart named name.  If name is a restart, it is returned
   if it is currently active.  If no such restart is found, nil is returned.
   It is an error to supply nil as a name."
  (dolist (restart-cluster *restart-clusters*)
    (dolist (restart restart-cluster)
      (when (or (eq restart name) (eq (restart-name restart) name))
	(return-from find-restart restart)))))
  

(defun invoke-restart (restart &rest values)
  "Calls the function associated with the given restart, passing any given
   arguments.  If the argument restart is not a restart or a currently active
   non-nil restart name, then a control-error is signalled."
  (let ((real-restart (find-restart restart)))
    (unless real-restart
      (error 'control-error
	     :format-string "Restart ~S is not active."
	     :format-arguments (list restart)))
    (apply (restart-function real-restart) values)))

(defun invoke-restart-interactively (restart)
  "Calls the function associated with the given restart, prompting for any
   necessary arguments.  If the argument restart is not a restart or a
   currently active non-nil restart name, then a control-error is signalled."
  (let ((real-restart (find-restart restart)))
    (unless real-restart
      (error 'control-error
	     :format-string "Restart ~S is not active."
	     :format-arguments (list restart)))
    (apply (restart-function real-restart)
	   (let ((interactive-function
		  (restart-interactive-function real-restart)))
	     (if interactive-function
		 (funcall interactive-function)
		 '())))))


(defmacro restart-case (expression &body clauses)
  "(RESTART-CASE form
   {(case-name arg-list {keyword value}* body)}*)
   The form is evaluated in a dynamic context where the clauses have special
   meanings as points to which control may be transferred (see INVOKE-RESTART).
   When clauses contain the same case-name, FIND-RESTART will find the first
   such clause."
  (flet ((transform-keywords (&key report interactive)
	   (let ((result '()))
	     (when report
	       (setq result (list* (if (stringp report)
				       `#'(lambda (stream)
					    (write-string ,report stream))
				       `#',report)
				   :report-function
				   result)))
	     (when interactive
	       (setq result (list* `#',interactive
				   :interactive-function
				   result)))
	     (nreverse result))))
    (let ((temp-var (gensym))
	  (outer-tag (gensym))
	  (inner-tag (gensym))
	  (tag-var (gensym))
	  (data
	    (mapcar #'(lambda (clause)
			(with-keyword-pairs ((report interactive &rest forms)
					     (cddr clause))
			  (list (car clause)			   ;name=0
				(gensym)			   ;tag=1
				(transform-keywords :report report ;keywords=2
						    :interactive interactive)
				(cadr clause)			   ;bvl=3
				forms)))			   ;body=4
		    clauses)))
      `(let ((,outer-tag (cons nil nil))
	     (,inner-tag (cons nil nil))
	     ,temp-var ,tag-var)
	 (catch ,outer-tag
	   (catch ,inner-tag
	     (throw ,outer-tag
		    (restart-bind
			,(mapcar #'(lambda (datum)
				     (let ((name (nth 0 datum))
					   (tag  (nth 1 datum))
					   (keys (nth 2 datum)))
				       `(,name #'(lambda (&rest temp)
						   (setf ,temp-var temp)
						   (setf ,tag-var ',tag)
						   (throw ,inner-tag nil))
					       ,@keys)))
				 data)
		      ,expression)))
	   (case ,tag-var
	     ,@(mapcar #'(lambda (datum)
			   (let ((tag  (nth 1 datum))
				 (bvl  (nth 3 datum))
				 (body (nth 4 datum)))
			     `(,tag
			       (apply #'(lambda ,bvl ,@body) ,temp-var))))
		       data)))))))
#|
This macro doesn't work in our system due to lossage in closing over tags.
The previous version is uglier, but it sets up unique run-time tags.

(defmacro restart-case (expression &body clauses)
  "(RESTART-CASE form
   {(case-name arg-list {keyword value}* body)}*)
   The form is evaluated in a dynamic context where the clauses have special
   meanings as points to which control may be transferred (see INVOKE-RESTART).
   When clauses contain the same case-name, FIND-RESTART will find the first
   such clause."
  (flet ((transform-keywords (&key report interactive)
	   (let ((result '()))
	     (when report
	       (setq result (list* (if (stringp report)
				       `#'(lambda (stream)
					    (write-string ,report stream))
				       `#',report)
				   :report-function
				   result)))
	     (when interactive
	       (setq result (list* `#',interactive
				   :interactive-function
				   result)))
	     (nreverse result))))
    (let ((block-tag (gensym))
	  (temp-var  (gensym))
	  (data
	    (mapcar #'(lambda (clause)
			(with-keyword-pairs ((report interactive &rest forms)
					     (cddr clause))
			  (list (car clause)			   ;name=0
				(gensym)			   ;tag=1
				(transform-keywords :report report ;keywords=2
						    :interactive interactive)
				(cadr clause)			   ;bvl=3
				forms)))			   ;body=4
		    clauses)))
      `(block ,block-tag
	 (let ((,temp-var nil))
	   (tagbody
	     (restart-bind
	       ,(mapcar #'(lambda (datum)
			    (let ((name (nth 0 datum))
				  (tag  (nth 1 datum))
				  (keys (nth 2 datum)))
			      `(,name #'(lambda (&rest temp)
					  (setq ,temp-var temp)
					  (go ,tag))
				,@keys)))
			data)
	       (return-from ,block-tag ,expression))
	     ,@(mapcan #'(lambda (datum)
			   (let ((tag  (nth 1 datum))
				 (bvl  (nth 3 datum))
				 (body (nth 4 datum)))
			     (list tag
				   `(return-from ,block-tag
				      (apply #'(lambda ,bvl ,@body)
					     ,temp-var)))))
		       data)))))))
|#

(defmacro with-simple-restart ((restart-name format-string
					     &rest format-arguments)
			       &body forms)
  "(WITH-SIMPLE-RESTART (restart-name format-string format-arguments)
   body)
   If restart-name is not invoked, then all values returned by forms are
   returned.  If control is transferred to this restart, it immediately
   returns the values nil and t."
  `(restart-case (progn ,@forms)
     (,restart-name ()
        :report (lambda (stream)
		  (format stream ,format-string ,@format-arguments))
      (values nil t))))



;;;; Conditions.

(defstruct (condition (:constructor |constructor for condition|)
                      (:predicate nil)
                      (:print-function condition-print))
  )

(defun condition-print (condition stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<~S.~X>"
	      (type-of condition)
	      (system:%primitive lisp::make-fixnum condition))
      (condition-report condition stream)))


(eval-when (eval compile load)

(defmacro parent-type     (condition-type) `(get ,condition-type 'parent-type))
(defmacro slots           (condition-type) `(get ,condition-type 'slots))
(defmacro conc-name       (condition-type) `(get ,condition-type 'conc-name))
(defmacro report-function (condition-type)
  `(get ,condition-type 'report-function))
(defmacro make-function   (condition-type) `(get ,condition-type 'make-function))

) ;eval-when

(defun condition-report (condition stream)
  (do ((type (type-of condition) (parent-type type)))
      ((not type)
       (format stream "The condition ~A occurred." (type-of condition)))
    (let ((reporter (report-function type)))
      (when reporter
        (funcall reporter condition stream)
        (return nil)))))

(setf (make-function   'condition) '|constructor for condition|)

(defun make-condition (type &rest slot-initializations)
  "Makes a condition of type type using slot-initializations as initial values
   for the slots."
  (let ((fn (make-function type)))
    (cond ((not fn) (error 'simple-type-error
			   :datum type
			   :expected-type '(satisfies make-function)
			   :format-string "Not a condition type: ~S"
			   :format-arguments (list type)))
          (t (apply fn slot-initializations)))))


;;; Some utilities used at macro expansion time.
;;;
(eval-when (eval compile load)

(defmacro resolve-function (function expression resolver)
  `(cond ((and ,function ,expression)
          (cerror "Use only the :~A information."
                  "Only one of :~A and :~A is allowed."
                  ',function ',expression))
         (,expression (setq ,function ,resolver))))
         
(defun parse-new-and-used-slots (slots parent-type)
  (let ((new '()) (used '()))
    (dolist (slot slots)
      (if (slot-used-p (car slot) parent-type)
          (push slot used)
          (push slot new)))
    (values new used)))

(defun slot-used-p (slot-name type)
  (cond ((eq type 'condition) nil)
        ((not type) (error "The type ~S does not inherit from condition." type))
        ((assoc slot-name (slots type)))
        (t (slot-used-p slot-name (parent-type type)))))

) ;eval-when

(defmacro define-condition (name (parent-type) slot-specs &rest options)
  "(DEFINE-CONDITION name (parent-type)
   ( {slot-name | (slot-name) | (slot-name default-value)}*)
   options)"
  (let ((constructor (let ((*package* (find-package "CONDITIONS")))
		       ;; Bind for the INTERN and the FORMAT.
		       (intern (format nil "Constructor for ~S" name)))))
    (let ((slots (mapcar #'(lambda (slot-spec)
			     (if (atom slot-spec) (list slot-spec) slot-spec))
			 slot-specs)))
      (multiple-value-bind (new-slots used-slots)
          (parse-new-and-used-slots slots parent-type)
	(let ((conc-name-p     nil)
	      (conc-name       nil)
	      (report-function nil)
	      (documentation   nil))
	  (do ((o options (cdr o)))
	      ((null o))
	    (let ((option (car o)))
	      (case (car option) ;should be ecase
		(:conc-name
		 (setq conc-name-p t)
		 (setq conc-name (cadr option)))
		(:report
		 (setq report-function
		       (if (stringp (cadr option))
			   `(lambda (stream)
			      (write-string ,(cadr option) stream))
			   (cadr option))))
		(:documentation (setq documentation (cadr option)))
		(otherwise
		 (cerror "Ignore this DEFINE-CONDITION option."
			 "Invalid DEFINE-CONDITION option: ~S" option)))))
	  (unless conc-name-p
	    (setq conc-name
		  (intern (concatenate 'simple-string (symbol-name name)
				       "-")
			  *package*)))
	  ;; The following three forms are compile-time side-effects.  For now,
	  ;; they affect the global environment, but with modified abstractions
	  ;; for parent-type, slots, and conc-name, the compiler could easily
	  ;; make them local.
	  (setf (parent-type name) parent-type)
          (setf (slots name)       slots)
          (setf (conc-name name)   conc-name)
          ;; finally, the expansion ...
	  `(progn
	     (defstruct (,name
			 (:constructor ,constructor)
			 (:predicate nil)
			 (:copier nil)
			 (:print-function condition-print)
			 (:include ,parent-type ,@used-slots)
			 (:conc-name ,conc-name))
	       ,@new-slots)
	     (setf (documentation ',name 'type) ',documentation)
	     (setf (parent-type ',name) ',parent-type)
	     (setf (slots ',name) ',slots)
	     (setf (conc-name ',name) ',conc-name)
	     (setf (report-function ',name)
		   ,(if report-function `#',report-function))
	     (setf (make-function ',name) ',constructor)
	     ',name))))))



;;;; HANDLER-BIND and SIGNAL.

(defvar *handler-clusters* nil)

(defmacro handler-bind (bindings &body forms)
  "(HANDLER-BIND ( {(type handler)}* )  body)
   Executes body in a dynamic context where the given handler bindings are
   in effect.  Each handler must take the condition being signalled as an
   argument.  The bindings are searched first to last in the event of a
   signalled condition."
  (unless (every #'(lambda (x) (and (listp x) (= (length x) 2))) bindings)
    (error "Ill-formed handler bindings."))
  `(let ((*handler-clusters*
	  (cons (list ,@(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
				bindings))
		*handler-clusters*)))
     ,@forms))

(defvar *break-on-signals* nil
  "When (typep condition *break-on-signals*) is true, then calls to SIGNAL will
   enter the debugger prior to signalling that condition.")

(defun signal (datum &rest arguments)
  "Invokes the signal facility on a condition formed from datum and arguments.
   If the condition is not handled, nil is returned.  If
   (TYPEP condition *BREAK-ON-SIGNALS*) is true, the debugger is invoked before
   any signalling is done."
  (let ((condition (coerce-to-condition datum arguments
					'simple-condition 'signal))
        (*handler-clusters* *handler-clusters*))
    (when (typep condition *break-on-signals*)
      (break "~A~%Break entered because of *break-on-signals*."
	     condition))
    (loop
      (unless *handler-clusters* (return))
      (let ((cluster (pop *handler-clusters*)))
	(dolist (handler cluster)
	  (when (typep condition (car handler))
	    (funcall (cdr handler) condition)))))
    nil))

;;; COERCE-TO-CONDITION is used in SIGNAL, ERROR, CERROR, WARN, and
;;; INVOKE-DEBUGGER for parsing the hairy argument conventions into a single
;;; argument that's directly usable by all the other routines.
;;;
(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((typep datum 'condition)
	 (if arguments
	     (cerror "Ignore the additional arguments."
		     'simple-type-error
		     :datum arguments
		     :expected-type 'null
		     :format-string "You may not supply additional arguments ~
				     when giving ~S to ~S."
		     :format-arguments (list datum function-name)))
	 datum)
        ((symbolp datum) ;Roughly, (subtypep datum 'condition).
         (apply #'make-condition datum arguments))
        ((stringp datum)
	 (make-condition default-type
                         :format-string datum
                         :format-arguments arguments))
        (t
         (error 'simple-type-error
		:datum datum
		:expected-type '(or symbol string)
		:format-string "Bad argument to ~S: ~S"
		:format-arguments (list function-name datum)))))



;;;; INFINITE-ERROR-PROTECT.

(defvar *error-system-initialized*)
(defvar *max-error-depth* 10 "The maximum number of nested errors allowed.")
(defvar *current-error-depth* 0 "The current number of nested errors.")

;;; INFINITE-ERROR-PROTECT is used by ERROR and friends to keep us out of
;;; hyperspace.
;;;
(defmacro infinite-error-protect (form)
  `(if (and (boundp '*error-system-initialized*) (numberp *current-error-depth*))
       (let ((*current-error-depth* (1+ *current-error-depth*)))
	 (if (> *current-error-depth* *max-error-depth*)
	     (error-error "Help! " *current-error-depth* " nested errors.")
	     ,form))
       (system:%primitive lisp::halt)))

;;; These are used in ERROR-ERROR.
;;;
(defvar %error-error-depth% 0)
(defvar *error-throw-up-count* 0)

(proclaim '(special lisp::*real-terminal-io*))

;;; ERROR-ERROR can be called when the error system is in trouble and needs
;;; to punt fast.  Prints a message without using format.  If we get into
;;; this recursively, then halt.
;;;
(defun error-error (&rest messages)
  (let ((%error-error-depth% (1+ %error-error-depth%)))
    (when (> *error-throw-up-count* 50)
      (system:%primitive lisp::halt)
      (throw 'lisp::top-level-catcher nil))
    (case %error-error-depth%
      (1)
      (2
       (setq *terminal-io* lisp::*real-terminal-io*))
      (3
       (incf *error-throw-up-count*)
       (throw 'lisp::top-level-catcher nil))
      (t
       (system:%primitive lisp::halt)
       (throw 'lisp::top-level-catcher nil)))
    
    (dolist (item messages) (princ item *terminal-io*))
    (debug:internal-debug)))



;;;; Fetching errorful function name.

#+new-compiler
;;; FIND-NAME returns the name of a function if it is a subr or named-lambda.
;;; If the function is a regular lambda, the whole list is returned, and if
;;; the function can't be recognized, () is returned.
(defun find-name (function)
  (declare (ignore function))
  'hunoz)

;;; GET-CALLER returns a form that returns the function which called the
;;; currently active function.
(defmacro get-caller ()
  'nil)


;;;; ERROR, CERROR, BREAK, WARN.

(define-condition serious-condition (condition) ())

(define-condition error (serious-condition)
  ((function-name nil)))

#+new-compiler
(defun error (datum &rest arguments)
  "Invokes the signal facility on a condition formed from datum and arguments.
   If the condition is not handled, the debugger is invoked."
  (infinite-error-protect
   (let ((condition (coerce-to-condition datum arguments 'simple-error 'error)))
     (unless (error-function-name condition)
       (setf (error-function-name condition) (find-name (get-caller))))
     (signal condition)
     (invoke-debugger condition))))

#+new-compiler
;;; CERROR must take care to no use arguments when datum is already a condition
;;; object.  Furthermore, we must set ERROR-FUNCTION-NAME here instead of
;;; letting ERROR do it, so we get the correct function name.
;;;
(defun cerror (continue-string datum &rest arguments)
  (with-simple-restart
      (continue "~A" (apply #'format nil continue-string arguments))
    (let ((condition (if (typep datum 'condition)
			 datum
			 (coerce-to-condition datum arguments
					      'simple-error 'error))))
      (unless (error-function-name condition)
	(setf (error-function-name condition) (find-name (get-caller))))
      (error condition)))
  nil)

#+new-compiler
(defun break (&optional (format-string "Break") &rest format-arguments)
  "Prints a message and invokes the debugger without allowing any possibility
   of condition handling occurring."
  (with-simple-restart (continue "Return from BREAK.")
    (invoke-debugger
      (make-condition 'simple-condition
		      :format-string format-string
		      :format-arguments format-arguments)))
  nil)

(define-condition warning (condition) ())

(defvar *break-on-warnings* ()
  "If non-NIL, then WARN will enter a break loop before returning.")

#+new-compiler
(defun warn (datum &rest arguments)
  "Warns about a situation by signalling a condition formed by datum and
   arguments.  Before signalling, if *break-on-warnings* is set, then BREAK
   is called.  While the condition is being signaled, a muffle-warning restart
   exists that causes WARN to immediately return nil."
  (let ((condition (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (check-type condition warning "a warning condition")
    (if *break-on-warnings*
	(break "~A~%Break entered because of *break-on-warnings*."
	       condition))
    (restart-case (signal condition)
      (muffle-warning ()
	  :report "Skip warning."
	(return-from warn nil)))
    (format *error-output* "~&Warning:~%~A~%" condition)
    nil))



;;;; Condition definitions.

;;; Serious-condition and error are defined on the previous page, so ERROR and
;;; CERROR can SETF a slot in the error condition object.
;;;


(defun simple-condition-printer (condition stream)
  (apply #'format stream (simple-condition-format-string condition)
	 		 (simple-condition-format-arguments condition)))

;;; The simple-condition type has a conc-name, so SIMPLE-CONDITION-FORMAT-STRING
;;; and SIMPLE-CONDITION-FORMAT-ARGUMENTS could be written to handle the
;;; simple-condition, simple-warning, simple-type-error, and simple-error types.
;;; This seems to create some kind of bogus multiple inheritance that the user
;;; sees.
;;;
(define-condition simple-condition (condition)
  (format-string
   (format-arguments '()))
  (:conc-name internal-simple-condition-)
  (:report simple-condition-printer))

;;; The simple-warning type has a conc-name, so SIMPLE-CONDITION-FORMAT-STRING
;;; and SIMPLE-CONDITION-FORMAT-ARGUMENTS could be written to handle the
;;; simple-condition, simple-warning, simple-type-error, and simple-error types.
;;; This seems to create some kind of bogus multiple inheritance that the user
;;; sees.
;;;
(define-condition simple-warning (warning)
  (format-string
   (format-arguments '()))
  (:conc-name internal-simple-warning-)
  (:report simple-condition-printer))


(defun print-simple-error (condition stream)
  (format stream "~&Error in function ~S.~%~?"
	  (internal-simple-error-function-name condition)
	  (internal-simple-error-format-string condition)
	  (internal-simple-error-format-arguments condition)))

;;; The simple-error type has a conc-name, so SIMPLE-CONDITION-FORMAT-STRING
;;; and SIMPLE-CONDITION-FORMAT-ARGUMENTS could be written to handle the
;;; simple-condition, simple-warning, simple-type-error, and simple-error types.
;;; This seems to create some kind of bogus multiple inheritance that the user
;;; sees.
;;;
(define-condition simple-error (error)
  (format-string
   (format-arguments '()))
  (:conc-name internal-simple-error-)
  (:report print-simple-error))


(define-condition storage-condition (serious-condition) ())

(define-condition stack-overflow    (storage-condition) ())
(define-condition storage-exhausted (storage-condition) ())

(define-condition type-error (error)
  (datum
   expected-type))

;;; The simple-type-error type has a conc-name, so
;;; SIMPLE-CONDITION-FORMAT-STRING and SIMPLE-CONDITION-FORMAT-ARGUMENTS could
;;; be written to handle the simple-condition, simple-warning,
;;; simple-type-error, and simple-error types.  This seems to create some kind
;;; of bogus multiple inheritance that the user sees.
;;;
(define-condition simple-type-error (type-error)
  (format-string
   (format-arguments '()))
  (:conc-name internal-simple-type-error-)
  (:report simple-condition-printer))

(define-condition case-failure (type-error)
  (name
   possibilities)
  (:report
    (lambda (condition stream)
      (format stream "~S fell through ~S expression.~%Wanted one of ~:S."
	      (type-error-datum condition)
	      (case-failure-name condition)
	      (case-failure-possibilities condition)))))


;;; SIMPLE-CONDITION-FORMAT-STRING and SIMPLE-CONDITION-FORMAT-ARGUMENTS.
;;; These exist for the obvious types to seemingly give the impression of
;;; multiple inheritance.  That is, the last three types inherit from warning,
;;; type-error, and error while inheriting from simple-condition also.
;;;
(defun simple-condition-format-string (condition)
  (etypecase condition
    (simple-condition  (internal-simple-condition-format-string  condition))
    (simple-warning    (internal-simple-warning-format-string    condition))
    (simple-type-error (internal-simple-type-error-format-string condition))
    (simple-error      (internal-simple-error-format-string      condition))))
;;;
(defun simple-condition-format-arguments (condition)
  (etypecase condition
    (simple-condition  (internal-simple-condition-format-arguments  condition))
    (simple-warning    (internal-simple-warning-format-arguments    condition))
    (simple-type-error (internal-simple-type-error-format-arguments condition))
    (simple-error      (internal-simple-error-format-arguments      condition))))


(define-condition program-error (error) ())


(defun print-control-error (condition stream)
  (apply #'format
	 stream "~&Error in function ~S.~%~?"
	 (control-error-function-name condition)
	 (control-error-format-string condition)
	 (control-error-format-arguments condition)))

(define-condition control-error (error)
  (format-string
   (format-arguments nil))
  (:report print-control-error))


(define-condition stream-error (error) (stream))

(define-condition end-of-file (stream-error) ())

(define-condition file-error (error) (pathname))

(define-condition package-error (error) (pathname))

(define-condition cell-error (error) (name))

(define-condition unbound-variable (cell-error) ()
  (:report (lambda (condition stream)
	     (format stream "The variable ~S is unbound."
		     (cell-error-name condition)))))
  
(define-condition undefined-function (cell-error) ()
  (:report (lambda (condition stream)
	     (format stream "The function ~S is undefined."
		     (cell-error-name condition)))))

(define-condition arithmetic-error (error) (operation operands))

(define-condition division-by-zero         (arithmetic-error) ())
(define-condition floating-point-overflow  (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())



;;;; HANDLER-CASE and IGNORE-ERRORS.

(defmacro handler-case (form &rest cases)
  "(HANDLER-CASE form
   { (type ([var]) body) }* )
   Executes form in a context with handlers established for the condition
   types.  A peculiar property allows type to be :no-error.  If such a clause
   occurs, and form returns normally, all its values are passed to this clause
   as if by MULTIPLE-VALUE-CALL.  The :no-error clause accepts more than one
   var specification."
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (make-symbol "normal-return"))
	      (error-return  (make-symbol "error-return")))
	  `(block ,error-return
	     (multiple-value-call #'(lambda ,@(cdr no-error-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let ((var (gensym))
	      (outer-tag (gensym))
	      (inner-tag (gensym))
	      (tag-var (gensym))
	      (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
				       cases)))
	  `(let ((,outer-tag (cons nil nil))
		 (,inner-tag (cons nil nil))
		 ,var ,tag-var)
	     ,var			;ignoreable
	     (catch ,outer-tag
	       (catch ,inner-tag
		 (throw ,outer-tag
			(handler-bind
			    ,(mapcar #'(lambda (annotated-case)
					 `(,(cadr annotated-case)
					   #'(lambda (temp)
					       ,(if (caddr annotated-case)
						    `(setq ,var temp)
						    '(declare (ignore temp)))
					       (setf ,tag-var
						     ',(car annotated-case))
					       (throw ,inner-tag nil))))
				     annotated-cases)
			  ,form)))
	       (case ,tag-var
		 ,@(mapcar #'(lambda (annotated-case)
			       (let ((body (cdddr annotated-case))
				     (varp (caddr annotated-case)))
				 `(,(car annotated-case)
				   ,@(if varp
					 `((let ((,(car varp) ,var))
					     ,@body))
					 body))))
			   annotated-cases))))))))
#|
This macro doesn't work in our system due to lossage in closing over tags.
The previous version sets up unique run-time tags.

(defmacro handler-case (form &rest cases)
  "(HANDLER-CASE form
   { (type ([var]) body) }* )
   Executes form in a context with handlers established for the condition
   types.  A peculiar property allows type to be :no-error.  If such a clause
   occurs, and form returns normally, all its values are passed to this clause
   as if by MULTIPLE-VALUE-CALL.  The :no-error clause accepts more than one
   var specification."
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (make-symbol "normal-return"))
	      (error-return  (make-symbol "error-return")))
	  `(block ,error-return
	     (multiple-value-call #'(lambda ,@(cdr no-error-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let ((tag (gensym))
	      (var (gensym))
	      (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
				       cases)))
	  `(block ,tag
	     (let ((,var nil))
	       ,var				;ignorable
	       (tagbody
		 (handler-bind
		  ,(mapcar #'(lambda (annotated-case)
			       (list (cadr annotated-case)
				     `#'(lambda (temp)
					  ,(if (caddr annotated-case)
					       `(setq ,var temp)
					       '(declare (ignore temp)))
					  (go ,(car annotated-case)))))
			   annotated-cases)
			       (return-from ,tag ,form))
		 ,@(mapcan
		    #'(lambda (annotated-case)
			(list (car annotated-case)
			      (let ((body (cdddr annotated-case)))
				`(return-from
				  ,tag
				  ,(cond ((caddr annotated-case)
					  `(let ((,(caaddr annotated-case)
						  ,var))
					     ,@body))
					 ((not (cdr body))
					  (car body))
					 (t
					  `(progn ,@body)))))))
			   annotated-cases))))))))
|#

(defmacro ignore-errors (&rest forms)
  "Executes forms after establishing a handler for all error conditions that
   returns from this form nil and the condition signalled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))



;;;; Restart definitions.

(define-condition abort-failure (control-error) ()
  (:report
   "Found an \"abort\" restart that failed to transfer control dynamically."))

;;; ABORT signals an error in case there was a restart named abort that did
;;; not tranfer control dynamically.  This could happen with RESTART-BIND.
;;;
(defun abort ()
  "Transfers control to a restart named abort, signalling a control-error if
   none exists."
  (invoke-restart 'abort)
  (error 'abort-failure))


(defun muffle-warning ()
  "Transfers control to a restart named muffle-warning, signalling a
   control-error if none exists."
  (invoke-restart 'muffle-warning))


;;; DEFINE-NIL-RETURNING-RESTART finds the restart before invoking it to keep
;;; INVOKE-RESTART from signalling a control-error condition.
;;;
(defmacro define-nil-returning-restart (name args doc)
  `(defun ,name ,args
     ,doc
     (if (find-restart ',name) (invoke-restart ',name ,@args))))

(define-nil-returning-restart continue ()
  "Transfer control to a restart named continue, returning nil if none exists.")

(define-nil-returning-restart store-value (value)
  "Transfer control and value to a restart named store-value, returning nil if
   none exists.")

(define-nil-returning-restart use-value (value)
  "Transfer control and value to a restart named use-value, returning nil if
   none exists.")




;;;; Internal Error Codes.

;;; *Internal-error-table* contains a vector, by error code, of functions.
;;; This is used in %SP-INTERNAL-ERROR, and initialized MAKE-ERROR-TABLE.
;;;
(defvar *internal-error-table*)

#+new-compiler
;;; %SP-INTERNAL-ERROR is called by the microcode when an internal error
;;; occurrs.  It is simply a dispatch routine which looks up a specialized
;;; function to call in the special variable, *internal-error-table*.
;;;
;;; ERR-CODE -- a fixnum which identifies the specific error.
;;; PC -- the relative offset of the NEXT macro instruction to be
;;;        executed in the code vector of the errorful function.
;;; ARG3 & ARG4 -- arbitrary meaning determined by ERR-CODE.
;;;
(defun lisp::%sp-internal-error (err-code arg3 arg4)
  (infinite-error-protect
   (funcall (svref *internal-error-table* err-code)
	    (find-name (get-caller))
	    0
	    arg3
	    arg4)))

;;; DEF-INTERNAL-ERROR defines a form which can be put into the system init
;;; file (spinit, or vaxinit) to define the errors which the microcode may
;;; signal.  The form looks like
;;;
;;; (def-internal-error err-code condition flag control-string &rest args)
;;;  ERR-CODE -- the internal code for this error. less than or equal to
;;;               max-internal-error which is declared in the init file.
;;;  CONDITION -- the name of the error to signal
;;;  FLAG -- one of CORRECTABLE, FATAL or SYSTEM-ERROR.  (not evaluated)
;;;           if CORRECTABLE, %sp-internal-error may return correction values
;;;           if SYSTEM-ERROR, the CONDITION arg is ignored.
;;;  CONTROL-STRING -- the error message as a format control string.
;;;  ARGS -- The args to the control string.  The 3rd & 4th args to
;;;           %sp-internal-error are available as the variables ARG3 & ARG4.
;;;
;;; NOTE: system-error is never supplied, and condition is never used.  Maybe
;;;       it will be when we signal appropriate conditions for certain
;;;       situations.
;;;

;example
;  (def-internal-error 6 :unbound-symbol correctable
;    "Unbound symbol: ~s." arg3)


(defmacro def-internal-error (number condition flag control-string &rest args)
  (declare (ignore condition))
  `(setf (svref *internal-error-table* ,number)
	 #'(lambda (callers-name PC arg3 arg4)
	     (declare (ignore ,@(unless (eq flag 'system-error) '(PC))
			      ,@(unless (member 'arg3 args) '(arg3))
			      ,@(unless (member 'arg4 args) '(arg4))))
	     ,(case flag
		((fatal) `(error 'simple-error
				 :function-name callers-name
				 :format-string ,control-string
				 :format-arguments (list ,@args)))
		((correctable) `(cerror 'simple-error
					:function-name callers-name
					:format-string ,control-string
					:format-arguments (list ,@args)))))))


(defconstant max-internal-error 100
  "The largest internal error number for Spice Lisp.")

(proclaim '(special allocation-space))


(defun make-error-table ()
  (setq *internal-error-table*
	(make-array (1+ max-internal-error)
		    :initial-element
		    #'(lambda (&rest ignore)
			(declare (ignore ignore))
			(break "Undefined Error."))))

  (def-internal-error 1 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'List)
  (def-internal-error 2 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'Symbol)
  (def-internal-error 3 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'Number)
  (def-internal-error 4 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'Integer)
  (def-internal-error 5 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'Ratio)
  (def-internal-error 6 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'Complex)
  (def-internal-error 7 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'Vector-like)
  (def-internal-error 8 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'simple-vector)
  (def-internal-error 9 :invalid-function fatal
    "Invalid function: ~s." arg3)
  (def-internal-error 10 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been a function or an array." arg3)
  (def-internal-error 11 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'U-vector-like)
  (def-internal-error 12 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s."
    arg3 'simple-bit-vector)
  (def-internal-error 13 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3
    'simple-string)
  (def-internal-error 14 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'character)
  (def-internal-error 15 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s."
    arg3 'Control-Stack-Pointer)
  (def-internal-error 16 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s."
    arg3 'Binding-Stack-Pointer)
  (def-internal-error 17 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'Array)
  (def-internal-error 18 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s."
    arg3 'Positive-Fixnum)
  (def-internal-error 19 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'SAP-pointer)
  (def-internal-error 20 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of tyep ~s." arg3 'system-pointer)
  (def-internal-error 21 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'float)
  (def-internal-error 22 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'rational)
  (def-internal-error 23 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been a non-complex number." arg3)

  (def-internal-error 25 :unbound-variable fatal
    "Unbound variable: ~s." arg3)
  (def-internal-error 26 :undefined-function fatal
    "Undefined function: ~s." arg3)
  (def-internal-error 27 :error fatal
    "Attempt to alter NIL.")
  (def-internal-error 28 :error fatal
    "Attempt to alter NIL.")
  (def-internal-error 29 :error fatal
    "Circularity detected in chain of symbols from definition cell of symbol ~S."
    arg3)

  ;;Special because handlers won't work while allocation-space is wrong.
  (setf (svref *internal-error-table* 24)
	#'(lambda (callers-name ignore0 ignore1 ignore2)
	    (declare (ignore ignore0 ignore1 ignore2))
	    (let ((bazfaz allocation-space))
	      (setq allocation-space 0)
	      (error 'simple-error
		     :function-name callers-name
		     :format-string "Illegal allocation-space value: ~S."
		     :format-arguments (list bazfaz)))))

  (def-internal-error 30 :error fatal
    "Illegal u-vector access type: ~s." arg3)
  (def-internal-error 31 :error fatal
    "Illegal vector length: ~s." arg3)
  (def-internal-error 32 :error fatal
    "Vector index, ~s, out of bounds." arg3)
  (def-internal-error 33 :error fatal
    "Illegal index: ~s." arg3)
  (def-internal-error 34 :error fatal
    "Illegal shrink value: ~s." arg3)
  (def-internal-error 35 :error fatal
    "Shrink value, ~s, is greater than current length of ~s." arg3 arg4)
  (def-internal-error 36 :error fatal
    "Illegal data vector, ~S, in an array." arg3)
  (def-internal-error 37 :error fatal
    "Too few arguments passed to two or three dimension array access miscop.")
  (def-internal-error 38 :error fatal
    "Too many arguments passed to two or three dimension array access miscop.")
  (def-internal-error 39 :error fatal
    "Illegal to allocate vector of size: ~s." arg3)

  (def-internal-error 40 :error fatal
    "Illegal byte pointer: (byte ~s ~s)." arg3 arg4)
  (def-internal-error 41 :error fatal
    "Illegal position, ~s, in byte spec." arg3)
  (def-internal-error 42 :error fatal
    "Illegal size, ~s, in byte spec." arg3)
  (def-internal-error 43 :error fatal
    "Illegal shift count: ~s." arg3)
  (def-internal-error 44 :error fatal
    "Illegal boole operation: ~s." arg3)

  (def-internal-error 50 :error fatal "Wrong number of arguments: ~D." arg3)

  (def-internal-error 55 :error fatal
    "~s is not <= to ~s (Alien index out of bounds.)" arg3 arg4)

  (def-internal-error 60 :error fatal
    "Attempt to divide ~s by ~s." arg3 arg4)
  (def-internal-error 61 :unseen-throw-tag fatal
    "No catcher for throw tag ~s." arg3)
  (def-internal-error 62 :error fatal
    "Something using ~S and ~S lead to a short-float underflow." arg3 arg4)
  (def-internal-error 63 :error fatal
    "Something using ~S and ~S lead to a short-float overflow." arg3 arg4)
#|
  (def-internal-error 64 :error fatal
    "Something using ~S and ~S lead to a single-float underflow." arg3 arg4)
  (def-internal-error 65 :error fatal
    "Something using ~S and ~S lead to a single-float overflow." arg3 arg4)
|#
  (def-internal-error 66 :error fatal
    "Something using ~S and ~S lead to a long-float underflow." arg3 arg4)
  (def-internal-error 67 :error fatal
    "Something using ~S and ~S lead to a long-float overflow." arg3 arg4)
  (def-internal-error 68 :error fatal
    "Something using ~S caused a short-float underflow." arg3)
  (def-internal-error 69 :error fatal
    "Something using ~S caused a short-float overflow." arg3)
  (def-internal-error 70 :error fatal
    "Something using ~S caused a long-float underflow." arg3)
  (def-internal-error 71 :error fatal
    "Something using ~S caused a long-float overflow." arg3)
  (def-internal-error 72 :error fatal
    "~S is not a legal argument to log, it should be non-zero." arg3)
  (def-internal-error 73 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'string-char)
  (def-internal-error 74 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'short-float)
  (def-internal-error 75 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'long-float)
  (def-internal-error 76 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'fixnum)
  (def-internal-error 77 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 'cons)
  (def-internal-error 78 :error fatal
    "Invalid exit.")
  (def-internal-error 79 :error fatal
    "Odd number of arguments in keyword part of argument list.")
  (def-internal-error 80 :error fatal
    "~S is not a known keyword argument specifier." arg3)
  (def-internal-error 81 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3 arg4)
  (def-internal-error 82 :wrong-type-argument fatal
    "Wrong type argument, ~s, should have been of type ~s." arg3
    '(or function symbol))
  (def-internal-error 83 :error fatal
    "~S is not = to ~S (Alien index out of bounds.)" arg3 arg4)
  
  )



;;; ERROR-INIT is called at init time to initialize the error system.
;;; It initializes the internal error table, and sets a variable.
;;;
(defun error-init ()
  (make-error-table)
  (setq *error-system-initialized* t))

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* nil))
