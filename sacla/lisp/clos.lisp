;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: clos.lisp,v 1.28 2004/09/24 07:31:33 yuji Exp $
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

;; Non conformance points to CLOS.
;; * (defmethod allocate-instance ((class structure-class) &rest initargs))
;;   is not implemented.
;;        
;; Non conformance points to MOP.
;; * 
;;


(defparameter *mop-working-p* nil)
(defparameter *symbol-function-is-funcallable-object-p* t)



(defstruct %standard-object
  class
  (version nil)
  storage)

(defun swap-%standard-object (a b)
  (rotatef (%standard-object-class   a) (%standard-object-class   b))
  (rotatef (%standard-object-version a) (%standard-object-version b))
  (rotatef (%standard-object-storage a) (%standard-object-storage b)))



(defparameter *classes* (make-hash-table)
  "The hash table of all classes in CLOS system.")
(defun find-class (symbol &optional errorp environment) ; clos
  "Return the class object named by SYMBOL in ENVIRONMENT."
  (declare (ignore environment))
  (multiple-value-bind (class presentp) (gethash symbol *classes*)
    (if presentp
        class
        (when errorp (error "Class ~S does not exist." symbol)))))
(defun (setf find-class) (new-class symbol &optional errorp environment) ; clos
  "Set the name of NEW-CLASS to SYMBOL in ENVIRONMENT."
  (declare (ignore environment))
  (if new-class
      (setf (gethash symbol *classes*) new-class)
      (remhash symbol *classes*))
  new-class)



(defun plist-member (key plist)
  (loop for rest on plist by #'cddr
        if (eq key (car rest)) return rest))

(defun plist-keys (plist) (loop for key in plist by #'cddr collect key))



;; AMOP 5.4.2 The defclass Macro http://www.lisp.org/mop/concepts.html#defclass
;; > The relationship between the arguments to the defclass macro and
;; > the arguments received by ensure-generic-function is defined.

(defun qt? (object)
  "Return a quoted form of OBJECT or OBJECT itself if it's self-evaluating."
  (flet ((self-evaluating-object-p (object)
           (typecase object
             ((or number character array) t)
             (symbol (or (keywordp object) (member object '(t nil))))
             (t nil))))
    (if (self-evaluating-object-p object)
        object
        `(quote ,object))))

(defun check-direct-slot-spec-options (slot-spec)
  (let ((reserved-keys '(:name :initargs :initfunction :readers :writers))
        (multiple-keys '(:initarg :reader :writer :accessor))
        (keys '())
        (name (car slot-spec)))
    (dolist (key (plist-keys (cdr slot-spec)))
      (cond
        ((member key reserved-keys)
         (error "Reserved option ~S is wrongly given in slot ~S ." key name))
        ((and (member key keys) (not (member key multiple-keys)))
         (error "Option ~S is given more than once in slot ~S." key name))
        (t (push key keys))))))

(defun direct-slot-initargs-form (slot-spec)
  ;; direct-slot-initargs is a canonicalized slot specification described in
  ;; http://www.lisp.org/mop/concepts.html#defclass .
  (flet ((specified-more-than-once-p (key)
           (< 1 (count key (plist-keys (cdr slot-spec)))))
         (quote-keys (plist)
           (loop for (key value) on plist by #'cddr nconc `(,(qt? key) ,value))))
    (check-direct-slot-spec-options slot-spec)
    (loop for (key value) on (cdr slot-spec) by #'cddr
          if (eq key :initform)
            nconc `(:initform ',value :initfunction #'(lambda () ,value))
            into plist
          else if (eq key :initarg)  collect value          into initargs
          else if (eq key :reader)   collect value          into readers
          else if (eq key :writer)   collect value          into writers
          else if (eq key :accessor) collect value          into readers and
                                     collect `(setf ,value) into writers
          else if (specified-more-than-once-p key)
            if (getf plist key)
              do (setf (cdr (last (getf plist key))) `((quote ,value)))
            else
              nconc `(,key (list ',value)) into plist end
          else nconc `(,key ',value) into plist
          finally
          (return `(list :name ',(car slot-spec) :initargs ',initargs
                    :readers ',readers :writers ',writers
                    ,@(quote-keys `(,@plist :initform 'nil :initfunction 'nil
                                    :documentation 'nil)))))))

(defun class-initargs-form (options)
  ;; See http://www.lisp.org/mop/concepts.html#defclass
  (labels ((check-options (alist)
             (let ((reserved-keys '(:name :direct-default-initargs))
                   (keys '()))
               (dolist (key (mapcar #'first alist))
                 (cond
                   ((member key reserved-keys)
                    (error "Reserved option ~S is wrongly given." key))
                   ((member key keys)
                    (error "Option ~S is given more than once." key))
                   (t (push key keys))))))
           (direct-default-initargs-form (initargs)
               (loop for (key form) on initargs by #'cddr
                     collect `(list ',key ',form #'(lambda () ,form)) into result
                     finally (return `(list ,@result))))
           (value-form (key rest)
             (case key
               (:direct-default-initargs (direct-default-initargs-form rest))
               (:direct-slots `(list ,@(mapcar #'direct-slot-initargs-form rest)))
               ((:metaclass :documentation)
                (destructuring-bind (value) rest (qt? value)))
               (t (qt? rest)))))
    (check-options options)
    (loop for (key . rest) in options
          when (eq key :default-initargs) do (setq key :direct-default-initargs)
          nconc `(,(qt? key) ,(value-form key rest)) into result
          finally (return `(list ,@result)))))

(defun <ensure-class> (name &rest initargs)
  (apply (if *mop-working-p* #'ensure-class #'ensure-system-class)
         name initargs))

(defmacro defclass (name direct-superclasses direct-slots &rest options) ; clos
  "Define a new class named NAME which is a subclass of DIRECT-SUPERCLASSES."
  (let* ((*message-prefix* (format nil "DEFCLASS ~S: " name)))
    `(let ((*message-prefix* ,*message-prefix*))
      (apply #'<ensure-class>
       ',name
       ,(class-initargs-form `((:direct-superclasses ,@direct-superclasses)
                               (:direct-slots ,@direct-slots)
                               ,@options))))))



(defun class-of (object)                ; clos
  "Return the class of which OBJECT is a direct instance."
  ;; 4.3.7 Integrating Types and Classes
  ;; http://www.lispworks.com/reference/HyperSpec/Body/04_cg.htm
  (cond
    ((%standard-object-p object) (%standard-object-class object))
    ((functionp object) (let ((gf (find-generic-function object)))
                          (if gf
                              (class-of gf)
                              (find-class 'function))))
    (t (let ((type (type-of object)))
         (or (and (symbolp type) (find-class type))
             (typecase object
               ((and symbol (not null)) (find-class 'symbol))
               (character (find-class 'character))
               (hash-table (find-class 'hash-table))
               (sequence (class-of-sequence object))
               ((and array (not vector)) (find-class 'array))
               (number (class-of-number object))
               (stream (class-of-stream object))
               (pathname (typecase object
                           (logical-pathname (find-class 'logical-pathname))
                           (t (find-class 'pathname))))
               (package (find-class 'package))
               (random-state (find-class 'random-state))
               (readtable (find-class 'readtable))
               (restart (find-class 'restart))
               (condition (class-of-condition object))
               (t (find-class 't))))))))

(defun frozen-class-instance-p (object)
  (and (%standard-object-p object) (null (%standard-object-version object))))

(defconstant +unbound-state+ (gensym "UNBOUND-STATE-"))

(defun slot-value (object slot-name)    ; clos
  "Return the value of the slot named SLOT-NAME in OBJECT."
  (flet ((slot-is-missing ()
           (slot-missing (class-of object) object slot-name 'slot-value))
         (slot-is-unbound ()
           (slot-unbound (class-of object) object slot-name)))
    (if (frozen-class-instance-p object)
        (let ((binding (assoc slot-name (%standard-object-storage object))))
          (if binding
              (let ((value (second binding)))
                (if (not (eq value +unbound-state+))
                    value
                    (values (slot-is-unbound))))
              (values (slot-is-missing))))
        (let* ((class (class-of object))
               (slot (find-slot class slot-name)))
          (if slot
              (slot-value-using-class class object slot)
              (values (slot-is-missing)))))))

(defun (setf slot-value) (new-value object slot-name) ; clos
  "Set the value of the slot named SLOT-NAME in OBJECT to NEW-VALUE."
  (flet ((slot-is-missing ()
           (slot-missing (class-of object) object slot-name 'setf new-value)))
    (if (frozen-class-instance-p object)
        (let ((binding (assoc slot-name (%standard-object-storage object))))
          (if binding
              (progn
                (assert (typep new-value (third binding)))
                (setf (second binding) new-value))
              (slot-is-missing)))
        (let* ((class (class-of object))
               (slot (find-slot class slot-name)))
          (assert (typep new-value (slot-definition-type slot)))
          (if slot
              (setf (slot-value-using-class class object slot) new-value)
              (slot-is-missing))))
    new-value))

(defun slot-boundp (instance slot-name) ; clos
  "Return true if the slot named SLOT-NAME in INSTANCE is bound."
  (flet ((slot-is-missing ()
           (slot-missing (class-of instance) instance slot-name 'slot-value)))
    (if (frozen-class-instance-p instance)
        (let ((binding (assoc slot-name (%standard-object-storage instance))))
          (if binding
              (not (eq (second binding) +unbound-state+))
              (values (slot-is-missing))))
        (let* ((class (class-of instance))
               (slot (find-slot class slot-name)))
          (if slot
              (slot-boundp-using-class class instance slot)
              (values (slot-is-missing)))))))
(defun find-slot (class slot-name)
  (find slot-name (slot-value class 'slots)
        :key #'(lambda (slot) (slot-value slot 'name))))
(defun local-slot-p (slot) (eq (slot-value slot 'allocation) :instance))



;; for debug
(defmethod print-object ((object %standard-object) stream)
  (flet ((slot (object name)
           (second (assoc name (%standard-object-storage object)))))
    (let ((class (class-of object)))
      (cond
        ((and (%standard-object-p class)
              (assoc 'name (%standard-object-storage class)))
         (if (assoc 'name (%standard-object-storage object))
             (format stream "#<~S:(~S)>" (slot class 'name) (slot object 'name))
             (format stream "#<~S:>" (slot class 'name))))
        ((assoc 'name (%standard-object-storage object))
         (format stream "#<:(~S)>" (slot object 'name)))
        (t
         (format stream "#<(CLOS object)>"))))))



;; 7.1.3 Defaulting of Initialization Arguments
;;         http://www.lispworks.com/reference/HyperSpec/Body/07_ac.htm
;; 7.1.4 Rules for Initialization Arguments
;;         http://www.lispworks.com/reference/HyperSpec/Body/07_ad.htm
(defun compute-standard-default-initargs (class)
  (flet ((default-initargs (class)
           (mapappend #'(lambda (class)
                          (slot-value class 'direct-default-initargs))
                      (slot-value class 'precedence-list))))
    (loop for initarg in (default-initargs class) with args = '()
          for name = (car initarg)
          unless (member name args)
            collect initarg and do (push name args))))

(defun defaulted-initargs (class initargs)
  (append initargs
          (loop for (key form func) in (slot-value class 'default-initargs)
                ;; each key is unique throughout the iteration.
                unless (plist-member key initargs)
                  nconc `(,key ,(funcall func)))))



(defun finalized-p (class)
  (flet ((defclassed-p (class)
             (and (%standard-object-p class) (%standard-object-storage class))))
    (when (symbolp class) (setq class (find-class class)))
    (and (defclassed-p class) (slot-value class 'finalized-p))))

(defun canonicalize-instance (instance)
  (let* ((class (class-of instance))
         (initargs (mapcan #'(lambda (binding)
                               (list (%keyword (first binding)) (second binding)))
                           (%standard-object-storage instance)))
         (defaulted-initargs (defaulted-initargs class initargs))
         (dummy (make-%standard-object
                 :class class
                 :storage (allocate-standard-instance-storage class))))
    (standard-shared-initialize dummy t defaulted-initargs)
    (swap-%standard-object instance dummy))
  instance)

(defun make-system-instance (class &rest initargs)
  (flet ((alist-storage (plist)
           (loop for (key value) on plist by #'cddr
                 collect (list (intern (string key)) value 't))))
    (let ((instance (make-%standard-object
                     :class class
                     :version nil
                     :storage (alist-storage initargs))))
      (when (finalized-p class)
        (canonicalize-instance instance))
      instance)))

(defun ensure-class-object (name &rest initargs)
  (let ((metaclass (let ((metaclass-name (getf initargs :metaclass)))
                     (when metaclass-name
                       (ensure-class-object metaclass-name))))
        (class (find-class name)))
    (if class
        (when initargs
          (let* ((dummy (apply #'make-system-instance metaclass initargs)))
            (swap-%standard-object class dummy)))
        (setf (find-class name)
              (apply #'make-system-instance metaclass initargs)))
    (find-class name)))



(defun allocate-standard-instance-storage (class)
  (let* ((slots (slot-value class 'slots))
         (local-slots (sort (mapcan #'(lambda (slot)
                                        (when (local-slot-p slot)
                                          (list slot)))
                                    slots)
                            #'<
                            :key #'(lambda (slot) (slot-value slot 'location))))
         (shared-slots (mapcan #'(lambda (slot)
                                   (when (not (local-slot-p slot))
                                     (list slot)))
                               slots)))
    (if (frozen-class-instance-p class)
        (let ((local-alist (loop for slot in local-slots
                                 for name = (slot-value slot 'name)
                                 for type = (slot-value slot 'type)
                                 collect `(,name ,+unbound-state+ ,type)))
              (shared-alist (loop for slot in shared-slots
                                  for name = (slot-value slot 'name)
                                  for binding = (slot-value slot 'shared-binding)
                                  collect (cons name binding))))
          (nconc local-alist shared-alist))
        (make-array (length local-slots) :initial-element +unbound-state+))))

(defun class-slot-names (class)
  (mapcar #'(lambda (slot) (slot-value slot 'name)) (slot-value class 'slots)))

(defun standard-shared-initialize (instance slot-names initargs)
  (when (eq slot-names 't)
    (setq slot-names (class-slot-names (class-of instance))))
  (mapc #'(lambda (slot)
            (let ((name (slot-value slot 'name)))
              (multiple-value-bind (key value tail)
                  (get-properties initargs (slot-value slot 'initargs))
                (declare (ignore key))
                (if tail
                    (setf (slot-value instance name) value)
                    (when (and (member name slot-names)
                               (not (slot-boundp instance name))
                               (slot-value slot 'initfunction))
                      (setf (slot-value instance name)
                            (funcall (slot-value slot 'initfunction))))))))
        (slot-value (class-of instance) 'slots)))



;; 4.3.5 Determining the Class Precedence List
;; http://www.lispworks.com/reference/HyperSpec/Body/04_ce.htm
(defun topological-sort (classes pairs)
  (do (next
       (unordered classes (remove next unordered))
       (pairs pairs (remove-if #'(lambda (pair) (eq (first pair) next)) pairs))
       (result nil (cons next result)))
      ((null unordered) (nreverse result))
    (setq next (let ((candidates (remove-if #'(lambda (class)
                                                (find class pairs :key #'second))
                                            unordered)))
                 (assert (not (null candidates)))
                 (if (endp (cdr candidates))
                     (car candidates)
                     (block picker
                       (dolist (class result)
                         (dolist (super (slot-value class 'direct-superclasses))
                           (when (find super candidates)
                             (return-from picker super))))))))))


(defun compute-standard-class-precedence-list (class)
  (labels ((direct-supers (class) (slot-value class 'direct-superclasses))
           (superclasses (class)
             (labels ((supers (class)
                        (unless (eq class (find-class 't 'errorp))
                          (let ((directs (direct-supers class)))
                            (append directs (mapappend #'supers directs))))))
               (remove-duplicates (supers class))))
           (local-precedence-order-pairs (class)
             (unless (eq class (find-class 't 'errorp))
               (let ((directs (direct-supers class)))
                 (mapcar #'list (cons class directs) directs))))
           (pairs (classes)
             (delete-duplicates (mapcan #'local-precedence-order-pairs classes)
                                :test #'equal)))
    (let ((classes (cons class (superclasses class))))
      (topological-sort classes (pairs classes)))))

(defun class-precedence-names (class)   ; for debug
  (when (symbolp class) (setq class (find-class class 'errorp)))
  (mapcar #'(lambda (class) (slot-value class 'name))
          (slot-value class 'precedence-list)))



;; 7.5.3 Inheritance of Slots and Slot Options
;; http://www.lispworks.com/reference/HyperSpec/Body/07_ec.htm
(defun and-types (types)
  (labels ((type-eq (a b) (and (subtypep a b) (subtypep b a)))
           (proper-subtype-p (subtype type)
             (and (not (type-eq subtype type)) (subtypep subtype type))))
    (let ((types (mapcan #'(lambda (type)
                             (when (notany #'(lambda (t1)
                                               (proper-subtype-p t1 type))
                                           types)
                               (list type)))
                         (remove-duplicates types :test #'type-eq))))
      (if (endp (cdr types))
          (car types)
          `(and ,@types)))))

(defun effective-slot-initargs (class name direct-slots)
  (declare (ignorable class))
  (assert (not (null direct-slots)))
  (let* ((most-specific-slot (first direct-slots))
         (allocation (slot-value most-specific-slot 'allocation))
         (type (and-types (mapcar #'(lambda (slot) (slot-value slot 'type))
                                  direct-slots)))
         (shared-binding (slot-value most-specific-slot 'shared-binding)))
    `(:name ,name
      :allocation ,allocation
      ,@(or (loop for slot in direct-slots
                  for initfunction = (slot-value slot 'initfunction)
                  if initfunction
                  return `(:initform     ,(slot-value slot 'initform)
                           :initfunction ,(slot-value slot 'initfunction)))
            '(:initform nil :initfunction nil))
      :type ,type
      :initargs ,(remove-duplicates
                  (mapappend #'(lambda (slot)
                                 (slot-value slot 'initargs))
                             direct-slots))
      :documentation ,(loop for slot in direct-slots
                            for documentation = (slot-value slot 'documentation)
                            if documentation return it)
      :location nil
      :shared-binding ,(prog1 shared-binding
                              (when shared-binding
                                (setf (cdr shared-binding) (list type)))))))

(defun effective-slot-specs (class)
  ;; effective-slot-spec::= (name ([[direct-slot-object*]]))
  (let* ((direct-slots (mapappend #'(lambda (class)
                                      (slot-value class 'direct-slots))
                                  (slot-value class 'precedence-list)))
         (names (remove-duplicates (mapcar #'(lambda (slot)
                                               (slot-value slot 'name))
                                           direct-slots))))
    (mapcar #'(lambda (name)
                (list name (loop for slot in direct-slots
                                 if (eq name (slot-value slot 'name))
                                 collect slot)))
            names)))

(defun standard-direct-slot-definition (slot-spec)
  (setq slot-spec `(:shared-binding ,(when (eq :class
                                               (getf slot-spec :allocation))
                                           (list +unbound-state+))
                    ,@slot-spec :allocation :instance :type t))
  (let ((class (ensure-class-object 'standard-direct-slot-definition)))
    (apply #'make-system-instance class slot-spec)))

(defun assign-slots-locations (slots)
  (loop for slot in slots with location = 0
        if (local-slot-p slot)
        do (setf (slot-value slot 'location) location)
           (incf location))
  slots)

(defun compute-system-class-slots (class)
  (let* ((slot-class (ensure-class-object 'standard-effective-slot-definition))
         (slots (mapcar #'(lambda (initargs)
                            (apply #'make-system-instance slot-class initargs))
                        (mapcar #'(lambda (spec)
                                    (apply #'effective-slot-initargs class spec))
                                (effective-slot-specs class)))))
    (assign-slots-locations slots)
    slots))

(defconstant +funcallable-instance-function-slot-name+
  (gensym "FUNCALLABLE-INSTANCE-FUNCTION-SLOT-NAME-"))
(defconstant +funcallable-instance-function-slot-spec+
  (let ((name +funcallable-instance-function-slot-name+))
    (eval (direct-slot-initargs-form `(,name :initarg ,name)))))

(defun canonicalize-system-class-initargs (name &rest initargs &key
                                           (metaclass 'standard-class)
                                           direct-slots direct-superclasses
                                           &allow-other-keys)
  (when (eq metaclass 'funcallable-standard-class)
    (push +funcallable-instance-function-slot-spec+ direct-slots))
  (assert (not (null direct-superclasses)))
  (setq direct-superclasses (mapcar #'ensure-class-object direct-superclasses)
        direct-slots (mapcar #'standard-direct-slot-definition direct-slots))
  `(:name ,name :version nil :slots nil :precedence-list nil
    :default-initargs nil :finalized-p nil :direct-subclasses nil
    :direct-superclasses ,direct-superclasses :direct-slots ,direct-slots
    :direct-default-initargs nil :metaclass ,metaclass ,@initargs))

(defun subclassp (subclass class)
  (when (symbolp subclass) (setq subclass (find-class subclass 'errorp)))
  (when (symbolp class)    (setq class    (find-class class    'errorp)))
  (member class (slot-value subclass 'precedence-list)))
(defun instancep (object class) (subclassp (class-of object) class))

(defun canonicalize-system-instances (new-class)
  (let ((name (slot-value new-class 'name)))
    (cond
      ((subclassp new-class (ensure-class-object 'base-class))
       (loop for class being the hash-values of *classes*
             do (when (eq (class-of class) new-class)
                  (canonicalize-instance class))))
      ((eq name 'standard-direct-slot-definition)
       (loop for class being the hash-values of *classes*
             do (when (finalized-p class)
                  (dolist (slot (slot-value class 'direct-slots))
                    (canonicalize-instance slot)))))
      ((eq name 'standard-effective-slot-definition)
       (loop for class being the hash-values of *classes*
             do (when (finalized-p class)
                  (dolist (slot (slot-value class 'slots))
                    (canonicalize-instance slot))))))))

(defun finalize-system-class-inheritance (class)
  (setf (slot-value class 'precedence-list)
        (compute-standard-class-precedence-list class))
  (setf (slot-value class 'slots)
        (compute-system-class-slots class))
  (setf (slot-value class 'default-initargs)
        (compute-standard-default-initargs class))
  (setf (slot-value class 'finalized-p) t)
  (dolist (super (slot-value class 'direct-superclasses))
    (pushnew class (slot-value super 'direct-subclasses)))

  (canonicalize-system-instances class))

(defun ensure-system-class (name &rest initargs)
  (format t "~&ensure-system-class: class name = ~S~%" name)
  (let* ((initargs (apply #'canonicalize-system-class-initargs name initargs))
         (class (apply #'ensure-class-object name initargs)))
    (finalize-system-class-inheritance class)
    class))


;;; Inheritance Structure of Metaobject Classes specified by MOP
;;         http://www.lisp.org/mop/concepts.html#inherit-struct-figure
;;
;;   t-+- standard-object
;;     |      +- a class defined by DEFCLASS
;;     |      +- *metaobject -+- *specializer
;;     |      |               |     +- *class -+- standard-class
;;     |      |               |     |          +- funcallable-standard-class
;;     |      |               |     |          +- forward-referenced-class 
;;     |      |               |     |          +- structure-class
;;     |      |               |     |          +- built-in-class
;;     |      |               |     +- eql-specializer
;;     |      |               +- *method - standard-method
;;     |      |               |               +- *standard-accessor-method
;;     |      |               |                    + standard-reader-method
;;     |      |               |                    + standard-writer-method
;;     |      |               +- *method-combination
;;     |      |               +- *slot-definition
;;     |      |               |     +- *effective-slot-definition ---------+
;;     |      |               |     +- *direct-slot-definition --------+   |
;;     |      +------+        |     +- *standard-slot-definition       |   |
;;     |             |        |          + standard-direct-slot-definition |
;;     |             |        |          + standard-effective-slot-definition
;;     |             |        |
;;     |             |        +----------+
;;     +- function   |                   |
;;     |      |      |                   |
;;     |  funcallable-standard-object    |
;;     |      |                          |
;;     |      |      +-------------------+
;;     |      |      |
;;     |  *generic-function
;;     |      |
;;     |  standard-generic-function
;;     |
;;     +- structure-object
;;     |      |
;;     |      +- a structure defined by DEFSTRUCT
;;
;; Each class marked with a ``*'' is an abstract class and is not
;; intended to be instantiated. The results are undefined if an attempt
;; is made to make an instance of one of these classes with make-instance.

;; Current build time DEFCLASS limitations.
;; * superclasses must be defclassed before their subclasses.
;; * shared slots are not supported (partially implemented).

(defclass t (t) ()
  (:documentation "A superclass of every class, including itself.")
  (:metaclass built-in-class))
(defclass standard-object (t) ()
  (:documentation "A superclass of every class that is an instance of standard-class except itself."))
(defclass metaobject (standard-object) ()) ; mop abstract
(defclass specializer (metaobject)      ; mop abstract
  ((direct-methods :reader specializer-direct-methods
                   :initarg :direct-methods :initform nil)
   (direct-generic-functions :reader specializer-direct-generic-functions
                             :initarg :direct-generic-functions :initform nil)))


(defclass class (specializer)           ; clos abstract
  ((name :accessor class-name :initarg :name :initform nil)))
(defclass base-class (class)
  ((version :accessor class-version :initarg :version :initform nil)
   (slots :reader class-slots :initarg :slots :initform nil)
   (direct-slots :reader class-direct-slots :initarg :direct-slots :initform nil)
   (precedence-list :reader class-precedence-list
                    :initarg :precedence-list :initform nil)
   (direct-superclasses :accessor class-direct-superclasses
                        :initarg :direct-superclasses :initform nil)
   (direct-subclasses :accessor class-direct-subclasses
                      :initarg :direct-subclasses :initform nil)
   (default-initargs :reader class-default-initargs
                     :initarg :default-initargs :initform nil)
   (direct-default-initargs :reader class-direct-default-initargs
                            :initarg :direct-default-initargs :initform nil)   
   (finalized-p :reader class-finalized-p :initarg :finalized-p :initform nil)
   (documentation :initform nil :initarg :documentation)))
(defclass standard-base-class (base-class)
  ((version :initform 0)
   (old-classes :reader class-old-classes
                :initform (make-array 1 :adjustable t :fill-pointer 0))
   (dependents :initform nil)))
(defclass standard-class (standard-base-class) ()) ; clos
(defclass funcallable-standard-class (standard-base-class) ; mop
  ((direct-superclasses
    :initform (list (find-class 'funcallable-standard-object)))))
(defclass forward-referenced-class (class) ; mop
  ((direct-subclasses :accessor class-direct-subclasses :initform nil)))
(defclass structure-class (base-class) ; clos
  ()
  (:metaclass structure-class))
(defclass built-in-class (base-class) ()) ; clos
(defclass old-class (class)
  ((version :reader class-version)
   (slots :reader class-slots)))


(defvar *eql-specializers* (make-hash-table))
(defclass eql-specializer (specializer) ; mop
  ((object :initarg :object)))
(defun eql-specializer-object (eql-specializer) ; mop
  (slot-value eql-specializer 'object))
(defun find-eql-specializer (object) (gethash object *eql-specializers*))
(defun intern-eql-specializer (object)  ; mop
  (or (find-eql-specializer object)
      (setf (gethash object *eql-specializers*)
            (make-instance 'eql-specializer :object object))))


(defclass slot-definition (metaobject) ()) ; mop abstract
(defclass standard-slot-definition (slot-definition) ; mop abstract
  ((name :reader slot-definition-name :initarg :name)
   (type :reader slot-definition-type :initarg :type :initform 't)
   (allocation :reader slot-definition-allocation
               :initarg :allocation :initform :instance)
   (initargs :reader slot-definition-initargs :initarg :initargs)
   (initform :reader slot-definition-initform :initarg :initform)
   (initfunction :reader slot-definition-initfunction
                 :initarg :initfunction)
   (shared-binding :reader slot-definition-shared-binding
                   :initarg :shared-binding)
   (documentation :initarg :documentation))
  (:default-initargs :name (error "Slot name must be specified.")))
(defclass direct-slot-definition (slot-definition) ; mop abstract
  ((readers :reader slot-definition-readers :initarg :readers)
   (writers :reader slot-definition-writers :initarg :writers)))
(defclass effective-slot-definition (slot-definition) ; mop abstract
      ((location :reader slot-definition-location :initarg :location)))
(defclass standard-direct-slot-definition (standard-slot-definition
                                           direct-slot-definition) ; mop
  ())
(defclass standard-effective-slot-definition (standard-slot-definition
                                              effective-slot-definition) ; mop
  ())


(defclass method (metaobject) ())       ; clos abstract
(defclass standard-method (method)      ; clos
  ((function :reader method-function :initarg :function)
   (qualifiers :reader method-qualifiers :initarg :qualifiers)
   (specializers :reader method-specializers :initarg :specializers)
   (specialized-lambda-list :reader method-specialized-lambda-list
                            :initarg :specialized-lambda-list)
   (lambda-list :reader method-lambda-list :initarg :lambda-list)
   (generic-function :reader method-generic-function
                     :initarg :generic-function)
   ))
(defclass standard-accessor-method (standard-method) ()) ; mop abstract
(defclass standard-reader-method (standard-accessor-method) ; mop
  ())
(defclass standard-writer-method (standard-accessor-method) ; mop
  ())


(defclass function (t) () (:metaclass built-in-class))
(defclass funcallable-standard-object (standard-object function) ; mop
  ()
  (:metaclass funcallable-standard-class))
(defun funcallable-instance-function (funcallable-instance)
  (slot-value funcallable-instance +funcallable-instance-function-slot-name+))

(defun set-funcallable-instance-function (funcallable-instance function) ; mop
  (setf (slot-value funcallable-instance
                    +funcallable-instance-function-slot-name+)
        function))
(defclass generic-function (metaobject funcallable-standard-object) ()
  ;; clos abstract
  (:metaclass funcallable-standard-class))
(defclass standard-generic-function (generic-function) ; clos
  ((name :reader generic-function-name :initarg :name)
   (lambda-list :reader generic-function-lambda-list
                :initarg :lambda-list)
   (argument-precedence-order :reader generic-function-argument-precedence-order
                              :initarg :argument-precedence-order)
   (declarations :reader generic-function-declarations
                 :initarg :declarations)
   (method-class :reader generic-function-method-class
                 :initarg :method-class)
   (method-combination :reader generic-function-method-combination
                       :initarg :method-combination)
   (methods :reader generic-function-methods :initarg :methods)

   (number-of-required-args :reader number-of-required-args
                            :initarg :number-of-required-args)
   (applicable-methods :reader generic-function-applicable-methods
                       :initform (make-hash-table))
   (effective-methods :reader generic-function-effective-methods
                      :initform (make-hash-table :test #'equal))
   (dependents :initform nil)))


(defclass structure-object (t) ()
  (:documentation "A superclass of every class that is an instance of structure-class except itself.")
  (:metaclass structure-class))


;; Classes that correspond to pre-defined type specifiers
;; http://www.lispworks.com/reference/HyperSpec/Body/04_cg.htm#classtypecorrespondence
(defclass symbol              (t)              () (:metaclass built-in-class))
(defclass character           (t)              () (:metaclass built-in-class))
(defclass hash-table          (t)              () (:metaclass built-in-class))

(defclass sequence            (t)              () (:metaclass built-in-class))
(defclass list                (sequence)       () (:metaclass built-in-class))
(defclass cons                (list)           () (:metaclass built-in-class))
(defclass null                (symbol list)    () (:metaclass built-in-class))
(defclass array               (t)              () (:metaclass built-in-class))
(defclass vector              (array sequence) () (:metaclass built-in-class))
(defclass bit-vector          (vector)         () (:metaclass built-in-class))
(defclass string              (vector)         () (:metaclass built-in-class))

(defclass number              (t)              () (:metaclass built-in-class))
(defclass complex             (number)         () (:metaclass built-in-class))
(defclass real                (number)         () (:metaclass built-in-class))
(defclass float               (real)           () (:metaclass built-in-class))
(defclass rational            (real)           () (:metaclass built-in-class))
(defclass ratio               (rational)       () (:metaclass built-in-class))
(defclass integer             (rational)       () (:metaclass built-in-class))

(defclass stream              (t)              () (:metaclass built-in-class))
(defclass broadcast-stream    (stream)         () (:metaclass built-in-class))
(defclass concatenated-stream (stream)         () (:metaclass built-in-class))
(defclass string-stream       (stream)         () (:metaclass built-in-class))
(defclass echo-stream         (stream)         () (:metaclass built-in-class))
(defclass synonym-stream      (stream)         () (:metaclass built-in-class))
(defclass file-stream         (stream)         () (:metaclass built-in-class))
(defclass two-way-stream      (stream)         () (:metaclass built-in-class))

(defclass pathname            (t)              () (:metaclass built-in-class))
(defclass logical-pathname    (pathname)       () (:metaclass built-in-class))

(defclass package             (t)              () (:metaclass built-in-class))
(defclass random-state        (t)              () (:metaclass built-in-class))
(defclass readtable           (t)              () (:metaclass built-in-class))
(defclass restart             (t)              () (:metaclass built-in-class))



(defclass method-combination (metaobject) ()) ; clos
(defstruct method-combination-type
  (name)
  (lambda-list)
  (group-specifiers)
  (args-lambda-list)
  (generic-function-symbol)
  (documentation)
  (function)
  (short-form-options))
(defclass standard-method-combination (method-combination) ; clos
  ((type :reader method-combination-type :initarg :type)
   (arguments :reader method-combination-arguments :initarg :arguments)))

(defparameter *method-combination-types* (make-hash-table))

(defun define-method-combination-type (name &rest initargs)
  (let ((combination-type (apply #'make-method-combination-type
                                 :allow-other-keys t :name name initargs)))
    (setf (gethash name *method-combination-types*) combination-type)))

(defun method-group-p (selecter qualifiers)
  ;; selecter::= qualifier-pattern | predicate
  (etypecase selecter
    (list (or (equal selecter qualifiers)
              (let ((last (last selecter)))
                (when (eq '* (cdr last))
                  (let* ((prefix `(,@(butlast selecter) ,(car last)))
                         (pos (mismatch prefix qualifiers)))
                    (or (null pos) (= pos (length prefix))))))))
    ((eql *) t)
    (symbol (funcall (symbol-function selecter) qualifiers))))

(defun check-variable-name (name)
  (flet ((valid-variable-name-p (name)
           (and (symbolp name) (not (constantp name)))))
    (assert (valid-variable-name-p name))))

(defun canonicalize-method-group-spec (spec)
  ;; spec ::= (name {qualifier-pattern+ | predicate} [[long-form-option]]) 
  ;; long-form-option::= :description description | :order order |
  ;;                     :required required-p
  ;; a canonicalized-spec is a simple plist.
  (let* ((rest spec)
         (name (prog2 (check-variable-name (car rest))
                   (car rest)
                 (setq rest (cdr rest))))
         (option-names '(:description :order :required))
         (selecters (let ((end (or (position-if #'(lambda (it)
                                                    (member it option-names))
                                                rest)
                                   (length rest))))
                      (prog1 (subseq rest 0 end)
                        (setq rest (subseq rest end)))))
         (description (getf rest :description ""))
         (order (getf rest :order :most-specific-first))
         (required-p (getf rest :required)))
    `(:name ,name
      :predicate #'(lambda (qualifiers)
                     (loop for item in ',selecters
                           thereis (method-group-p item qualifiers)))
      :description ,description
      :order ,order
      :required ,required-p)))

(defconstant +gf-args-variable+ (gensym "GF-ARGS-VARIABLE-")
  "A Variable name whose value is a list of all arguments to a generic function.")

(defun extract-required-part (lambda-list)
  (flet ((skip (key lambda-list)
           (if (eq (first lambda-list) key)
               (cddr lambda-list)
               lambda-list)))
    (ldiff (skip '&environment (skip '&whole lambda-list))
           (member-if #'(lambda (it) (member it lambda-list-keywords))
                      lambda-list))))

(defun extract-specified-part (key lambda-list)
  (case key
    ((&eval &whole)
     (list (second (member key lambda-list))))
    (t
     (let ((here (cdr (member key lambda-list))))
       (ldiff here
              (member-if #'(lambda (it) (member it lambda-list-keywords))
                         here))))))

(defun extract-optional-part (lambda-list)
  (extract-specified-part '&optional lambda-list))

(defun parse-define-method-combination-arguments-lambda-list (lambda-list)
  ;; Define-method-combination Arguments Lambda Lists
  ;; http://www.lispworks.com/reference/HyperSpec/Body/03_dj.htm
  (let ((required (extract-required-part lambda-list))
        (whole    (extract-specified-part '&whole    lambda-list))
        (optional (extract-specified-part '&optional lambda-list))
        (rest     (extract-specified-part '&rest     lambda-list))
        (keys     (extract-specified-part '&key      lambda-list))
        (aux      (extract-specified-part '&aux      lambda-list)))
    (values (first whole)
            required
            (mapcar #'(lambda (spec)
                        (if (consp spec)
                            `(,(first spec) ,(second spec) ,@(cddr spec))
                            `(,spec nil)))
                    optional)
            (first rest)
            (mapcar #'(lambda (spec)
                        (let ((key (if (consp spec) (car spec) spec))
                              (rest (when (consp spec) (rest spec))))
                          `(,(if (consp key) key `(,(%keyword key) ,key))
                            ,(car rest)
                            ,@(cdr rest))))
                    keys)
            (mapcar #'(lambda (spec)
                        (if (consp spec)
                            `(,(first spec) ,(second spec))
                            `(,spec nil)))
                    aux))))

(defmacro getk (plist key init-form)
  "Similar to getf except eval and return INIT-FORM if KEY has no value in PLIST."
  (let ((not-exist (gensym))
        (value (gensym)))
    `(let ((,value (getf ,plist ,key ,not-exist)))
      (if (eq ,not-exist ,value) ,init-form ,value))))

(defmacro with-args-lambda-list (args-lambda-list generic-function-symbol
                                 &body forms)
  (let ((gf-lambda-list (gensym))
        (nrequired (gensym))
        (noptional (gensym))
        (rest-args (gensym)))
    (multiple-value-bind (whole required optional rest keys aux)
        (parse-define-method-combination-arguments-lambda-list args-lambda-list)
      `(let* ((,gf-lambda-list (slot-value ,generic-function-symbol 'lambda-list))
              (,nrequired (length (extract-required-part ,gf-lambda-list)))
              (,noptional (length (extract-optional-part ,gf-lambda-list)))
              (,rest-args (subseq ,+gf-args-variable+ (+ ,nrequired ,noptional)))
              ,@(when whole `((,whole ,+gf-args-variable+)))
              ,@(loop for var in required and i upfrom 0
                      collect `(,var (when (< ,i ,nrequired)
                                       (nth ,i ,+gf-args-variable+))))
              ,@(loop for (var init-form) in optional and i upfrom 0
                      collect
                      `(,var (if (< ,i ,noptional)
                                 (nth (+ ,nrequired ,i) ,+gf-args-variable+)
                                 ,init-form)))
              ,@(when rest `((,rest ,rest-args)))
              ,@(loop for ((key var) init-form) in keys and i upfrom 0
                      collect `(,var (getk ,rest-args ',key ,init-form)))
              ,@(loop for (var init-form) in aux and i upfrom 0
                      collect `(,var ,init-form)))
        ,@forms))))

(defun invalid-method-error (method format-control &rest args)
  (declare (ignorable method))
  (apply #'error format-control args))

(defun method-combination-error (format-control &rest args)
  (apply #'error format-control args))

(defmacro with-method-groups (method-group-specs methods-form &body forms)
  (flet ((grouping-form (spec methods-var)
           (let ((predicate (getf spec :predicate))
                 (group (gensym))
                 (leftovers (gensym))
                 (method (gensym)))
             `(let ((,group '())
                    (,leftovers '()))
               (dolist (,method ,methods-var)
                 (if (funcall ,predicate (slot-value ,method 'qualifiers))
                     (push ,method ,group)
                     (push ,method ,leftovers)))
               (ecase ,(getf spec :order)
                 (:most-specific-last )
                 (:most-specific-first (setq ,group (nreverse ,group))))
               ,@(when (getf spec :required)
                       `((when (null ,group)
                           (error "Method group ~S must not be empty."
                                  ',(getf spec :name)))))
               (setq ,methods-var (nreverse ,leftovers))
               ,group))))
    (let ((rest (gensym))
          (method (gensym)))
      `(let* ((,rest ,methods-form)
              ,@(mapcar #'(lambda (spec)
                            `(,(getf spec :name) ,(grouping-form spec rest)))
                        method-group-specs))
        (dolist (,method ,rest)
          (invalid-method-error ,method
                                "Method ~S with qualifiers ~S does not~ belong ~
                                 to any method group."
                                ,method (slot-value ,method 'qualifiers)))
        ,@forms))))

(defun method-combination-type-lambda
    (&key name lambda-list args-lambda-list generic-function-symbol
     method-group-specs declarations forms &allow-other-keys)
  (let ((methods (gensym)))
    `(lambda (,generic-function-symbol ,methods ,@lambda-list)
      ,@declarations
      (let ((*message-prefix* ,(format nil "METHOD COMBINATION TYPE ~S: " name)))
        (with-method-groups ,method-group-specs
          ,methods
          ,@(if (null args-lambda-list)
                forms
                `((with-args-lambda-list ,args-lambda-list
                    ,generic-function-symbol
                    ,@forms))))))))

(defun long-form-method-combination-args (args)
  ;; define-method-combination name lambda-list (method-group-specifier*) args
  ;; args ::= [(:arguments . args-lambda-list)]
  ;;          [(:generic-function generic-function-symbol)]
  ;;          [[declaration* | documentation]] form*
  (let ((rest args))
    (labels ((nextp (key) (and (consp (car rest)) (eq key (caar rest))))
             (args-lambda-list ()
               (when (nextp :arguments)
                 (prog1 (cdr (car rest)) (setq rest (cdr rest)))))
             (generic-function-symbol ()
               (if (nextp :generic-function)
                   (prog1 (second (car rest)) (setq rest (cdr rest)))
                   (gensym)))
             (declaration* ()
               (let ((end (position-if-not #'declarationp rest)))
                 (when end
                   (prog1 (subseq rest 0 end) (setq rest (nthcdr end rest))))))
             (documentation? ()
               (when (stringp (car rest))
                 (prog1 (car rest) (setq rest (cdr rest)))))
             (form* () rest))
      (let ((declarations '()))
        `(:args-lambda-list ,(args-lambda-list)
          :generic-function-symbol ,(generic-function-symbol)
          :documentation ,(prog2 (setq declarations (declaration*))
                             (documentation?))
          :declarations (,@declarations ,@(declaration*))
          :forms ,(form*))))))

(defun define-long-form-method-combination (name lambda-list method-group-specs
                                            &rest args)
  (let* ((initargs `(:name ,name
                     :lambda-list ,lambda-list
                     :method-group-specs
                     ,(mapcar #'canonicalize-method-group-spec method-group-specs)
                     ,@(long-form-method-combination-args args)))
         (lambda-expression (apply #'method-combination-type-lambda initargs)))
    ;;(format t "~&~S~%" lambda-expression)
    (apply #'define-method-combination-type name
           `(,@initargs
             :function ,(compile nil lambda-expression)
             :short-form-options nil))))

(defun define-short-form-method-combination
    (name &key identity-with-one-argument (documentation "") (operator name))
  (define-long-form-method-combination name
      '(&optional (order :most-specific-first))
    `((around (:around))
      (primary (,name) :order order :required t))
      documentation
      `(let ((form (if (and ,identity-with-one-argument (null (rest primary)))
                       `(call-method ,(first primary))
                       (cons ',operator (mapcar #'(lambda (method)
                                                    `(call-method ,method))
                                                primary)))))
        (if around
            `(call-method ,(first around) (,@(rest around) (make-method ,form)))
            form)))
  (let ((combination-type (gethash name *method-combination-types*)))
    (setf (method-combination-type-short-form-options combination-type)
          `(:documentation ,documentation
            :operator ,operator
            :identity-with-one-argument ,identity-with-one-argument)))
  name)

(defmacro define-method-combination (name &rest args) ; clos
  "Define new types of method combination."
  (format t "~&define-method-combination: ~S~%" name)
  `(let ((*message-prefix*
          ,(format nil "DEFINE-METHOD-COMBINATION ~S: " name)))
    (apply #',(if (listp (first args))
                  'define-long-form-method-combination
                  'define-short-form-method-combination) ',name ',args)))


;; 7.6.6.4 Built-in Method Combination Types
;; http://www.lispworks.com/reference/HyperSpec/Body/07_ffd.htm
(define-method-combination +      :identity-with-one-argument t)
(define-method-combination and    :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument t)
(define-method-combination list   :identity-with-one-argument t)
(define-method-combination max    :identity-with-one-argument t)
(define-method-combination min    :identity-with-one-argument t)
(define-method-combination nconc  :identity-with-one-argument t)
(define-method-combination or     :identity-with-one-argument t)
(define-method-combination progn  :identity-with-one-argument t)
(define-method-combination standard ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                      (progn ,@(call-methods before)
                             (call-method ,(first primary)
                                          ,(rest primary)))
                      ,@(call-methods (reverse after)))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
            (,@(rest around)
             (make-method ,form)))
          form))))


;;;;
;#|

;; AMOP 5.4.5 The defgeneric Macro
;; http://www.lisp.org/mop/concepts.html#defgeneric
;;
;; AMOP 5.4.3 The defmethod Macro
;; http://www.lisp.org/mop/concepts.html#defmethod
;; AMOP 5.4.4 Processing Method Bodies
;; http://www.lisp.org/mop/concepts.html#processing-method-bodies

(defvar *generic-functions* (make-hash-table))
(defun find-generic-function (symbol &optional errorp)
  (let ((symbol-function function))
    (if (instancep function 'generic-function)
        function
        (multiple-value-bind (generic-function presentp)
            (gethash discriminating-function *generic-functions*)
          (if presentp
              generic-function
              (when errorp
                (error "Generic function ~S does not exist." symbol)))))))
(defun install-generic-function (symbol gf)
  (let (discriminating-function (compute-discriminating-function gf))
    (if *symbol-function-is-funcallable-object-p*
        (when symbol (setf (symbol-function symbol) gf))
        (progn
          (remhash (funcallable-instance-function gf) *generic-functions*)
          (setf (gethash discriminating-function *generic-functions*) gf)
          (when symbol (setf (symbol-function symbol) discriminating-function))))
    (set-funcallable-instance-function gf discriminating-function)
    gf))


(defun method-spec (method-description)
  ;; method-description::= (:method method-qualifier* specialized-lambda-list
  ;;                        [[declaration* | documentation]] form*)
  (let ((rest (progn
                (assert (eq :method (car method-description)))
                (cdr method-description))))
    (flet ((qualifier* ()
             (let* ((end (or (position-if #'consp rest)
                             (error "No specialized lambda list found in ~S."
                                    method-description)))
                    (method-qualifiers (subseq rest 0 end)))
               (assert (notany #'listp method-qualifiers))
               (prog1 method-qualifiers (setq rest (nthcdr end rest)))))
           (lambda-list1 ()
             (let ((specialized-lambda-list (car rest)))
               (validate-specialized-lambda-list specialized-lambda-list)
               (prog1 specialized-lambda-list (setq rest (cdr rest)))))
           (declaration* ()
             (let ((end (position-if-not #'declarationp rest)))
               (when end
                 (prog1 (subseq rest 0 end) (setq rest (nthcdr end rest))))))
           (documentation? ()
             (when (stringp (car rest))
               (prog1 (car rest) (setq rest (cdr rest)))))
           (form* () rest))
      `(:qualifiers ,(qualifier*)
        :specialized-lambda-list ,(lambda-list1)
        ,@(let ((decls (declaration*)))
               `(:documentation ,(documentation?)
                 :declarations ,(mapappend #'cdr `(,@decls ,@(declaration*)))))
        :forms ,(form*)))))

(defun method-spec-to-ensure-generic-function-form (name spec env)
  (let* ((lambda-list (extract-lambda-list (getf spec :specialized-lambda-list)))
         (options (canonicalize-defgeneric-options lambda-list '())))
    `(ensure-generic-function ,name :environment ,env ,@options)))

(defun allow-other-keys (lambda-list)
  (if (and (member '&key lambda-list)
           (not (member '&allow-other-keys lambda-list)))
      (let* ((key-end (or (position &aux lambda-list) (length lambda-list)))
             (aux-part (subseq lambda-list key-end)))
        `(,@(subseq lambda-list 0 key-end) &allow-other-keys ,@aux-part))
      lambda-list))

(defun make-system-method-lambda ()
  )

(defun <make-method-lambda>
    (generic-function method lambda-expression environment)
 (funcall (if *mop-working-p* #'make-method-lambda #'make-system-method-lambda)
          generic-function method lambda-expression environment))

(defun <class-prototype> (class)
  (funcall (if *mop-working-p* #'class-prototype #'make-system-instance)
           class))

(defun method-initargs-form (gf-form environment &key initial-method-p
                             null-lexical-environment-p specialized-lambda-list
                             qualifiers documentation declarations forms)
  (let* ((lambda-list (extract-lambda-list specialized-lambda-list))
         (specializer-names (extract-specializer-names specialized-lambda-list))
         (gf (gensym))
         (methods (gensym)) ;; used by make-method-lambda
         (lambda-expression (gensym))
         (initargs (gensym)))
    `(let* ((,gf ,gf-form)
            (,method-class (slot-value ,gf 'method-class)))
      (multiple-value-bind (,lambda-expression ,initargs)
          (<make-method-lambda> ,gf
                                (<class-prototype> ,method-class)
                                '(lambda (,+gf-args-variable+ ,methods)
                                  (apply #'(lambda ,(allow-other-keys lambda-list)
                                             (declare ,@declarations)
                                             ,@forms)
                                   ,+gf-args-variable+))
                                ,environment)
        (append ,initargs
                (list 
                 :qualifiers ',qualifiers
                 :specialized-lambda-list ',specialized-lambda-list
                 :lambda-list ',lambda-list
                 :specializers (mapcar #'find-class ,specializer-names)
                 :documentation ',documentation
                 :function (compile nil ,(if null-lexical-environment-p
                                             `(eval ,lambda-expression)
                                             'lambda-expression))
                 :initial-method-p ,initial-method-p)))))))


(defun check-defgeneric-declarations (declarations)
  
  )

(defun check-singleton-options (options valid-option-names)
  (flet ((redundant-option-error (option-name)
           (error 'simple-program-error
                  :format-control "~AOption ~S is given more than once."
                  :format-arguments (list *message-prefix* option-name)))
         (invalid-option-error (option-name)
           (error 'simple-program-error
                  :format-control "~AInvalid option ~S is given."
                  :format-arguments (list *message-prefix* option-name))))
    (loop for (key . rest) in options with processed = '()
          when (member key processed) do (redundant-option-error key)
          when (not (member key valid-option-names)) do (invalid-option-error key)
          do (push key processed)))

(defvar generic-function-initarg-names
  '(:argument-precedence-order :declare :documentation :environment
    :generic-function-class :lambda-list :method-class :method-combination))

(defun generic-function-initargs-form (options)
  (check-singleton-options options generic-function-initarg-names)
  (flet ((value-form (key rest)
           (case key
             ((:documentation :environment :generic-function-class
               :method-class :lambda-list)
              (destructuring-bind (value) rest (qt? value)))
             (t (qt? rest)))))
    (loop for (key . rest) in options
          when (eq key :declare) do (setq key :declarations)
          nconc `(,(qt? key) ,(value-form key rest)) into result
          finally (return `(list ,@result)))))

(defun ensure-system-generic-function (name &rest initargs)
  
  )

(defun add-system-method (generic-function method)
  )

(defun <ensure-generic-function> (name &rest initargs)
  (apply (if *mop-working-p*
             #'ensure-generic-function
             #'ensure-system-generic-function)
         name initargs))

(defun <make-instance> (name &rest initargs)
  (apply (if *mop-working-p* #'make-instance #'make-system-instance)
         name initargs))

(defun <add-method> (generic-function method)
  (funcall (if *mop-working-p* #'add-method #'add-system-method)
           generic-function method))

(defmacro defgeneric (name lambda-list &body args &environment env) ; clos
  "Define a generic function named NAME."
  (let* ((*message-prefix* (format nil "DEFGENERIC ~S: " name))
         (method-descriptions (loop for spec in args
                                    if (eq (first spec) :method) collect spec))
         (declarations (loop for spec in args
                             if (eq (first spec) 'declare) append (rest spec)))
         (options `((:lambda-list ,lambda-list)
                    (:environment ,env)
                    ,@(when declarations `((:declare ,@declarations)))
                    ,@(loop for spec in args
                            unless (member (first spec) '(:method declare))
                              collect spec)))
         (gf (gensym))
         (method-class (gensym))
         (methods (gensym)))
    (check-defgeneric-declarations declarations)
    `(let* ((*message-prefix* ,*message-prefix*)
            (,gf (apply #'<ensure-generic-function> ',name
                        ,(generic-function-initargs-form options)))
            (,method-class (generic-function-method-class ,gf))
            (,methods (list ,@(mapcar
                               #'(lambda (spec)
                                   `(apply #'<make-instance> ,method-class
                                     :initial-method-p t
                                     ,(apply #'method-initargs-form gf env spec)))
                               (mapcar #'method-spec method-descriptions)))))
      (mapc #'(lambda (method) (<add-method> ,gf method)) ,methods)
      ,gf)))))

(defmacro defmethod (name &rest args &environment env) ; clos
  "Define a method named NAME."
  (let ((spec (method-spec `(:method ,@args)))
        (gf (gensym))
        (method (gensym)))
    `(let* ((,gf (or (find-generic-function name)
                     ,(method-spec-to-ensure-generic-function-form
                       name spec env)))
            (,method (apply #'<make-instance>
                            (generic-function-method-class ,gf)
                            (method-initargs-form ,gf ,env ,@spec))))
      (<add-method> ,gf ,method)
      ,method)))

(defgeneric ensure-class-using-class (class name &key direct-default-initargs direct-slots direct-superclasses metaclass &allow-other-keys)) ; mop

(defmethod ensure-class-using-class ((class class) name &key (metaclass 'standard-class) direct-superclasses &allow-other-keys)
  (check-type class metaclass)
  (apply #'reinitialize-instance class initargs))

(defmethod ensure-class-using-class ((class forward-referenced-class) name &rest initargs &key (metaclass 'standard-class) direct-superclasses &allow-other-keys)
  (apply #'change-class class metaclass initargs))

(defmethod ensure-class-using-class ((class null) name &rest initargs
                                     &key (metaclass 'standard-class)
                                     direct-superclasses &allow-other-keys)
  (setf (find-class name) (apply #'make-instance metaclass initargs)))

(defun ensure-class (name &rest args &key &allow-other-keys) ; mop
  (apply #'ensure-class-using-class (find-class name) name args))



(defgeneric compute-class-precedence-list (class)) ; mop
(defmethod compute-class-precedence-list ((class class))
  (compute-standard-class-precedence-list class))


(defgeneric compute-default-initargs (class)) ; mop
(defmethod compute-default-initargs ((class standard-base-class))
  (compute-standard-default-initargs class))


(defgeneric effective-slot-definition-class (class &rest initargs)) ; mop
(defmethod effective-slot-definition-class ((class standard-base-class)
                                            &rest initargs)
  (find-class 'standard-effective-slot-definition))
(defgeneric compute-effective-slot-definition
    (class name direct-slot-definitions)) ; mop
(defmethod compute-effective-slot-definition ((class standard-base-class)
                                              name direct-slot-definitions)
  (apply #'make-instance
         (apply #'effective-slot-definition-class class initargs)
         (effective-slot-initargs class name direct-slot-definitions)))
(defgeneric compute-slots (class))      ; mop
(defmethod compute-slots :around ((class standard-base-class))
  (let ((slots (call-next-method)))
    (assign-slots-locations slots)
    slots))
(defmethod compute-slots ((class standard-base-class))
  (loop for (name direct-slots) in (effective-slot-specs class)
        collect (compute-effective-slot-definition class name direct-slots)))


(defgeneric finalize-inheritance (class)) ; mop
(defmethod finalize-inheritance ((class standard-base-class))
  ;; see "Class Finalization Protocol"
  ;; http://www.lisp.org/mop/concepts.html#class-finalization-protocol
  (setf (slot-value class 'precedence-list) (compute-class-precedence-list class))
  (setf (slot-value class 'default-initargs) (compute-default-initargs class))
  (setf (slot-value class 'slots) (compute-slots class))
  (setf (slot-value class 'finalized-p) t)

  class)
(defmethod finalize-inheritance ((class forward-referenced-class))
  (error "Cannot finalize inheritance for forward-referenced-class object."))
;; ! write an after method which computes a hash-table of slot-names and
;; slot-definition objects which will be used in find-slot.


(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys)
  (:documentation ;; clos
   "Create and return a new instance of CLASS, without initializing it."))
(defmethod allocate-instance ((class standard-base-class) &rest initargs)
  (make-%standard-object :class class
                         :version (slot-value class 'version)
                         :storage (allocate-standard-instance-storage class)))
(defmethod allocate-instance ((class structure-class) &rest initargs)
  (error "allocate-instance specialized for structure-class is not implemented."))
(defmethod allocate-instance ((class built-in-class) &rest initargs)
  (error "`allocate-instance' is not applicable to built-in-class."))

(defgeneric validate-superclass (class superclass)) ; mop
(defmethod validate-superclass ((class class) (superclass class))
  (let ((class-of-class (class-of class-of))
        (class-of-superclass (class-of superclass))
        (standard-class (find-class 'standard-class))
        (funcallable-standard-class (find-class 'funcallable-standard-class)))
    ;; http://www.lisp.org/mop/dictionary.html#validate-superclass
    (or (eq superclass (find-class 't))
        ;; (i) If the superclass argument is the class named t, 
        (eq class-of-superclass class-of-class)
        ;; (ii) if the class of the class argument is the same as
        ;; the class of the superclass argument or 
        (and (eq class-of-class standard-class)
             (eq class-of-superclass funcallable-standard-class))
        (and (eq class-of-class funcallable-standard-class)
             (eq class-of-superclass standard-class))
        ;; (iii) if the classes one of the arguments is standard-class and
        ;; the class of the other is funcallable-standard-class. 
        )))

(defun canonicalize-direct-superclasses (class direct-superclasses)
  (flet ((superclass (designator)
           (etypecase designator
             (symbol (or (find-class designator)
                         (ensure-class designator
                                       :metaclass 'forward-referenced-class)))
             (class designator))))
    (mapcar #'(lambda (designator)
                (let ((superclass (superclass designator)))
                  (unless (validate-superclass class superclass)
                    (error "~S cannot be a superclass of ~S"
                                superclass class))
                  superclass))
            direct-superclasses)))

(defgeneric shared-initialize  (instance slot-names &rest initargs
                                         &key &allow-other-keys)
  (:documentation ;; clos
   "Fill the slots of INSTANCE using INITARGS and :initform forms."))

(defmethod shared-initialize ((instance standard-object) slot-names
                              &rest initargs)
  (standard-shared-initialize instance slot-names initargs))

(defgeneric check-initargs (instance gf-args-pairs initargs))
(defmethod check-initargs ((instance standard-object) gf-args-pairs initargs)
  (unless (getf initargs :allow-other-keys)
    (let* ((class (class-of instance))
           (valid-keys
            (remove-duplicates
             (nconc (mapappend #'function-keywords
                               (mapappend #'(lambda (gf-args-pair)
                                              (apply #'applicable-methods
                                                     gs-args-pair))
                                          gf-args-pairs))
                    (mapappend #'slot-definition-initargs (class-slots class))
                    '(:allow-other-keys))))
           (keys (remove-duplicates (plist-keys initargs)))
           (invalid-keys (set-difference keys valid-keys)))
      (when invalid-keys
        (error "Invalid initialization argument keyword~P: ~S"
               (length invalid-keys) invalid-keys)))))
(defmethod check-initargs ((instance standard-slot-definition)
                           gf-args-pairs initargs)
  (let ((initform-supplied-p (plist-member :initform initargs))
        (initfunction-supplied-p (plist-member :initfunction initargs)))
    (assert (or (and initform-supplied-p initfunction-supplied-p)
                (and (not initform-supplied-p) (not initfunction-supplied-p))))
    (when (and (not initform-supplied-p) (not initfunction-supplied-p))
      (setq initargs `(:initform nil :initfunction nil ,@initargs)))
    (call-next-method instance gf-args-pairs initargs)))

;; ... more check-initargs methods


(defmethod shared-initialize ((instance standard-base-class) slot-names
                              &rest initargs
                              &key (direct-slots "never used" direct-slots-p))
  (flet ((direct-slot (class spec)
           (apply #'make-instance
                  (apply #'direct-slot-definition-class class spec)
                  spec)))
    (when direct-slots-p
      (setq initargs
            `(:direct-slots ,(mapcar #'(lambda (spec) (direct-slot instance spec))
                                     direct-slots)
              ,@initargs)))
    (apply #'call-next-method instance slot-names initargs)
    ;; define readers & writers here using reader-method-class & writer-method-class !
    ))

(defmethod shared-initialize ((instance standard-class) slot-names &rest initargs &key (direct-superclasses "never used" direct-superclasses-p) (metaclass 'standard-class))
  ;; http://www.lispworks.com/reference/HyperSpec/Body/m_defcla.htm
  ;; If the superclass list is empty, then the superclass defaults
  ;; depending on the metaclass, with standard-object being the default
  ;; for standard-class.
  (when direct-superclasses-p
    (setq initargs
          `(:direct-superclasses
            ,(canonicalize-direct-superclasses
              instance (or direct-superclasses (list 'standard-object)))
            ,@initargs)))
  (apply #'call-next-method instance slot-names initargs))

(defmethod shared-initialize ((instance funcallable-standard-class) slot-names &rest initargs &key (direct-superclasses "never used" direct-superclasses-p) (metaclass 'funcallable-standard-class))
  ;; http://www.lisp.org/mop/dictionary.html#class-mo-init
  ;; if the class is an instance of funcallable-standard-class
  ;; or one of its subclasses the default value is list of the class
  ;; funcallable-standard-object.
  (when direct-superclasses-p
    (setq initargs
          `(:direct-superclasses
            ,(canonicalize-direct-superclasses
              instance
              (or direct-superclasses (list 'funcallable-standard-object)))
            ,@initargs)))
  
  (apply #'call-next-method instance slot-names initargs)

  (let ((name (getf initargs :name (generic-function-name instance))))
    (install-generic-function name instance))
  
  instance)


(defgeneric add-direct-subclass (superclass subclass)) ; mop
(defmethod add-direct-subclass ((superclass class) (subclass class))
  (pushnew subclass (class-direct-subclasses superclass)))

(defgeneric remove-direct-subclass (superclass subclass)) ; mop
(defmethod remove-direct-subclass ((superclass class) (subclass class))
  (setf (class-direct-subclasses superclass)
        (remove subclass (class-direct-subclasses superclass))))

(defgeneric reinitialize-instance (instance &rest initargs
                                            &key &allow-other-keys)
  (:documentation ;; clos
   "Change the values of local slots of INSTANCE according to INITARGS."))
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  ;; http://www.lispworks.com/reference/HyperSpec/Body/f_reinit.htm
  ;; The system-supplied primary method for reinitialize-instance checks
  ;; the validity of initargs and signals an error if an initarg is supplied
  ;; that is not declared as valid. The method then calls the generic function
  ;; shared-initialize with the following arguments: the instance, nil
  ;; (which means no slots should be initialized according to their initforms),
  ;; and the initargs it received.
  (check-initargs instance
                  `((,#'reinitialize-instance (,instance ,@initargs))
                    (,#'shared-initialize (,instance nil ,@initargs)))
                  initargs)
  (apply #'shared-initialize instance nil initargs))

(defmethod reinitialize-instance :after ((instance standard-generic-function)
                                         &rest initargs)
  (map-dependents instance
                  #'(lambda (dependent)
                      (apply #'update-dependent instance dependent initargs))))

(defgeneric make-instances-obsolete (class) ; clos
  (:documentation "Initiate the process of updating the instances of CLASS."))
(defmethod make-instances-obsolete ((class standard-base-class))
  (vector-push-extend (make-instance 'old-class :current-class class)
                      (class-old-classes class))
  (setf (slot-value class 'finalized-p) nil)
  (incf (slot-value class 'version))
  (mapc #'(lambda (gf) (clear-gf-cache gf))
        (specializer-direct-generic-functions class))
  (mapc #'(lambda (child)
            (when (class-finalized-p child) (make-instances-obsolete child)))
        (class-direct-subclasses class))
  class)
(defmethod make-instances-obsolete ((class symbol))
  (apply #'make-instances-obsolete (find-class class)))

(defmethod reinitialize-instance ((instance standard-base-class) &rest initargs)
  (let ((finalizedp (class-finalized-p instance))
        (previous (class-direct-superclasses instance)))
    (when finalizedp
      (make-instances-obsolete instance))
    (call-next-method)
    (let ((current (class-direct-superclasses instance)))
      (mapc #'(lambda (super) (remove-direct-subclass super instance))
            (set-difference previous current))
      (mapc #'(lambda (super) (add-direct-subclass super instance))
            (set-difference current previous)))
    (when finalizedp
      (finalize-inheritance instance)
      (map-dependents instance
                      #'(lambda (dependent)
                          (apply #'update-dependent
                                 instance dependent initargs))))
    instance))
         

(defgeneric update-instance-for-different-class (previous current &rest initargs &key &allow-other-keys)
  (:documentation ;; clos
   "Called only by change-class. Programmers may write methods for it."))
(defmethod update-instance-for-different-class ((previous standard-object) (current standard-object) &rest initargs)
  ;; http://www.lispworks.com/reference/HyperSpec/Body/f_update.htm
  ;; The system-supplied primary method on update-instance-for-different-class
  ;; checks the validity of initargs and signals an error if an initarg is
  ;; supplied that is not declared as valid. This method then initializes slots
  ;; with values according to the initargs, and initializes the newly added
  ;; slots with values according to their :initform forms. It does this by
  ;; calling the generic function shared-initialize with the following
  ;; arguments: the instance (current), a list of names of the newly added
  ;; slots, and the initargs it received. Newly added slots are those local
  ;; slots for which no slot of the same name exists in the previous class.
  (let ((added-local-slots (set-difference
                            (mapcan #'(lambda (slot)
                                        (when (local-slot-p slot)
                                          (list (slot-definition-name slot))))
                                    (class-slots (class-of current)))
                            (class-slot-names (class-of previous)))))
    (check-initargs current
                    `((,#'update-instance-for-different-class (,previous
                                                               ,current
                                                               ,@initargs))
                      (,#'shared-initialize (,current
                                             ,added-local-slots
                                             ,@initargs)))
                    initargs)
    (apply #'shared-initialize current added-local-slots initargs)))
(defmethod update-instance-for-different-class :after ((previous forward-referenced-class) (current standard-base-class) &rest initargs)
  (mapc #'(lambda (super) (add-direct-subclass super current))
        (class-direct-superclasses current)))

(defgeneric change-class (instance new-class &key &allow-other-keys)
  (:documentation ;; clos
   "Change the class of INSTANCE to NEW-CLASS destructively."))
(defmethod change-class ((instance t) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))
(defmethod change-class ((instance standard-object) (new-class standard-class) &rest initargs)
  (let ((previous (allocate-instance new-class)))
    (swap-%standard-object instance previous)
    (loop with prev-slot-names = (class-slot-names (class-of previous))
          slot in (mapcan #'(lambda (slot)
                              (when (and (local-slot-p slot)
                                         (member (slot-definition-name slot)
                                                 prev-slot-names))
                                (list slot)))
                          (class-slots new-class))
          name = (slot-definition-name slot)
          if (slot-boundp previous name) do (slot-makunbound instance name)
          else do (setf (slot-value instance name) (slot-value previous name)))
    (apply #'update-instance-for-different-class previous instance initargs)))
    

(defgeneric update-instance-for-redefined-class (instance added-slots discarded-slots property-list &rest initargs &key &allow-other-keys)
  (:documentation ;; clos
   "Called by the mechanism activated by make-instances-obsolete."))
(defmethod update-instance-for-redefined-class ((instance standard-object) added-slots discarded-slots property-list &rest initargs)
  ;; http://www.lispworks.com/reference/HyperSpec/Body/f_upda_1.htm
  ;; The system-supplied primary method on
  ;; update-instance-for-redefined-class checks the validity of initargs
  ;; and signals an error if an initarg is supplied that is not declared as
  ;; valid. This method then initializes slots with values according to the
  ;; initargs, and initializes the newly added-slots with values according
  ;; to their :initform forms. It does this by calling the generic function
  ;; shared-initialize with the following arguments: the instance, a list
  ;; of names of the newly added-slots to instance, and the initargs it
  ;; received. Newly added-slots are those local slots for which no slot of
  ;; the same name exists in the old version of the class.
  (let* ((class (class-of instance))
         (added-local-slots (mapcan #'(lambda (name)
                                        (if (local-slot-p (find-slot class name))
                                            (list name)
                                            nil))
                                    added-slots)))
    (check-initargs instance
                    `((,#'update-instance-for-redefined-class (,instance
                                                               ,added-slots
                                                               ,discarded-slots
                                                               ,property-list
                                                               ,@initargs))
                      (,#'shared-initialize (,instance ,added-local-slots
                                             ,@initargs)))
                    initargs)
    (apply #'shared-initialize instance added-local-slots initargs)))

(defun obsolete-instance-p (instance)
  (/= (%standard-object-version instance)
      (class-version (class-of instance))))

(defun update-obsolete-instance (instance)
  (let* ((class (class-of instance))
         (old-class (aref (class-old-classes class)
                          (%standard-object-version instance)))
         (old-instance (allocate-instance class)))
    (swap-%standard-object instance old-instance)
    (setf (%standard-object-class old-instance) old-class)
    (let* ((old (class-slot-names old-class))
           (new (class-slot-names class))
           (common (intersection old new))
           (discarded (set-difference old new))
           (added (set-difference new old)))
      (mapc #'(lambda (name)
                ;;  slots of the same name
                ;;  old         current    slot value/unbound state
                ;;  local       shared     discarded
                ;;  shared      shared     retained
                ;;  local       local      retained
                ;;  shared      local      retained
                (if (and (local-slot-p (find-slot old-class name))
                         (not (local-slot-p (find-slot class name))))
                    (push name discarded)
                    (if (slot-boundp old-instance name)
                        (setf (slot-value instance name)
                              (slot-value old-instance name))
                        (slot-makunbound instance name))))
            common)
      (let ((plist (loop for name in discarded
                         when (slot-boundp old-instance name)
                         nconc `(,name ,(slot-value old-instance name)))))
        (update-instance-for-redefined-class instance added discarded plist)))))

(defun update-instance-if-obsolete (instance)
  (when (obsolete-instance-p object) (update-obsolete-instance object)))



(defgeneric initialize-instance (instance &key &allow-other-keys)) ; clos
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))
(defmethod initialize-instance :after ((instance class) &rest initargs)
  (mapc #'(lambda (super) (add-direct-subclass super instance))
        (class-direct-superclasses instance)))


(defgeneric make-instance (class &rest initargs &key &allow-other-keys)
  (:documentation "Create and return a new instance of CLASS.")) ; clos
(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))
(defmethod make-instance ((class funcallable-standard-class) &rest initargs)
  (push +funcallable-instance-function-slot-spec+ (getf initargs :direct-slots))
  (apply #'call-next-method class ,@initargs))
(defmethod make-instance ((class standard-base-class) &rest initargs)
  (let ((instance (apply #'allocate-instance class initargs))
        (defaulted-initargs (defaulted-initargs class initargs)))
    (check-initargs instance
                    `((,#'allocate-instance (,class ,@defaulted-initargs))
                      (,#'initialize-instance (,instance ,@defaulted-initargs))
                      (,#'shared-initialize (,instance t ,@defaulted-initargs)))
                    defaulted-initargs)
    (apply #'initialize-instance instance defaulted-initargs)))
(defmethod make-instance ((class old-class) &rest initargs
                          &key (current-class (required-argument)))
  (let ((instance (apply #'allocate-instance class initargs)))
    (check-initargs initargs `(,#'make-instance (,class ,@initargs) initargs))
    (setf (slot-value instance 'name)    (slot-value current-class 'name))
    (setf (slot-value instance 'version) (slot-value current-class 'version))
    (setf (slot-value instance 'slots)   (copy-seq (slot-value current-class
                                                               'slots)))
    instance))


(defgeneric class-name (class)          ; clos
  (:documentation "Return the name of CLASS."))
(defgeneric (setf class-name) (name class) ; clos
  (:documentation "Set the name of CLASS to NAME."))
(defgeneric class-direct-slots (class)) ; mop
(defgeneric class-direct-default-initargs (class)) ; mop
(defgeneric class-direct-superclasses (class)) ; mop
(defgeneric class-direct-subclasses (class)) ; mop
(defgeneric class-precedence-list (class)) ; mop
(defgeneric class-default-initargs (class)) ; mop
(defgeneric class-default-initfuncs (class))
(defgeneric class-slots (class))        ; mop
(defgeneric class-finalized-p (class))  ; mop
(defgeneric class-prototype (class))    ; mop

(defmethod class-precedence-list :before ((class standard-base-class))
  (unless (class-finalized-p class) (finalize-inheritance class)))
(defmethod class-slots :before ((class standard-base-class))
  (unless (class-finalized-p class) (finalize-inheritance class)))
(defmethod class-default-initargs :before ((class standard-base-class))
  (unless (class-finalized-p class) (finalize-inheritance class)))
(defmethod class-default-initfuncs :before ((class standard-base-class))
  (unless (class-finalized-p class) (finalize-inheritance class)))


    

(defmethod documentation ((x standard-base-class) (doc-type (eql 't)))
  (slot-value x 'documentation))
(defmethod documentation ((x standard-base-class) (doc-type (eql 'type)))
  (slot-value x 'documentation))




(defgeneric direct-slot-definition-class (class &rest initargs)) ; mop
(defmethod direct-slot-definition-class ((class standard-base-class) &rest initargs)
  (find-class 'standard-direct-slot-definition))



(defgeneric specializer-direct-methods (specializer)) ; mop
(defgeneric specializer-direct-generic-functions (specializer)) ; mop


(defgeneric add-direct-method (specializer method)) ; mop
(defmethod add-direct-method ((specializer specializer) (method method))
  (pushnew method (slot-value specializer 'direct-methods))
  (pushnew (method-generic-function method)
           (slot-value specializer 'direct-generic-functions)))

(defgeneric remove-direct-method (specializer method)) ; mop
(defmethod remove-direct-method ((specializer specializer) (method method))
  (setf (slot-value specializer 'direct-methods)
        (remove method (specializer-direct-methods specializer)))
  (let ((gf (method-generic-function method)))
    (unless (member gf (mapcar #'method-generic-function
                               (specializer-direct-methods specializer)))
      (setf (slot-value specializer 'direct-generic-functions)
            (remove gf (specializer-direct-generic-functions))))))
          

(defgeneric reader-method-class (class direct-slot &rest initargs)) ; mop
(defmethod reader-method-class ((class standard-base-class) (direct-slot standard-direct-slot-definition) &rest initargs)
  (find-class 'standard-reader-method))

(defgeneric writer-method-class (class direct-slot &rest initargs)) ; mop
(defmethod writer-method-class ((class standard-base-class) (direct-slot standard-direct-slot-definition) &rest initargs)
  (find-class 'standard-writer-method))


(defgeneric add-dependent (metaobject dependent)) ; mop
(defmethod add-dependent ((class standard-base-class) dependent)
  (pushnew dependent (slot-value class 'dependents))) 
(defmethod add-dependent ((generic-function standard-generic-function) dependent)
  (pushnew dependent (slot-value generic-function 'dependents)))
(defgeneric remove-dependent (metaobject dependent)) ; mop
(defmethod remove-dependent ((class standard-base-class) dependent)
  (setf (slot-value class 'dependents)
        (remove dependent (slot-value class 'dependents))))
(defmethod remove-dependent
    ((generic-function standard-generic-function) dependent)
  (setf (slot-value generic-function 'dependents)
        (remove dependent (slot-value generic-function 'dependents))))

(defgeneric map-dependents (metaobject function)) ; mop
(defmethod map-dependents ((metaobject standard-base-class) function)
  (mapc function (slot-value metaobject 'dependents)))
(defmethod map-dependents ((metaobject standard-generic-function) function)
  (mapc function (slot-value metaobject 'dependents)))
(defgeneric update-dependent (metaobject dependent &rest initargs)) ; mop



(defgeneric make-load-form (object &optional environment)
  (:documentation ;; clos
   "Return forms to enable load to construct an object equivalent to OBJECT."))
(defmethod make-load-form ((object standard-object) &optional environment)
  )
(defmethod make-load-form ((object structure-object) &optional environment)
  )
(defmethod make-load-form ((object condition) &optional environment)
  )
(defmethod make-load-form ((object class) &optional environment)
  )

(defun make-load-form-saving-slots (object &key slot-names environment) ; clos
  "Return forms that will construct an object equivalent to OBJECT."
  )






(defmacro with-accessors (slot-entries instance &body body) ; clos
  "Make slots accessible like variables through specified accessors."
  (let ((instance (gensym)))
    `(let ((,instance ,instance-form))
      (symbol-macrolet (,@(mapcar
                           #'(lambda (entry)
                               `(,(first entry) (,(second entry) ,instance)))
                           slot-entries))
          ,@body))))

(defmacro with-slots (slot-entries instance-form &body body) ; clos
  "Create a lexical environment where slots are accessible like variables."
  (let ((instance (gensym)))
    `(let ((,instance ,instance-form))
      (symbol-macrolet (,@(mapcar
                           #'(lambda (entry)
                               (if (symbolp entry)
                                   `(,entry (slot-value ,instance ',entry))
                                   `(,(first entry)
                                     (slot-value ,instance ',(second entry)))))
                           slot-entries))
          ,@body))))


(defun standard-instance-access (instance location) ; mop
  (let ((storage (%standard-object-storage instance)))
    (if (frozen-class-instance-p instance)
        (second (nth location storage))
        (aref storage location))))

(defun funcallable-standard-instance-access (instance location)
  (standard-instance-access instance location))

(defun refer-slot-using-class (class object slot)
  (update-instance-if-obsolete object)
  (if (local-slot-p slot)
      (let (location (slot-definition-location slot))
        (aref (%standard-object-storage object) location))
      (car (slot-definition-shared-binding slot))))

(defgeneric slot-value-using-class (class object slot)) ; mop
(defmethod slot-value-using-class ((class standard-base-class) object
                                   (slot standard-effective-slot-definition))
  (let* ((value (refer-slot-using-class class object slot)))
    (if (eq value +unbound-state+)
        (values (slot-unbound class object (slot-definition-name slot)))
        value)))
(defmethod slot-value-using-class ((class built-in-class) object slot)
  (error "slot-value-using-class cannot be used for a built-in-class object."))



(defgeneric (setf slot-value-using-class) (new-value class object slot)) ; mop
(defmethod (setf slot-value-using-class) (new-value (class standard-base-class) object (slot standard-effective-slot-definition))
  (update-instance-if-obsolete object)
  (if (local-slot-p slot)
      (let ((location (slot-definition-location slot)))
        (setf (aref (%standard-object-storage object) location) new-value))
      (setf (car (slot-definition-shared-binding slot)) new-value)))
(defmethod (setf slot-value-using-class)
    (new-value (class built-in-class) object slot)
  (error "(setf slot-value-using-class) cannot be used for ~
          a built-in-class object."))



(defgeneric slot-exists-p-using-class (class object slot-name)) ; mop?
(defmethod slot-exists-p-using-class ((class standard-base-class)
                                      object slot-name)
  (find-slot class slot-name))
(defun slot-exists-p (object slot-name) ; clos
  "Return true if OBJECT has a slot named SLOT-NAME."
  (slot-exists-p-using-class (class-of object) object slot-name))





(defgeneric slot-boundp-using-class (class object slot)) ; mop
(defmethod slot-boundp-using-class ((class standard-base-class) object (slot standard-effective-slot-definition))
  (not (eq (refer-slot-using-class class object slot) +unbound-state+)))

(defmethod slot-boundp-using-class ((class built-in-class) object slot)
  (error "slot-boundp-using-class cannot be used for a built-in-class object."))


(defun directly-accessible-slot-p (slot)
  ;; http://www.lisp.org/mop/concepts.html#instance-structure-protocol
  ;; > In particular, portable programs can control the implementation
  ;; > of, and obtain direct access to, slots with allocation :instance and
  ;; > type t. These are called directly accessible slots.
  (and (eq (slot-definition-allocation slot) :instance)
       (eq (slot-definition-type       slot) 't)))


(defgeneric slot-makunbound-using-class (class object slot)) ; mop
(defmethod slot-makunbound-using-class ((class standard-base-class) object (slot standard-effective-slot-definition))
  (setf (slot-value-using-class class object slot) +unbound-state+))
(defmethod slot-makunbound-using-class ((class built-in-class) object slot)
  (error
   "slot-makunbound-using-class cannot be used for a built-in-class object."))
(defun slot-makunbound (instance slot-name) ; clos
  "Restore a slot of the name SLOT-NAME in INSTANCE to the unbound state."
  (let* ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (slot-makunbound-using-class class instance slot)
        (slot-missing class instance slot-name 'slot-makunbound))
    instance))

(defgeneric slot-missing (class object slot-name operation &optional new-value)
  (:documentation ;; clos
   "Invoked when a slot not defined in CLASS is accessed by SLOT-NAME."))
(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
  (error "The slot ~S is missing in the class ~S."
         slot-name
         (etypecase class (symbol class) (class (class-name class)))))

(defgeneric slot-unbound (class instance slot-name)
  (:documentation ;; clos
   "Called when an unbound slot named SLOT-NAME is read in INSTANCE of CLASS."))
(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :instance instance :name slot-name))






(defgeneric ensure-generic-function-using-class (generic-function  function-name &key argument-precedence-order declarations documentation generic-function-class lambda-list method-class method-combination name &allow-other-keys)) ; mop
(defmethod ensure-generic-function-using-class ((generic-function generic-function) function-name &rest initargs &key generic-function-class &allow-other-keys)
  (apply #'reinitialize-instance generic-function initargs))
(defmethod ensure-generic-function-using-class ((generic-function null) function-name &rest initargs &key generic-function-class &allow-other-keys)
  (apply #'make-instance generic-function-class initargs))

(defun ensure-generic-function (function-name &rest initargs
                                &key argument-precedence-order declare
                                documentation environment generic-function-class
                                lambda-list method-class method-combination)
  "Define a globally named generic function with no methods." ; clos
  (let ((fdefinition (fdefinition function-name)))
    (when (and fdefinition (not (instancep fdefinition 'generic-function)))
      (error "~S already names an ordinary function or a macro." function-name))
    (loop initially (setq initargs (copy-list initargs))
          while (remf initargs :declare))
    (apply #'ensure-generic-function-using-class fdefinition
           `(:declarations ,declare ,@initargs))))


(defun (setf generic-function-name) (new-name generic-function)
  )



(defun clear-gf-cache (gf)
  (clrhash (generic-function-applicable-methods gf))
  (clrhash (generic-function-effective-methods gf)))


(defgeneric method-qualifiers (method)  ; clos
  (:documentation "Return a list of the qualifiers of METHOD."))




(defun check-specialized-lambda-list (specialized-lambda-list)
  (let ((required-part (extract-required-part specialized-lambda-list)))
    (assert (plusp (length required-part)))
    (dolist (var-spec required-part)
      (etypecase var-spec
        (symbol (check-variable-name var-speck))
        (cons (let ((variable-name (first var-spec))
                    (parameter-specializer-name (second var-spec)))
                (check-variable-name variable-name)
                (assert (or (symbolp parameter-specializer-name)
                            (eq (car parameter-specializer-name) 'eql)))))))))

(defun extract-lambda-list (specialized-lambda-list) ; mop
  (check-specialized-lambda-list specialized-lambda-list)
  (loop for rest on specialized-lambda-list
        for item = (car rest)
        if (member item '(&optional &rest &key &aux))
        append rest and do (loop-finish)
        else collect (if (consp item) (car item) item)))

(defun extract-specializer-names (specialized-lambda-list) ; mop
  (check-specialized-lambda-list specialized-lambda-list)
  (loop for item in specialized-lambda-list
        if (member item '(&optional &rest &key &aux)) do (loop-finish)
        else collect (if (consp item) (second item) 't)))

(defun extract-keyword-names (specialized-lambda-list)
  (check-specialized-lambda-list specialized-lambda-list)
  (let ((allow-other-keys-p nil))
    (values (loop for item in (rest (member '&key specialized-lambda-list))
                  if (eq item '&allow-other-keys)
                  do (setq allow-other-keys-p t) (loop-finish)
                  else if (eq item '&aux) do (loop-finish)
                  else collect (if (consp item)
                                   (if (consp (car item))
                                       (caar item)
                                       (%keyword (car item)))
                                   (%keyword item)))
            allow-other-keys-p)))

(defgeneric function-keywords (method)  ; clos
  (:documentation "Return the keyword parameter specifiers for METHOD."))
(defmethod function-keywords ((method standard-method))
  (extract-keyword-names (method-specialized-lambda-list method)))

(defgeneric no-applicable-method (generic-function &rest function-arguments)
  (:documentation ;; clos
   "Called when GENERIC-FUNCTION is invoked and no method is applicable."))
(defmethod no-applicable-method ((generic-function t) &rest function-arguments)
  )

(defgeneric no-next-method (generic-function method &rest args) ; clos
  (:documentation "Called by call-next-method when there is no next method."))
(defmethod no-next-method ((generic-function standard-generic-function) (method standard-method) &rest args)
  )



(defgeneric find-method-combination     ; mop
    (generic-function method-combination-type-name method-combination-arguments))
(defmethod find-method-combination
    ((gf standard-generic-function) method-combination-type combination-options)
  (multiple-value-bind (type presentp)
      (gethash method-combination-type *method-combination-types*)
    (if presentp
        (make-instance 'standard-method-combination
                       :type type :arguments method-combination-arguments)
        (error "Method combination ~S does not exist." method-combination-type))))

(defun make-method-form-p (object)
  (and (consp object) (eq 'make-method (first object))))

(defun make-method-description (gf form)
  `(:method 'make-method ,(generic-function-lambda-list gf)
    (with-call-method ,gf ,form)))

;; Local Macro CALL-METHOD, MAKE-METHOD
(defmacro with-call-method (gf &body body)
  `(macrolet
    ((call-method (method next-methods &environment env)
      (flet ((method-form (form)
               (apply #'<make-instance>
                      (generic-function-method-class ,gf)
                      (method-initargs-form
                       ',gf env :null-lexical-environment-p t
                       (method-spec (make-method-description ,gf form))))))
        (when (make-method-form-p method)
          (setq method (method-form (second method))))
        (setq next-methods
              (mapcar #'(lambda (method)
                          (if (make-method-form-p method)
                              (method-form (second method))
                              `(quote ,method)))
                      next-methods)))
      `(funcall (method-function ,method)
        ,+gf-args-variable+ (list ,@next-methods))))
    ,@body))

(defgeneric compute-effective-method (generic-function
                                      method-combination methods)) ; mop
(defmethod compute-effective-method ((generic-function standard-generic-function)
                                     method-combination methods)
  (let* ((type (method-combination-type method-combination))
         (type-function (method-combination-type-function type))
         (arguments (method-combination-arguments method-combination))
         (effective-method
          (apply type-function generic-function methods arguments)))
    (values `(with-call-method ,generic-function
              ,effective-method)
            `(:arguments ,(method-combination-type-args-lambda-list type)
              :generic-function
              ,(method-combination-type-generic-function-symbol type)))))


;; Local Function NEXT-METHOD-P
;; Local Function CALL-NEXT-METHOD
(defgeneric make-method-lambda
    (generic-function method lambda-expression environment)) ; mop
(defmethod make-method-lambda ((generic-function standard-generic-function)
                               (method standard-method)
                               lambda-expression environment)
  (let* ((lambda-list (second lambda-expression))
         (gf-args (first lambda-list))
         (next-methods (second lambda-list))
         (name (generic-function-name generic-function))
         (args (gensym)))
    (multiple-value-bind (decls forms)
        (declarations-and-forms (cddr lambda-expression))
      `(lambda ,lambda-list
        ,@decls
        (block ,(if (symbolp name) name (second name))
          (labels ((next-method-p () ,next-methods)
                   (call-next-method (&rest ,args)
                     (unless ,args (setq ,args ,gf-args))
                     (if (next-method-p)
                         (funcall (method-function (car ,next-methods))
                                  ,args (cdr ,next-methods))
                         (apply #'no-next-method ,generic-function ,method
                                ,(first lambda-list)))))
            ,@forms))))))


(defgeneric specializer-satisfied-p (specifier arg))
(defmethod specializer-satisfied-p ((specializer class) arg)
  (member (class-of arg) (class-precedence-list specializer)))
(defmethod specializer-satisfied-p ((specializer eql-specializer) arg)
  (eql arg (eql-specializer-object specializer)))

(defun applicable-method-p (method args)
  (every #'specializer-satisfied-p (method-specializers method) args))

(defun eql-specializer-p (specializer)
  (eq (class-of specializer) (find-class 'eql-specializer)))

(defun more-specific-specializer-p (a b arg)
  (cond
    ((eql-specializer-p a) (not (eql-specializer-p b)))
    ((eql-specializer-p b) nil)
    (t (let ((list (class-precedence-list (class-of arg))))
         (< (position a list) (position b list))))))

(defun sort-methods (gf methods)
  (let ((indeces (loop for arg in (generic-function-argument-precedence-order gf)
                       collect (position arg
                                         (generic-function-lambda-list gf)))))
    (flet ((more-specific-method-p (a b)
             (loop for i in indeces
                   if (more-specific-specializer-p (elt (method-specializers a) i)
                                                   (elt (method-specializers b) i)
                                                   (elt args i))
                   return t)))
      (sort methods #'more-specific-method-p))))

(defgeneric compute-applicable-methods (generic-function function-arguments) ;clos
  (:documentation "Return the set of applicable methods of GENERIC-FUNCTION. "))
(defmethod compute-applicable-methods ((gf standard-generic-function) args)
  (let ((methods (mapcan #'(lambda (method)
                             (when (applicable-method-p method args)
                               (list method)))
                         (generic-function-methods gf))))
    (sort-methods gf methods)))

(defgeneric compute-applicable-methods-using-classes
    (generic-function classes))         ; mop
(defmethod compute-applicable-methods-using-classes
    ((generic-function standard-generic-function) classes)
  (flet ((filter (method)
           (let ((eql-specializer-p nil))
             (when (every #'(lambda (a b)
                              (if (and (eql-specializer-p a)
                                       (eq (class-of (eql-specializer-object a))
                                           b))
                                  (setq eql-specializer-p t)
                                  (subclassp b a)))
                          (method-specializers method)
                          classes)
               (if eql-specializer-p
                   (return-from compute-applicable-methods-using-classes
                     (values nil nil))
                   (list method))))))
    (values (sort-methods gf (mapcan #'filter (generic-function-methods gf)))
            t)))

(defun applicable-methods (gf args)
  (let ((classes (mapcar #'class-of
                         (subseq args 0 (number-of-required-args gf)))))
    (multiple-value-bind (methods presentp)
        (gethash classes (generic-function-applicable-methods gf))
      (if presentp
          methods
          (multiple-value-bind (methods memorablep)
              (compute-applicable-methods-using-classes gf classes)
            (if memorablep
                (setf (gethash classes (generic-function-applicable-methods gf))
                      methods)
                (compute-applicable-methods gf args)))))))

(defgeneric compute-discriminating-function (generic-function)) ; mop
(defmethod compute-discriminating-function ((gf standard-generic-function))
  (let* ((combination (slot-value gf 'method-combination))
         (methods (gensym)))
    (compile
     nil
     `(lambda (&rest ,+gf-args-variable+)
       ;; check args here.

       (let ((,methods (applicable-methods ,gf ,+gf-args-variable+)))
         (if (null ,methods)
             (apply #'no-applicable-method ,gf ,+gf-args-variable+)
             (multiple-value-bind (effective-method-function presentp)
                 (gethash methods (generic-function-effective-methods gf))
               (if presentp
                   (funcall effective-method-function ,+gf-args-variable+)
                   (progn
                     (setq effective-method-function
                           (compile
                            nil
                            (eval '(lambda (,+gf-args-variable+)
                                    ,(compute-effective-method gf
                                                               combination
                                                               methods)))))
                     (setf (gethash methods
                                    (generic-function-effective-methods gf))
                           effective-method-function)
                     (funcall effective-method-function
                              ,+gf-args-variable+))))))))))

(defgeneric find-method
    (generic-function qualifiers specializers &optional errorp)
  (:documentation ;; clos
   "Return the method object that agrees on QUALIFIERS and SPECIALIZERS."))
(defmethod find-method
    ((gf standard-generic-function) qualifiers specializers &optional errorp)
  (when (/= (length specializers) (number-of-required-args gf))
    (error "The lambda list of ~S is ~S, and it doesn't match specializers ~S."
           gf (generic-function-lambda-list gf) specializers))
  (flet ((agreep (a b)
           (if (eql-specializer-p a)
               (and (eql-specializer-p b)
                    (eql (eql-specializer-object a) (eql-specializer-object b)))
               (eq a b))))
    (let ((method (find-if
                   #'(lambda (method)
                       (and (agreep (method-specializers method) specializers)
                            (equal (method-qualifiers method) qualifiers)))
                   (generic-function-methods generic-function))))
      (or method
          (when errorp
            (error "No method for ~S with qualifiers ~S and specializers ~S."
                   gf qualifiers specializers))))))

(defun check-lambda-list-congruence (gf method)
  )

(defgeneric add-method (generic-function method) ; clos
  (:documentation "Add METHOD to GENERIC-FUNCTION."))
(defmethod add-method
    ((generic-function standard-generic-function) (method method))
  (when (method-generic-function method)
    (error "Method ~S is already associated with generic function ~S"
           method (method-generic-function method)))
  (check-lambda-list-congruence generic-function method)

  (let ((old-method (find-method generic-function
                                 (method-qualifiers method)
                                 (method-specializers method))))
    (when old-method
      (remove-method generic-function old-method)))
  (push method (slot-value generic-function 'methods))
  (mapc #'(lambda (specializer) (add-direct-method specializer method))
        (method-specializers method))
  (clear-gf-cache gf)
  (map-dependents generic-function
                  #'(lambda (dependent)
                      (update-dependent generic-function dependent
                                        'add-method method)))
  method)

(defgeneric remove-method (generic-function method) ; clos
  (:documentation "Remove METHOD from GENERIC-FUNCTION by modifying it"))
(defmethod remove-method ((generic-function standard-generic-function) method)
  (let ((gf-methods (generic-function-methods generic-function)))
    (when (member method gf-methods)
      (setf (slot-value generic-function 'methods) (remove method gf-methods))
      (mapc #'(lambda (specializer) (remove-direct-method specializer method))
            (method-specializers method))))
  (clear-gf-cache gf)
  (map-dependents generic-function
                  #'(lambda (dependent)
                      (update-dependent generic-function dependent
                                        'remove-method method)))
  method)



(defun class-of-sequence (sequence)
  (typecase sequence
    (list (typecase sequence
            (null (find-class 'null))
            (cons (find-class 'cons))
            (t    (find-class 'list))))
    (vector (typecase sequence
              (bit-vector (find-class 'bit-vector))
              (string     (find-class 'string))
              (t          (find-class 'vector))))
    (t (find-class 'sequence))))

(defun class-of-number (number)
  (typecase number
    (integer  (find-class 'integer))
    (ratio    (find-class 'ratio))
    (rational (find-class 'rational))
    (float    (find-class 'float))
    (real     (find-class 'real))
    (complex  (find-class 'complex))
    (t        (find-class 'number))))

(defun class-of-stream (stream)
  (typecase stream
    (broadcast-stream    (find-class 'broadcast-stream))
    (concatenated-stream (find-class 'concatenated-stream))
    (string-stream       (find-class 'string-stream))
    (echo-stream         (find-class 'echo-stream))
    (synonym-stream      (find-class 'synonym-stream))
    (file-stream         (find-class 'file-stream))
    (two-way-stream      (find-class 'two-way-stream))
    (t                   (find-class 'stream))))

(defun class-of-condition (condition)
  (typecase condition
    (simple-error      (find-class 'simple-error))
    (simple-type-error (find-class 'simple-type-error))
    (simple-warning    (find-class 'simple-warning))
    (simple-condition  (find-class 'simple-condition))

    (floating-point-inexact           (find-class 'floating-point-inexact))
    (floating-point-invalid-operation (find-class
                                       'floating-point-invalid-operation))
    (floating-point-overflow          (find-class 'floating-point-overflow))
    (floating-point-underflow         (find-class 'floating-point-underflow))
    (division-by-zero                 (find-class 'division-by-zero))
    (arithmetic-error                 (find-class 'arithmetic-error))

    (reader-error (find-class 'reader-error))
    (parse-error  (find-class 'parse-error))

    (end-of-file  (find-class 'end-of-file))
    (stream-error (find-class 'stream-error))

    (unbound-slot       (find-class 'unbound-slot))
    (unbound-variable   (find-class 'unbound-variable))
    (undefined-function (find-class 'undefined-function))
    (cell-error         (find-class 'cell-error))

    (type-error         (find-class 'type-error))
    (package-error      (find-class 'package-error))
    (control-error      (find-class 'control-error))
    (print-not-readable (find-class 'print-not-readable))
    (program-error      (find-class 'program-error))
    (file-error         (find-class 'file-error))
    (error              (find-class 'error))
    
    (storage-condition (find-class 'storage-condition))
    (serious-condition (find-class 'serious-condition))

    (style-warning (find-class 'style-warning))
    (warning       (find-class 'warning))

    (t (find-class 'condition))))


(progn
  (setq *mop-working-p* t))



(defconstant +condition-report-slot-name+
  (gensym "CONDITION-REPORT-SLOT-NAME-"))

(deftype lambda-expression () '(satisfies lambda-expression-p))
(defun lambda-expression-p (object)
  (and (consp object) (eq (first object) 'lambda) (listp (second object))))

(defun condition-initargs-form (options)
  (check-singleton-options options '(:default-initargs :documentation :report
                                     :direct-superclasses :direct-slots))
  (let ((report-option (assoc :report options)))
    (when report-option
      (destructuring-bind (report-name) (cdr report-option)
        (assert (typep report-name (or string symbol lambda-expression)))
        (let ((slot-spec `(,+condition-report-slot-name+ :allocation :class
                           :initform ',report-name)))
          (push slot-spec (cdr (assoc :direct-slots options)))))
      (setq options (remove :report options :key #'car))))
  (class-initargs-form options))

(defmacro define-condition (name parent-types slot-specs &rest options)
  (let ((*message-prefix* (format nil "DEFINE-CONDITION ~S: " name))
        (options `((:direct-superclasses ,@(or parent-types '(condition)))
                   (:direct-slots ,@slot-specs)
                   ,@options)))
    `(let ((*message-prefix* ,*message-prefix*))
      (apply #'ensure-class ',name ,(condition-initargs-form options)))))

;; 9.1.1 Condition Types
;; http://www.lispworks.com/reference/HyperSpec/Body/09_aa.htm
;; > The metaclass of the class condition is not specified.
(define-condition condition (t) ()
  (:report (lambda (condition stream)
             (format stream "Condition ~S is signaled." (class-of condition)))))

(defun make-condition (type &rest slot-initializations)
  )

|#