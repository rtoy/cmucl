;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; This file isolates all the backend specific data so that we can compile
;;; and use different backends.
;;; 
;;; Written by William Lott.
;;;
(in-package "C")

(export '(def-vm-support-routine *backend* *target-backend* *native-backend*
	   backend-name backend-version backend-fasl-file-type
	   backend-fasl-file-implementation backend-fasl-file-version
	   backend-register-save-penalty backend-byte-order
	   backend-any-primitive-type

	   ;; The various backends need to call these support routines
	   make-stack-pointer-tn primitive-type primitive-type-of))


;;;; VM support routine stuff.

(eval-when (compile load eval)
  (defvar *vm-support-routines* nil))

(defmacro def-vm-support-routine (name ll &body body)
  (unless (member (string name) *vm-support-routines*
		  :test #'string=)
    (warn "Unknown VM support routine: ~A" name))
  (let ((local-name (symbolicate (backend-name *backend*) "-" name)))
    `(progn
       (defun ,local-name ,ll ,@body)
       (setf (,(intern (concatenate 'simple-string "BACKEND-" (string name))
		       (symbol-package 'foo))
	      *backend*)
	     #',local-name))))

(eval-when (compile eval)

(defmacro def-vm-support-routine-stub (name)
  `(progn
     #-bootstrap-backend
     (defun ,name (&rest args)
       (apply (or (,(symbolicate "BACKEND-" name) *backend*)
		  (error "Machine specific support routine ~S undefined for ~S"
			 ',name *backend*))
	      args))
     (eval-when (compile load eval)
       (pushnew ,(string name) *vm-support-routines*
		:test #'string=))))

); eval-when (compile eval)

;;; From VM.LISP
(def-vm-support-routine-stub immediate-constant-sc)
(def-vm-support-routine-stub location-print-name)

;;; From PRIMTYPE.LISP
(def-vm-support-routine-stub primitive-type-of)
(def-vm-support-routine-stub primitive-type)

;;; From C-CALL.LISP
(def-vm-support-routine-stub make-call-out-nsp-tn)
(def-vm-support-routine-stub make-call-out-argument-tns)
(def-vm-support-routine-stub make-call-out-result-tn)

;;; From CALL.LISP
(def-vm-support-routine-stub standard-argument-location)
(def-vm-support-routine-stub make-return-pc-passing-location)
(def-vm-support-routine-stub make-old-fp-passing-location)
(def-vm-support-routine-stub make-old-fp-save-location)
(def-vm-support-routine-stub make-return-pc-save-location)
(def-vm-support-routine-stub make-argument-count-location)
(def-vm-support-routine-stub make-nfp-tn)
(def-vm-support-routine-stub make-stack-pointer-tn)
(def-vm-support-routine-stub make-number-stack-pointer-tn)
(def-vm-support-routine-stub make-unknown-values-locations)
(def-vm-support-routine-stub select-component-format)

;;; From NLX.LISP
(def-vm-support-routine-stub make-nlx-sp-tn)
(def-vm-support-routine-stub make-dynamic-state-tns)

;;; From SUPPORT.LISP
(def-vm-support-routine-stub generate-call-sequence)
(def-vm-support-routine-stub generate-return-sequence)



;;;; The actual backend structure.

(defstruct (backend
	    (:print-function %print-backend))
  ;; The name of this backend.  Something like ``PMAX''
  (name nil)

  ;; The version string for this backend.
  ;; Something like ``DECstation 3100/Mach 0.0''
  (version nil)

  ;; Information about fasl files for this backend.
  (fasl-file-type nil)
  (fasl-file-implementation nil)
  (fasl-file-version nil)

  ;; The number of references that a TN must have to offset the overhead of
  ;; saving the TN across a call.
  (register-save-penalty 0)

  ;; The byte order of the target machine.  Should either be :big-endian
  ;; which has the MSB first (RT) or :little-endian which has the MSB last
  ;; (VAX).
  (byte-order nil :type (or null (member :little-endian :big-endian)))

  ;; Translates from SC numbers to SC info structures.  SC numbers are always
  ;; used instead of names at run time, so changing this vector changes all the
  ;; references.
  (sc-numbers (make-array sc-number-limit) :type sc-vector)

  ;; A list of all the SBs defined, so that we can easily iterate over them.
  (sb-list () :type list)

  ;; Translates from template names to template structures.
  (template-names (make-hash-table :test #'eq) :type hash-table)

  ;; Hashtable from SC and SB names the corresponding structures.  The META
  ;; versions are only used at meta-compile and load times, so the defining
  ;; macros can change these at meta-compile time without breaking the
  ;; compiler.
  (sc-names (make-hash-table :test #'eq) :type hash-table)
  (sb-names (make-hash-table :test #'eq) :type hash-table)
  (meta-sc-names (make-hash-table :test #'eq) :type hash-table)
  (meta-sb-names (make-hash-table :test #'eq) :type hash-table)

  ;; Like *SC-Numbers*, but is updated at meta-compile time.
  (meta-sc-numbers (make-array sc-number-limit) :type sc-vector)

  ;; Translates from primitive type names to the corresponding primitive-type
  ;; structure.
  (primitive-type-names (make-hash-table :test #'eq) :type hash-table)

  ;; Establishes a convenient handle on primitive type unions, or whatever.
  ;; These names can only be used as the :arg-types or :result-types for VOPs
  ;; and can map to anything else that can be used as :arg-types or
  ;; :result-types (e.g. :or, :constant).
  (primitive-type-aliases (make-hash-table :test #'eq) :type hash-table)

  ;; Meta-compile time translation from names to primitive types.
  (meta-primitive-type-names (make-hash-table :test #'eq) :type hash-table)

  ;; The primitive type T is somewhat magical, in that it is the only
  ;; primitive type that overlaps with other primitive types.  An object
  ;; of primitive-type T is in the canonical descriptor (boxed or pointer)
  ;; representation.
  ;;
  ;; We stick the T primitive-type in a variable so that people who have to
  ;; special-case it can get at it conveniently.  This is done by the machine
  ;; specific VM definition, since the DEF-PRIMITIVE-TYPE for T must specify
  ;; the SCs that boxed objects can be allocated in.
  (any-primitive-type nil :type (or null primitive-type))

  ;; Hashtable translating from VOP names to the corresponding VOP-Parse
  ;; structures.  This information is only used at meta-compile time.
  (parsed-vops (make-hash-table :test #'eq) :type hash-table)

  . #.(mapcar #'(lambda (slot)
		  `(,(intern slot) nil :type (or null function)))
	      (sort (copy-list *vm-support-routines*) #'string<)))

(defprinter backend
  name)


(defvar *native-backend* (make-backend)
  "The backend for the machine we are running on. Do not change this.")
(defvar *target-backend* *native-backend*
  "The backend we are attempting to compile.")
(defvar *backend* *native-backend*
  "The backend we are using to compile.")
