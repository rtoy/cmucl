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
	   backend-any-primitive-type backend-info-environment
	   backend-instruction-formats backend-instruction-flavors
	   backend-assembler-resources backend-special-arg-types
	   backend-features

	   ;; The various backends need to call these support routines
	   make-stack-pointer-tn primitive-type primitive-type-of))


;;;; VM support routine stuff.

(eval-when (compile eval load)

(defconstant vm-support-routines
  '(;; From VM.LISP
    immediate-constant-sc
    location-print-name
    
    ;; From PRIMTYPE.LISP
    primitive-type-of
    primitive-type
    
    ;; From C-CALL.LISP
    make-call-out-nsp-tn
    make-call-out-argument-tns
    make-call-out-result-tn
    
    ;; From CALL.LISP
    standard-argument-location
    make-return-pc-passing-location
    make-old-fp-passing-location
    make-old-fp-save-location
    make-return-pc-save-location
    make-argument-count-location
    make-nfp-tn
    make-stack-pointer-tn
    make-number-stack-pointer-tn
    make-unknown-values-locations
    select-component-format
    
    ;; From NLX.LISP
    make-nlx-sp-tn
    make-dynamic-state-tns
    
    ;; From SUPPORT.LISP
    generate-call-sequence
    generate-return-sequence))

); eval-when


(defmacro def-vm-support-routine (name ll &body body)
  (unless (member (intern (string name) (find-package "C"))
		  vm-support-routines)
    (warn "Unknown VM support routine: ~A" name))
  (let ((local-name (symbolicate (backend-name *target-backend*) "-" name)))
    `(progn
       (defun ,local-name ,ll ,@body)
       (setf (,(intern (concatenate 'simple-string "BACKEND-" (string name))
		       (find-package "C"))
	      *target-backend*)
	     #',local-name))))



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
  (sc-numbers (make-array sc-number-limit :initial-element nil)
	      :type sc-vector)

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
  (meta-sc-numbers (make-array sc-number-limit :initial-element nil)
		   :type sc-vector)

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

  ;; The backend specific aspects of the info environment.
  (info-environment nil :type list)

  ;; Support for the assembler.
  (instruction-formats (make-hash-table :test #'eq) :type hash-table)
  (instruction-flavors (make-hash-table :test #'equal) :type hash-table)
  (special-arg-types (make-hash-table :test #'eq) :type hash-table)
  (assembler-resources nil :type list)

  ;; The backend specific features list, if any.
  (features nil :type list)

  . #.(mapcar #'(lambda (slot)
		  `(,slot nil :type (or null function)))
	      (sort (copy-list vm-support-routines)
		    #'string<
		    :key #'symbol-name)))


(defprinter backend
  name)


(defvar *native-backend* (make-backend)
  "The backend for the machine we are running on. Do not change this.")
(defvar *target-backend* *native-backend*
  "The backend we are attempting to compile.")
(defvar *backend* *native-backend*
  "The backend we are using to compile.")



;;;; Generate the stubs.

(macrolet
    ((frob ()
       `(progn
	  ,@(mapcar
	     #'(lambda (name)
		 `(defun ,name (&rest args)
		    (apply (or (,(symbolicate "BACKEND-" name) *backend*)
			       (error "Machine specific support routine ~S ~
					undefined for ~S"
				      ',name *backend*))
			   args)))
	     vm-support-routines))))
  (frob))
