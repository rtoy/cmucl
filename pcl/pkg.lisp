;;;-*-Mode:LISP; Package:(PCL (LISP WALKER)); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;

(in-package ':walker :use '(:lisp))

(export '(define-walker-template
	  walk-form
	  nested-walk-form
	  variable-lexical-p
	  variable-special-p
	  variable-globally-special-p
	  *variable-declarations*
	  variable-declaration
	  ))

(in-package :iterate :use '(:lisp :walker))

(export '(iterate iterate* gathering gather with-gathering interval elements 
	  list-elements list-tails plist-elements eachtime while until 
	  collecting joining maximizing minimizing summing 
	  *iterate-warnings*))

(in-package :pcl :use '(:lisp :walker :iterate))

;;;
;;; Some CommonLisps have more symbols in the Lisp package than the ones that
;;; are explicitly specified in CLtL.  This causes trouble. Any Lisp that has
;;; extra symbols in the Lisp package should shadow those symbols in the PCL
;;; package.
;;;
#+TI
(shadow '(string-append once-only destructuring-bind
	  memq assq delq neq true false
	  without-interrupts
	  defmethod)
	*the-pcl-package*)

#+CMU
(shadow '(destructuring-bind)
        *the-pcl-package*)

#+GCLisp
(shadow '(string-append memq assq delq neq make-instance)
	*the-pcl-package*)

#+Genera
(shadowing-import '(zl:arglist zwei:indentation) *the-pcl-package*)

#+Lucid 
(import '(#-LCL3.0 system:arglist #+LCL3.0 lcl:arglist
	  system:structurep system:structure-type system:structure-length)
	*the-pcl-package*)
  
#+lucid
(#-LCL3.0 progn #+LCL3.0 lcl:handler-bind 
    #+LCL3.0 ((lcl:warning #'(lambda (condition)
			       (declare (ignore condition))
			       (lcl:muffle-warning))))
(let ((importer
        #+LCL3.0 #'sys:import-from-lucid-pkg
	#-LCL3.0 (let ((x (find-symbol "IMPORT-FROM-LUCID-PKG" "LUCID")))
		   (if (and x (fboundp x))
		       (symbol-function x)
		       ;; Only the #'(lambda (x) ...) below is really needed, 
		       ;;  but when available, the "internal" function 
		       ;;  'import-from-lucid-pkg' provides better checking.
		       #'(lambda (name)
			   (import (intern name "LUCID")))))))
  ;;
  ;; We need the following "internal", undocumented Lucid goodies:
  (mapc importer '("%POINTER" "DEFSTRUCT-SIMPLE-PREDICATE"
		   #-LCL3.0 "LOGAND&" "%LOGAND&" #+VAX "LOGAND&-VARIABLE"))

  ;;
  ;; For without-interrupts.
  ;; 
  #+LCL3.0
  (mapc importer '("*SCHEDULER-WAKEUP*" "MAYBE-CALL-SCHEDULER"))

  ;;
  ;; We import the following symbols, because in 2.1 Lisps they have to be
  ;;  accessed as SYS:<foo>, whereas in 3.0 lisps, they are homed in the
  ;;  LUCID-COMMON-LISP package.
  (mapc importer '("ARGLIST" "NAMED-LAMBDA" "*PRINT-STRUCTURE*"))
  ;;
  ;; We import the following symbols, because in 2.1 Lisps they have to be
  ;;  accessed as LUCID::<foo>, whereas in 3.0 lisps, they have to be
  ;;  accessed as SYS:<foo>
  (mapc importer '(
		   "NEW-STRUCTURE"   	"STRUCTURE-REF"
		   "STRUCTUREP"         "STRUCTURE-TYPE"  "STRUCTURE-LENGTH"
		   "PROCEDUREP"     	"PROCEDURE-SYMBOL"
		   "PROCEDURE-REF" 	"SET-PROCEDURE-REF" 
		   ))
; ;;
; ;;  The following is for the "patch" to the general defstruct printer.
; (mapc importer '(
; 	           "OUTPUT-STRUCTURE" 	  "DEFSTRUCT-INFO"
;		   "OUTPUT-TERSE-OBJECT"  "DEFAULT-STRUCTURE-PRINT" 
;		   "STRUCTURE-TYPE" 	  "*PRINT-OUTPUT*"
;		   ))
  ;;
  ;; The following is for a "patch" affecting compilation of %logand&.
  ;; On APOLLO, Domain/CommonLISP 2.10 does not include %logand& whereas
  ;; Domain/CommonLISP 2.20 does; Domain/CommonLISP 2.20 includes :DOMAIN/OS
  ;; on *FEATURES*, so this conditionalizes correctly for APOLLO.
  #-(or (and APOLLO DOMAIN/OS) LCL3.0 VAX) 
  (mapc importer '("COPY-STRUCTURE"  "GET-FDESC"  "SET-FDESC"))
  
  nil))

#+kcl
(progn
(import '(system:structurep))
(shadow 'lisp:dotimes)
)
#+kcl
(in-package "SI")
#+kcl
(export '(%structure-name
          %compiled-function-name
          %set-compiled-function-name))
#+kcl
(in-package 'pcl)

#+cmu (shadow 'lisp:dotimes)

#+cmu
(import '(kernel:funcallable-instance-p ext:structurep)
	*the-pcl-package*)


(shadow 'documentation)


;;;						
;;; These come from the index pages of 88-002R.
;;;
;;;
(eval-when (compile load eval)  
  
(defvar *exports* '(add-method
		    built-in-class
		    call-method
		    call-next-method
		    change-class
		    class-name
		    class-of
		    compute-applicable-methods
		    defclass
		    defgeneric
		    define-method-combination
		    defmethod
		    ensure-generic-function
		    find-class
		    find-method
		    function-keywords
		    generic-flet
		    generic-labels
		    initialize-instance
		    invalid-method-error
		    make-instance
		    make-instances-obsolete
		    method-combination-error
		    method-qualifiers
		    next-method-p
		    no-applicable-method
		    no-next-method
		    print-object
		    reinitialize-instance
		    remove-method
		    shared-initialize
		    slot-boundp
		    slot-exists-p
		    slot-makunbound
		    slot-missing
		    slot-unbound
		    slot-value
		    standard
		    standard-class
		    standard-generic-function
		    standard-method
		    standard-object
		    structure-class
		    #-cmu symbol-macrolet
		    update-instance-for-different-class
		    update-instance-for-redefined-class
		    with-accessors
		    with-added-methods
		    with-slots
		    ))

);eval-when 

#-(or KCL IBCL CMU)
(export *exports* *the-pcl-package*)

#+CMU
(export '#.*exports* *the-pcl-package*)

#+(or KCL IBCL)
(mapc 'export (list *exports*) (list *the-pcl-package*))



;(defvar *chapter-3-exports* '(
;			  get-setf-function
;			  get-setf-function-name
;
;			  class-prototype
;			  class
;			  object
;
;;			  essential-class
;			   
;			  class-name
;			  class-precedence-list
;			  class-local-supers
;			  class-local-slots
;			  class-direct-subclasses
;			  class-direct-methods
;			  class-slots
;
;			   
;			  method-arglist
;			  method-argument-specifiers			
;			  method-function
;			   
;			  method-equal
;			   
;			  slotd-name
;			  slot-missing
;			   
;;			  define-meta-class
;;			  %allocate-instance
;;			  %instance-ref
;;			  %instancep
;;			  %instance-meta-class
;
;			  allocate-instance
;			  optimize-slot-value
;			  optimize-setf-of-slot-value
;			  add-named-class
;			  class-for-redefinition
;			  add-class
;			  supers-changed
;			  slots-changed
;			  validate-superclass
;			  make-slotd
;			  compute-class-precedence-list
;			  walk-method-body
;			  walk-method-body-form
;			  add-named-method
;			  remove-named-method
;
;
;			  ))

(defvar *slot-accessor-name-package*
  (or (find-package :slot-accessor-name)
      (make-package :slot-accessor-name 
		    :use '()
		    :nicknames '(:s-a-n))))
