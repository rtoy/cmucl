;; Bootstrap file for adding support for localization.

(setf lisp::*enable-package-locked-errors* nil)

(defvar lisp::*environment-list-initialized* nil)

(defpackage "INTL"
  (:use "COMMON-LISP")
  (:export "SETLOCALE" "TEXTDOMAIN" "GETTEXT" "DGETTEXT" "NGETTEXT" "DNGETTEXT"
           "*TRANSLATABLE-DUMP-STREAM*" "READ-TRANSLATABLE-STRING"
	   "*LOCALE-DIRECTORIES*"))

(with-open-file (s "target:code/intl.lisp")
  (compile-from-stream s))

(intl::install)


(in-package "C")
;; The textdomain for the documentation
(define-info-type function textdomain (or string null) nil)
(define-info-type variable textdomain (or string null) nil)
(define-info-type type textdomain (or string null) nil)
(define-info-type typed-structure textdomain (or string null) nil)
(define-info-type setf textdomain (or string null) nil)

;;;
;;; Like DEFSTRUCT, but silently clobber old definitions.
;;;
(defmacro defstruct! (name &rest stuff)
  `(handler-bind ((error (lambda (c)
                           (declare (ignore c))
                           (invoke-restart 'kernel::clobber-it))))
     (defstruct ,name ,@stuff)))


(defstruct! (template
	    (:print-function %print-template)
	    (:pure t))
  ;;
  ;; The symbol name of this VOP.  This is used when printing the VOP and is
  ;; also used to provide a handle for definition and translation.
  (name nil :type symbol)
  ;;
  ;; A Function-Type describing the arg/result type restrictions.  We compute
  ;; this from the Primitive-Type restrictions to make life easier for IR1
  ;; phases that need to anticipate LTN's template selection.
  (type (required-argument) :type function-type)
  ;;
  ;; Lists of restrictions on the argument and result types.  A restriction may
  ;; take several forms:
  ;; -- The restriction * is no restriction at all.
  ;; -- A restriction (:OR <primitive-type>*) means that the operand must have
  ;;    one of the specified primitive types.
  ;; -- A restriction (:CONSTANT <predicate> <type-spec>) means that the
  ;;    argument (not a result) must be a compile-time constant that satisfies
  ;;    the specified predicate function.  In this case, the constant value
  ;;    will be passed as an info argument rather than as a normal argument.
  ;;    <type-spec> is a Lisp type specifier for the type tested by the
  ;;    predicate, used when we want to represent the type constraint as a Lisp
  ;;    function type. 
  ;;
  ;; If Result-Types is :Conditional, then this is an IF-xxx style conditional
  ;; that yeilds its result as a control transfer.  The emit function takes two
  ;; info arguments: the target label and a boolean flag indicating whether to
  ;; negate the sense of the test.
  (arg-types nil :type list)
  (result-types nil :type (or list (member :conditional)))
  ;;
  ;; The primitive type restriction applied to each extra argument or result
  ;; following the fixed operands.  If NIL, no extra args/results are allowed.
  ;; Otherwise, either * or a (:OR ...) list as described for the
  ;; {ARG,RESULT}-TYPES.
  (more-args-type nil :type (or (member nil *) cons))
  (more-results-type nil :type (or (member nil *) cons))
  ;;
  ;; If true, this is a function that is called with no arguments to see if
  ;; this template can be emitted.  This is used to conditionally compile for
  ;; different target hardware configuarations (e.g. FP hardware.)
  (guard nil :type (or function null))
  ;;
  ;; The policy under which this template is the best translation.  Note that
  ;; LTN might use this template under other policies if it can't figure our
  ;; anything better to do.
  (policy (required-argument) :type policies)
  ;;
  ;; The base cost for this template, given optimistic assumptions such as no
  ;; operand loading, etc.
  (cost (required-argument) :type index)
  ;;
  ;; If true, then a short noun-like phrase describing what this VOP "does",
  ;; i.e. the implementation strategy.  This is for use in efficiency notes.
  (note nil :type (or string null))
  ;;
  ;; The number of trailing arguments to VOP or %Primitive that we bundle into
  ;; a list and pass into the emit function.  This provides a way to pass
  ;; uninterpreted stuff directly to the code generator.
  (info-arg-count 0 :type index)
  ;;
  ;; A function that emits the VOPs for this template.  Arguments:
  ;;  1] Node for source context.
  ;;  2] IR2-Block that we place the VOP in.
  ;;  3] This structure.
  ;;  4] Head of argument TN-Ref list.
  ;;  5] Head of result TN-Ref list.
  ;;  6] If Info-Arg-Count is non-zero, then a list of the magic arguments.
  ;;
  ;; Two values are returned: the first and last VOP emitted.  This vop
  ;; sequence must be linked into the VOP Next/Prev chain for the block.  At
  ;; least one VOP is always emitted.
  (emit-function (required-argument) :type function)
  ;;
  ;; The text domain for the note.
  (note-domain intl::*default-domain* :type (or string null)))