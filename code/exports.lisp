;;; -*- Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/exports.lisp,v 1.32 1990/06/09 17:30:12 wlott Exp $
;;;
;;; All the stuff necessary to export various symbols from various packages.
;;;
;;; Written by William Lott.
;;; 


;;; Old compiler cleanup.

#-new-compiler
(progn

(labels
    ((nuke (name package)
	   (let* ((package (find-package package))
		  (symbol (find-symbol name package)))
	     (when symbol
	       (unintern symbol package)))))
  (nuke "REAL" "XLIB")
  (nuke "FORM" "LISP")
  (nuke "INDEX" "LISP")
  (nuke "LEXICAL-ENVIRONMENT" "LISP")
  (nuke "NEGATE" "LISP")
  (nuke "TYPE-EXPAND" "LISP")
  (nuke "VAR" "LISP")
  (nuke "ARG" "LISP")
  (nuke "STRUCTURE-TYPE" "XP")
  (nuke "CONCAT-PNAMES" "LISP")
  (nuke "ONCE-ONLY" "COMPILER")
  (nuke "CONSTANT" "COMPILER")
  (nuke "SAP+" "COMPILER")
  (nuke "POINTER<" "COMPILER")
  (nuke "POINTER>" "COMPILER")
  (nuke "SAP+" "LISP"))

(labels
    ((lisp->system (name)
		   (let ((symbol (find-symbol name (find-package "LISP"))))
		     (when symbol
		       (import symbol (find-package "SYSTEM"))))))
  (lisp->system "%SET-ALIEN-ACCESS")
  (lisp->system "CHECK<=")
  (lisp->system "CT-A-VAL")
  (lisp->system "CT-A-VAL-ALIEN")
  (lisp->system "CT-A-VAL-OFFSET")
  (lisp->system "CT-A-VAL-P")
  (lisp->system "CT-A-VAL-SAP")
  (lisp->system "CT-A-VAL-SIZE")
  (lisp->system "CT-A-VAL-TYPE")
  (lisp->system "DEPORT-BOOLEAN")
  (lisp->system "DEPORT-INTEGER")
  (lisp->system "MAKE-CT-A-VAL")
  (lisp->system "NATURALIZE-BOOLEAN")
  (lisp->system "NATURALIZE-INTEGER")
  (lisp->system "SAP-REF-SAP"))

(let ((symbol (find-symbol "CHECK=" (find-package "COMPILER"))))
  (when symbol (import symbol (find-package "SYSTEM"))))

); #-new-compiler progn



;;; Create the packages

(in-package "LISP")
(in-package "KERNEL")
(in-package "SYSTEM" :nicknames '("SYS"))
(in-package "EXTENSIONS" :nicknames '("EXT"))
(in-package "USER")
(in-package "VM")
(in-package "C")
(in-package "ASSEMBLER" :nicknames '("ASSEM"))
(in-package "BIGNUM")
(in-package "DEBUG")
(in-package "DEBUG-INTERNALS" :nicknames '("DI"))


(in-package "LISP")

(use-package "KERNEL")
(use-package "EXT")
(use-package "SYSTEM")
(use-package "BIGNUM")

(export '(&allow-other-keys &aux &body &environment &key &optional &rest
	  &whole * ** *** *applyhook* *break-on-signals*
	  *break-on-warnings* *debug-io* *debugger-hook*
	  *default-pathname-defaults* *error-output* *evalhook* *features*
	  *gensym-counter* *load-verbose* *macroexpand-hook* *modules*
	  *package* *print-array* *print-base* *print-case* *print-circle*
	  *print-escape* *print-gensym* *print-length* *print-level*
	  *print-pretty* *print-radix* *query-io* *random-state*
	  *read-base* *read-default-float-format* *read-suppress*
	  *readtable* *standard-input* *standard-output* *terminal-io*
	  *trace-output* + ++ +++ - / // /// /= 1+ 1- < <= = > >= abort abs
	  acons acos acosh adjoin adjust-array adjustable-array-p
	  alpha-char-p alphanumericp and append apply applyhook apropos
	  apropos-list aref arithmetic-error arithmetic-error-operands
	  arithmetic-error-operation array array-dimension
	  array-dimension-limit array-dimensions array-element-type
	  array-has-fill-pointer-p array-in-bounds-p array-rank
	  array-rank-limit array-row-major-index array-total-size
	  array-total-size-limit arrayp ash asin asinh assert assoc
	  assoc-if assoc-if-not atan atanh atom base-character base-string
	  bignum bit bit-and bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand
	  bit-nor bit-not bit-orc1 bit-orc2 bit-vector bit-vector-p bit-xor
	  block boole boole-1 boole-2 boole-and boole-andc1 boole-andc2
	  boole-c1 boole-c2 boole-clr boole-eqv boole-ior boole-nand
	  boole-nor boole-orc1 boole-orc2 boole-set boole-xor both-case-p
	  boundp break butlast byte byte-position byte-size caaaar caaadr
	  caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr
	  caddr cadr call-arguments-limit car case catch ccase cdaaar
	  cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar
	  cddddr cdddr cddr cdr ceiling cell-error cerror char char-bit
	  char-bits char-bits-limit char-code char-code-limit
	  char-control-bit char-downcase char-equal char-font
	  char-font-limit char-greaterp char-hyper-bit char-int char-lessp
	  char-meta-bit char-name char-not-equal char-not-greaterp
	  char-not-lessp char-super-bit char-upcase char/= char< char<=
	  char= char> char>= character characterp check-type cis
	  clear-input clear-output close clrhash code-char coerce common
	  commonp compilation-speed compile compile-file compiled-function
	  compiled-function-p compiler-let complex complexp
	  compute-restarts concatenate cond condition conjugate cons consp
	  constantp continue control-error copy-alist copy-list
	  copy-readtable copy-seq copy-symbol copy-tree cos cosh count
	  count-if count-if-not ctypecase debug-info decf declaration
	  declare decode-float decode-universal-time defconstant
	  define-condition define-modify-macro define-setf-method defmacro
	  defparameter defsetf defstruct deftype defun defvar delete
	  delete-duplicates delete-file delete-if delete-if-not denominator
	  deposit-field describe digit-char digit-char-p directory
	  directory-namestring disassemble division-by-zero do do*
	  do-all-symbols do-external-symbols do-symbols documentation
	  dolist dotimes double-float double-float-epsilon
	  double-float-negative-epsilon dpb dribble ecase ed eighth elt
	  encode-universal-time end-of-file endp enough-namestring eq eql
	  equal equalp error etypecase eval eval-when evalhook evenp every
	  exp export expt extended-character fboundp fceiling fdefinition
	  ffloor fifth file-author file-error file-error-pathname
	  file-length file-namestring file-position file-write-date fill
	  fill-pointer find find-all-symbols find-if find-if-not
	  find-package find-restart find-symbol finish-output first fixnum
	  flet float float-digits float-precision float-radix float-sign
	  floating-point-overflow floating-point-underflow floatp floor
	  fmakunbound force-output format fourth fresh-line fround
	  ftruncate ftype funcall function function-lambda-expression
	  functionp gcd gensym gentemp get get-decoded-time
	  get-dispatch-macro-character get-internal-real-time
	  get-internal-run-time get-macro-character
	  get-output-stream-string get-properties get-setf-method
	  get-setf-method-multiple-value get-universal-time getf gethash go
	  graphic-char-p handler-bind handler-case hash-table
	  hash-table-count hash-table-p host-namestring identity if ignore
	  ignore-errors imagpart import in-package incf inline
	  input-stream-p inspect int-char integer integer-decode-float
	  integer-length integerp intern internal-time-units-per-second
	  intersection invoke-debugger invoke-restart
	  invoke-restart-interactively isqrt keyword keywordp labels lambda
	  lambda-list-keywords lambda-parameters-limit last lcm ldb
	  ldb-test ldiff least-negative-double-float
	  least-negative-long-float least-negative-short-float
	  least-negative-single-float least-positive-double-float
	  least-positive-long-float least-positive-short-float
	  least-positive-single-float length let let*
	  lisp-implementation-type lisp-implementation-version list list*
	  list-all-packages list-length listen listp load locally log
	  logand logandc1 logandc2 logbitp logcount logeqv logior lognand
	  lognor lognot logorc1 logorc2 logtest logxor long-float
	  long-float-epsilon long-float-negative-epsilon long-site-name
	  loop lower-case-p machine-instance machine-type machine-version
	  macro-function macroexpand macroexpand-1 macrolet make-array
	  make-broadcast-stream make-char make-concatenated-stream
	  make-condition make-dispatch-macro-character make-echo-stream
	  make-hash-table make-list make-package make-pathname
	  make-random-state make-sequence make-string
	  make-string-input-stream make-string-output-stream make-symbol
	  make-synonym-stream make-two-way-stream makunbound map mapc
	  mapcan mapcar mapcon maphash mapl maplist mask-field max member
	  member-if member-if-not merge merge-pathnames min minusp mismatch
	  mod most-negative-double-float most-negative-fixnum
	  most-negative-long-float most-negative-short-float
	  most-negative-single-float most-positive-double-float
	  most-positive-fixnum most-positive-long-float
	  most-positive-short-float most-positive-single-float
	  muffle-warning multiple-value-bind multiple-value-call
	  multiple-value-list multiple-value-prog1 multiple-value-setq
	  multiple-values-limit name-char namestring nbutlast nconc nil
	  nintersection ninth not notany notevery notinline nreconc
	  nreverse nset-difference nset-exclusive-or nstring-capitalize
	  nstring-downcase nstring-upcase nsublis nsubst nsubst-if
	  nsubst-if-not nsubstitute nsubstitute-if nsubstitute-if-not nth
	  nthcdr null number numberp numerator nunion oddp open optimize or
	  otherwise output-stream-p package package-error
	  package-error-package package-name package-nicknames
	  package-shadowing-symbols package-use-list package-used-by-list
	  packagep pairlis parse-integer parse-namestring pathname
	  pathname-device pathname-directory pathname-host pathname-name
	  pathname-type pathname-version pathnamep peek-char phase pi plusp
	  pop position position-if position-if-not pprint prin1
	  prin1-to-string princ princ-to-string print probe-file proclaim
	  prog prog* prog1 prog2 progn program-error progv provide psetf
	  psetq push pushnew quote random random-state random-state-p
	  rassoc rassoc-if rassoc-if-not ratio rational rationalize
	  rationalp read read-byte read-char read-char-no-hang
	  read-delimited-list read-from-string read-line
	  read-preserving-whitespace readtable readtablep real realpart
	  reduce rem remf remhash remove remove-duplicates remove-if
	  remove-if-not remprop rename-file rename-package replace require
	  rest restart restart-bind restart-case restart-name return
	  return-from revappend reverse room rotatef round row-major-aref
	  rplaca rplacd safety satisfies sbit scale-float schar search
	  second sequence serious-condition set set-char-bit set-difference
	  set-dispatch-macro-character set-exclusive-or set-macro-character
	  set-syntax-from-char setf setq seventh shadow shadowing-import
	  shiftf short-float short-float-epsilon
	  short-float-negative-epsilon short-site-name signal signed-byte
	  signum simple-array simple-base-string simple-bit-vector
	  simple-bit-vector-p simple-condition
	  simple-condition-format-arguments simple-condition-format-string
	  simple-error simple-string simple-string-p simple-type-error
	  simple-vector simple-vector-p simple-warning sin single-float
	  single-float-epsilon single-float-negative-epsilon sinh sixth
	  sleep software-type software-version some sort space special
	  special-form-p speed sqrt stable-sort stack-overflow
	  standard-char standard-char-p step storage-condition
	  storage-exhausted store-value stream stream-element-type
	  stream-error stream-error-stream streamp string string-capitalize
	  string-char string-char-p string-downcase string-equal
	  string-greaterp string-left-trim string-lessp string-not-equal
	  string-not-greaterp string-not-lessp string-right-trim
	  string-trim string-upcase string/= string< string<= string=
	  string> string>= stringp structure sublis subseq subsetp subst
	  subst-if subst-if-not substitute substitute-if substitute-if-not
	  subtypep svref sxhash symbol symbol-function symbol-name
	  symbol-package symbol-plist symbol-value symbolp t tagbody tailp
	  tan tanh tenth terpri the third throw time trace tree-equal
	  truename truncate type type-error type-error-datum
	  type-error-expected-type type-of typecase typep unbound-variable
	  undefined-function unexport unintern union unless unread-char
	  unsigned-byte untrace unuse-package unwind-protect upper-case-p
	  use-package use-value user-homedir-pathname values values-list
	  variable vector vector-pop vector-push vector-push-extend vectorp
	  warn warning when with-compilation-unit with-input-from-string
	  with-open-file with-open-stream with-output-to-string
	  with-simple-restart write write-byte write-char write-line
	  write-string write-to-string y-or-n-p yes-or-no-p zerop))


(in-package "KERNEL")

(use-package "EXT")
(use-package "SYSTEM")
(use-package "BIGNUM")

(export '(%array-fill-pointer %array-available-elements %array-data-vector
	  %array-displacement %array-displaced-p %array-dimension
	  %check-bound %dpb %ldb %negate *empty-type* *eval-stack-top*
	  *null-type* *universal-type* *wild-type* 32bit-logical-not
	  32bit-logical-nor 32bit-logical-and 32bit-logical-or
	  32bit-logical-xor always-subtypep args-type args-type-allowp
	  args-type-keyp args-type-keywords args-type-optional args-type-p
	  args-type-required args-type-rest array-rank array-total-size
	  array-type array-type-complexp array-type-dimensions
	  array-type-element-type array-type-p
	  array-type-specialized-element-type ash-index bit-bash-clear
	  bit-bash-set bit-bash-not bit-bash-copy bit-bash-and bit-bash-ior
	  bit-bash-xor bit-bash-eqv bit-bash-lognand bit-bash-lognor
	  bit-bash-andc1 bit-bash-andc2 bit-bash-orc1 bit-bash-orc2
	  bit-index boole-code boolean byte-specifier callable char-int
	  consed-sequence constant-type constant-type-p constant-type-type
	  containing-integer-type copy-from-system-area copy-to-system-area
	  csubtypep ctype ctype-of ctype-p ctypep data-vector-ref
	  data-vector-set filename float-digits float-exponent
	  float-format-max float-radix form function-type
	  function-type-allowp function-type-keyp function-type-keywords
	  function-type-optional function-type-p function-type-required
	  function-type-rest function-type-returns function-type-wild-args
	  hairy-type hairy-type-check-template hairy-type-specifier index
	  internal-time irrational key-info key-info-name key-info-p
	  key-info-type lexical-environment make-args-type
	  make-function-type make-key-info make-member-type make-named-type
	  make-numeric-type make-structure-type make-union-type
	  make-values-type member-type member-type-members member-type-p
	  merge-bits named-type named-type-name named-type-p
	  native-byte-order negate never-subtypep numeric-contagion
	  numeric-type numeric-type-class numeric-type-complexp
	  numeric-type-format numeric-type-high numeric-type-low
	  numeric-type-p parse-unknown-type parse-unknown-type-specifier
	  pathname-device pathname-directory pathname-host pathname-name
	  pathname-type pathname-version pathnamelike sequence-end
	  simple-unboxed-array single-value-type specifier-type streamlike
	  stringable stringlike structure-type structure-type-name
	  structure-type-p system-area-clear system-area-copy truth
	  type-expand type-init two-arg-* two-arg-+ two-arg-- two-arg-/
	  two-arg-/= two-arg-< two-arg-<= two-arg-= two-arg-> two-arg->=
	  two-arg-and two-arg-gcd two-arg-ior two-arg-lcm two-arg-xor
	  type-difference type-intersect type-intersection type-specifier
	  type-specifier-symbols type-union type/= type= types-intersect
	  unboxed-array union-type union-type-p union-type-types
	  unknown-type unknown-type-p unknown-type-specifier
	  values-subtypep values-type values-type-allowp
	  values-type-intersect values-type-intersection values-type-keyp
	  values-type-keywords values-type-optional values-type-p
	  values-type-required values-type-rest values-type-union
	  values-types values-types-intersect void))


(in-package "EXTENSIONS")

(export '(*after-gc-hooks* *after-save-initializations* *backup-extension*
	  *before-gc-hooks* *before-save-initializations*
 	  *bytes-consed-between-gcs* *clx-fds-to-displays*
 	  *command-line-strings* *command-line-switches*
 	  *command-line-utility-name* *command-line-words*
 	  *command-switch-demons* *compatibility-warnings*
 	  *describe-implementation-details* *describe-indentation*
 	  *describe-level* *describe-print-length* *describe-print-level*
 	  *describe-verbose* *display-event-handlers* *editor-lisp-p*
 	  *environment-list* *gc-inhibit-hook* *gc-notify-after*
 	  *gc-notify-before* *gc-verbose* *hemlock-version*
 	  *ignore-floating-point-underflow* *info-environment*
 	  *intexp-maximum-exponenent* *keyword-package* *lisp-package*
 	  *load-if-source-newer* *max-step-indentation*
 	  *max-trace-indentation* *module-file-translations* *prompt*
 	  *require-verbose* *safe-defstruct-accessors* *step-print-length*
 	  *step-print-level* *terminal-line-mode* *trace-print-length*
 	  *trace-print-level* *traced-function-list* abort
 	  accept-tcp-connection add-oob-handler ambiguous-files
 	  argument-list assq basic-definition bignump bitp c-sizeof
 	  call-user-miscop careful-symbol-function carefully-add-font-paths
 	  char clean-up-compiler clear-info close-socket cmd-switch-arg
 	  cmd-switch-name cmd-switch-value cmd-switch-words collect
 	  command-line-switch command-line-switch-p
 	  compact-info-environment compile-from-stream compiledp
 	  complete-file concat-pnames connect-to-inet-socket constant
 	  constant-argument create-inet-listener create-inet-socket debug
 	  def-c-array def-c-pointer def-c-procedure def-c-record
 	  def-c-routine def-c-type def-c-variable default-clx-event-handler
 	  default-directory define-info-class define-info-type
 	  define-keyboard-modifier define-keysym define-mouse-code
 	  defmodule defswitch deletef delq disable-clx-event-handling
 	  do-anonymous do-info double-floatp dovector e
 	  enable-clx-event-handling encapsulate encapsulated-p
 	  file-writable fixnump flush-display-events format-decoded-time
 	  format-universal-time gc gc-off gc-on get-bytes-consed
 	  get-code-pointer get-command-line-switch get-data-pointer grindef
 	  host-entry host-entry-addr host-entry-addr-list
 	  host-entry-aliases host-entry-name htonl htons ignorable
 	  ignore-errors inaddr-any indenting-further info int
 	  interactive-eval ipproto-tcp ipproto-udp iterate letf letf*
 	  listen-skip-whitespace load-foreign long long-floatp
 	  lookup-host-entry make-info-environment maybe-inline memq ntohl
 	  ntohs object-set-event-handler once-only open-clx-display
 	  parse-time print-directory print-herald process-alive-p
 	  process-close process-core-dumped process-error process-exit-code
 	  process-input process-kill process-output process-p process-pid
 	  process-plist process-pty process-status process-status-hook
 	  process-wait putf quit ratiop read-char-no-edit realp
 	  remove-all-oob-handlers remove-oob-handler reset-foreign-pointers
 	  run-program save save-all-buffers save-lisp search-list
 	  send-character-out-of-band serve-button-press
 	  serve-button-release serve-circulate-notify
 	  serve-circulate-request serve-client-message
 	  serve-colormap-notify serve-configure-notify
 	  serve-configure-request serve-create-notify serve-destroy-notify
 	  serve-enter-notify serve-exposure serve-focus-in serve-focus-out
 	  serve-graphics-exposure serve-gravity-notify serve-key-press
 	  serve-key-release serve-leave-notify serve-map-notify
 	  serve-map-request serve-motion-notify serve-no-exposure
 	  serve-property-notify serve-reparent-notify serve-resize-request
 	  serve-selection-clear serve-selection-notify
 	  serve-selection-request serve-unmap-notify
 	  serve-visibility-notify set-symbol-function-carefully short
 	  short-floatp signal single-floatp structurep translate-character
 	  translate-mouse-character truly-the uncompile undefined-value
 	  unencapsulate unsigned-char unsigned-int unsigned-long
 	  unsigned-short void with-clx-event-handling
	  weak-pointer weak-pointer-p make-weak-pointer weak-pointer-value))


(in-package "SYSTEM")

(export '(%alien-indirect %assembler-code-type %bind-aligned-sap
	  %set-alien-access %standard-char-p %static-alien-area
	  %string-char-p *alien-eval-when* *beep-function* *gr-messages*
	  *in-the-compiler* *maximum-interpreter-error-checking*
	  *nameserverport* *pornography-of-death*
	  *port-ownership-rights-handlers* *port-receive-rights-handlers*
	  *stderr* *stdin* *stdout* *task-data* *task-notify* *task-self*
	  *tty* *typescriptport* *usertypescript* *userwindow*
	  *xwindow-table* add-fd-handler add-port-death-handler
	  add-port-object add-xwindow-object alien alien-access
	  alien-address alien-assign alien-bind alien-index alien-indirect
	  alien-sap alien-size alien-type alien-value
	  allocate-system-memory beep bits boolean bytes c-procedure
	  check<= check= compiler-version copy-alien ct-a-val
	  ct-a-val-alien ct-a-val-offset ct-a-val-p ct-a-val-sap
	  ct-a-val-size ct-a-val-type deallocate-system-memory defalien
	  default-interrupt defenumeration define-alien-stack defoperator
	  defrecord deport-boolean deport-integer dispose-alien
	  double-float-radix enable-interrupt enumeration fd-stream
	  fd-stream-fd fd-stream-p fexpr find-if-in-closure gr-bind gr-call
	  gr-call* gr-error ignore-interrupt int-sap invalidate-descriptor
	  long-float-radix long-words macro make-alien make-ct-a-val
	  make-fd-stream make-indenting-stream make-object-set map-port
	  map-xwindow naturalize-boolean naturalize-integer
	  null-terminated-string object-set-operation output-raw-bytes
	  parse-body perq-string pointer pointer< pointer> port primep
	  read-n-bytes record-size remove-fd-handler
	  remove-port-death-handler remove-port-object
	  remove-xwindow-object resolve-loaded-assembler-references sap+
	  sap- sap-int sap-ref-16 sap-ref-32 sap-ref-8 sap-ref-sap
	  serve-all-events serve-event server server-message
	  short-float-radix signed-sap-ref-16 signed-sap-ref-32
	  signed-sap-ref-8 single-float-radix symbol-macro-let
	  system-area-pointer system-area-pointer-p unproclaim unstructured
	  wait-until-fd-usable with-enabled-interrupts with-fd-handler
	  with-interrupts with-reply-port with-stack-alien without-gcing
	  without-hemlock without-interrupts words))


(in-package "USER")

(use-package "EXT")



(in-package "VM")

(use-package "KERNEL")
(use-package "EXT")

(export '(*assembly-unit-length* *primitive-objects* array-data-slot
	  array-dimensions-offset array-displaced-p-slot
	  array-displacement-slot array-elements-slot
	  array-fill-pointer-slot atomic-flag base-character-type
	  bignum-digits-offset bignum-type binding-size binding-symbol-slot
	  binding-value-slot byte-bits catch-block-current-code-slot
	  catch-block-current-cont-slot catch-block-current-uwp-slot
	  catch-block-entry-pc-slot catch-block-previous-catch-slot
	  catch-block-size catch-block-size-slot catch-block-tag-slot
	  cerror-trap closure-function-header-type closure-function-slot
	  closure-header-type closure-info-offset code-code-size-slot
	  code-constants-offset code-debug-info-slot code-entry-points-slot
	  code-header-type complex-array-type complex-bit-vector-type
	  complex-imag-slot complex-real-slot complex-size
	  complex-string-type complex-type complex-vector-type
	  cons-car-slot cons-cdr-slot cons-size 
	  define-for-each-primitive-object double-float-size
	  double-float-type double-float-value-slot error-trap
	  even-fixnum-type exported-static-symbols fixnum
	  function-header-arglist-slot function-header-code-offset
	  function-header-name-slot function-header-next-slot
	  function-header-self-slot function-header-type
	  function-header-type-slot function-pointer-type genesis halt-trap
	  interrupted-flag list-pointer-type lowtag-bits lowtag-limit
	  lowtag-mask most-positive-cost odd-fixnum-type
	  offset-static-symbol other-immediate-0-type
	  other-immediate-1-type other-pointer-type pad-data-block
	  pending-interrupt-trap primitive-object-header
	  primitive-object-lowtag primitive-object-name
	  primitive-object-options primitive-object-size
	  primitive-object-slots primitive-object-variable-length
	  ratio-denominator-slot ratio-numerator-slot ratio-size ratio-type
	  return-pc-header-type return-pc-return-point-offset
	  sap-pointer-slot sap-size sap-type sc-number-limit
	  simple-array-double-float-type simple-array-single-float-type
	  simple-array-type simple-array-unsigned-byte-16-type
	  simple-array-unsigned-byte-2-type
	  simple-array-unsigned-byte-32-type
	  simple-array-unsigned-byte-4-type
	  simple-array-unsigned-byte-8-type simple-bit-vector-type
	  simple-string-type simple-vector-type single-float-size
	  single-float-type single-float-value-slot slot-docs slot-length
	  slot-name slot-offset slot-options slot-rest-p
	  static-symbol-offset static-symbol-p static-symbols
	  structure-pointer-type symbol-function-slot symbol-header-type
	  symbol-name-slot symbol-package-slot symbol-plist-slot
	  symbol-size symbol-value-slot target-binding-stack-start
	  target-byte-order target-control-stack-start
	  target-dynamic-space-start target-fasl-code-format
	  target-fasl-file-type target-heap-address-space
	  target-most-negative-fixnum target-most-positive-fixnum
	  target-read-only-space-start target-static-space-start type-bits
	  type-mask unbound-marker-type unwind-block-current-code-slot
	  unwind-block-current-cont-slot unwind-block-current-uwp-slot
	  unwind-block-entry-pc-slot unwind-block-size
	  value-cell-header-type value-cell-size value-cell-value-slot
	  vector-data-offset vector-length-slot vector-normal-subtype
	  vector-structure-subtype vector-valid-hashing-subtype
	  vector-must-rehash-subtype vm-version word-bits word-bytes
	  word-shift weak-pointer-type weak-pointer-size
	  weak-pointer-value-slot weak-pointer-next-slot))


(in-package "C")

(use-package "EXT")
(use-package "KERNEL")
(use-package "SYSTEM")
(use-package "VM")
(use-package "ASSEM")
(use-package "BIGNUM")

(export '(*compile-time-define-macros* *compiling-for-interpreter*
	  compile-for-eval entry-node-info-nlx-tag entry-node-info-st-top
	  lambda-eval-info-args-passed lambda-eval-info-entries
	  lambda-eval-info-frame-size))

(in-package "LISP")
(import '(
	  %aset
	  %bitset
	  %charset
	  %primitive
	  %put
	  %raw-bits
	  %rplaca
	  %rplacd
	  %sbitset
	  %scharset
	  %set-documentation
	  %set-fdefinition
	  %set-fill-pointer
	  %set-row-major-aref
	  %set-sap-ref-sap
	  %set-sap-ref-32
	  %set-sap-ref-16
	  %set-sap-ref-8
	  %setelt
	  %setnth
	  %sp-set-definition
	  %sp-set-plist
	  %standard-char-p
	  %string-char-p
	  %svset
	  %typep
	  %array-typep
	  array-header-p
	  base-char-p
	  double-float-p
	  long-float-p
	  short-float-p
	  simple-array-p
	  single-float-p
	  string<*
	  string>*
	  string<=*
	  string>=*
	  string=*
	  string/=*
	  %sp-string-compare
	  )
	"C")


(in-package "ASSEM")

(export '(*current-position* align assemble define-argument-type
	  define-fixup-type define-format define-instruction
	  define-pseudo-instruction define-random-resources
	  define-register-file dump-segment emit-code-vector emit-label
	  finalize-segment fixup fixup-flavor fixup-name fixup-offset
	  fixup-p gen-label insert-segment inst label label-id label-position
	  make-fixup make-segment nuke-segment))


(in-package "EVAL")

(use-package "KERNEL")

(export '(internal-eval interpreted-function-arglist
	  interpreted-function-closure
	  interpreted-function-lambda-expression interpreted-function-name
	  interpreted-function-p make-interpreted-function))


(in-package "BIGNUM")

(use-package "KERNEL")

(import 'vm:bignum-type)

(export '(add-bignums bignum-ashift-left bignum-ashift-right bignum-compare
	  bignum-deposit-byte bignum-element-type bignum-gcd bignum-index
	  bignum-integer-length bignum-load-byte bignum-logcount
	  bignum-logical-and bignum-logical-ior bignum-logical-not
	  bignum-logical-xor bignum-plus-p bignum-to-double-float
	  bignum-to-single-float bignum-truncate bignum-type make-small-bignum
	  multiply-bignums negate-bignum subtract-bignum))



(in-package "DEBUG")

(export '(internal-debug *in-the-debugger* backtrace *flush-debug-errors*
	  *debug-print-level* *debug-print-length* *debug-prompt*

	  var arg))


(in-package "DEBUG-INTERNALS")

(use-package "SYSTEM")
(use-package "EXT")

;;; The compiler's debug-source structure is almost exactly what we want, so
;;; just get these symbols and export them.
;;;
(import '(c::debug-source-from c::debug-source-name c::debug-source-created
	  c::debug-source-compiled c::debug-source-start-positions
	  c::debug-source c::debug-source-p))

(export '(debug-variable-name debug-variable-package debug-variable-symbol
	  debug-variable-id debug-variable-value debug-variable-validity
	  debug-variable-valid-value debug-variable debug-variable-p

	  top-frame frame-down frame-up frame-debug-function
	  frame-code-location eval-in-frame return-from-frame frame-catches
	  frame-number frame frame-p

	  do-blocks debug-function-lambda-list debug-variable-info-available
	  do-debug-function-variables debug-function-symbol-variables
	  ambiguous-debug-variables preprocess-for-eval function-debug-function
	  debug-function-function debug-function-kind debug-function-name
	  debug-function debug-function-p

	  do-debug-block-locations debug-block-successors debug-block
	  debug-block-p debug-block-elsewhere-p

	  make-breakpoint activate-breakpoint deactivate-breakpoint
	  breakpoint-hook-function breakpoint-info breakpoint-kind
	  breakpoint-what breakpoint breakpoint-p

	  code-location-debug-function code-location-debug-block
	  code-location-top-level-form-offset code-location-form-number
	  code-location-debug-source code-location code-location-p
	  unknown-code-location unknown-code-location-p

	  debug-source-from debug-source-name debug-source-created
	  debug-source-compiled debug-source-root-number
	  debug-source-start-positions form-number-translations
	  source-path-context debug-source debug-source-p

	  debug-condition no-debug-info no-debug-function-returns
	  no-debug-blocks lambda-list-unavailable

	  debug-error unhandled-condition invalid-control-stack-pointer
	  unknown-code-location unknown-debug-variable invalid-value))
