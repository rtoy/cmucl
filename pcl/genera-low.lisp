;;; -*- Mode:LISP; Package:(PCL Lisp 1000); Base:10.; Syntax:Common-lisp; Patch-File: Yes -*-
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
;;; This is the 3600 version of the file portable-low.
;;;

(in-package 'pcl)

#+IMach						;On the I-Machine these are
(eval-when (compile load eval)			;faster than the versions
						;that use :test #'eq.  
(defmacro memq (item list) `(member ,item ,list))
(defmacro assq (item list) `(assoc ,item ,list))
(defmacro rassq (item list) `(rassoc ,item ,list))
(defmacro delq (item list) `(delete ,item ,list))
(defmacro posq (item list) `(position ,item ,list))

)

compiler::
(defoptimizer (cl:the the-just-gets-in-the-way-of-optimizers) (form)
  (matchp form
    (('cl:the type subform)
     (ignore type)
     subform)
    (* form)))

(defmacro %ash (x count)
  (if (and (constantp count) (zerop (eval count)))
      x
      `(the fixnum (ash (the fixnum ,x ) ,count))))

;;;
;;;
;;;

(defmacro without-interrupts (&body body)
  `(let ((outer-scheduling-state si:inhibit-scheduling-flag)
	 (si:inhibit-scheduling-flag t))
     (macrolet ((interrupts-on  ()
		  '(when (null outer-scheduling-state)
		     (setq si:inhibit-scheduling-flag nil)))
		(interrupts-off ()
		  '(setq si:inhibit-scheduling-flag t)))
       (progn outer-scheduling-state)
       ,.body)))

;;;
;;; It would appear that #, does not work properly in Genera.  At least I can't get it
;;; to work when I use it inside of std-instance-p (defined later in this file).  So,
;;; all of this is just to support that.
;;;
;;;     WHEN                       EXPANDS-TO
;;;   compile to a file          (#:EVAL-AT-LOAD-TIME-MARKER . <form>)
;;;   compile to core            '<result of evaluating form>
;;;   not in compiler at all     (progn <form>)
;;;
;;; Believe me when I tell you that I don't know why it is I need both a
;;; transformer and an optimizer to get this to work.  Believe me when I
;;; tell you that I don't really care why either.
;;;
(defmacro load-time-eval (form)
  ;; The interpreted definition of load-time-eval.  This definition
  ;; never gets compiled.
  (let ((value (gensym)))
    `(multiple-value-bind (,value)
	 (progn ,form)
       ,value)))

(compiler:deftransformer (load-time-eval optimize-load-time-eval) (form)
  (compiler-is-a-loser-internal form))

(compiler:defoptimizer (load-time-eval transform-load-time-eval) (form)
  (compiler-is-a-loser-internal form))

(defun compiler-is-a-loser-internal (form)  
  ;; When compiling a call to load-time-eval the compiler will call
  ;; this optimizer before the macro expansion.
  (if zl:compiler:(and (boundp '*compile-function*)	;Probably don't need
						        ;this boundp check
						        ;but it can't hurt.
		       (funcall *compile-function* :to-core-p))
      ;; Compiling to core.
      ;; Evaluate the form now, and expand into a constant
      ;; (the result of evaluating the form).
      `',(eval (cadr form))
      ;; Compiling to a file.
      ;; Generate the magic which causes the dumper compiler and loader
      ;; to do magic and evaluate the form at load time.
      `',(cons compiler:eval-at-load-time-marker (cadr form))))

;;   
;;;;;; Memory Block primitives.                ***
  ;;   


(defmacro make-memory-block (size &optional area)
  `(make-array ,size :area ,area))

(defmacro memory-block-ref (block offset)	;Don't want to go faster yet.
  `(aref ,block ,offset))

(defvar class-wrapper-area)
(eval-when (load eval)
  (si:make-area :name 'class-wrapper-area
		:room t
		:gc :static))

(eval-when (compile load eval)
  (remprop '%%allocate-instance--class 'inline))

(eval-when (compile load eval)
  
(scl:defflavor std-instance
	((wrapper nil)
	 (slots   nil))
	()
  (:constructor %%allocate-instance--class())
  :ordered-instance-variables)

(defvar *std-instance-flavor* (flavor:find-flavor 'std-instance))

)

#-imach
(scl:defsubst pcl-%instance-flavor (instance)
  (declare (compiler:do-not-record-macroexpansions))
  (sys::%make-pointer sys:dtp-array
		      (sys:%p-contents-as-locative
			(sys:follow-structure-forwarding instance))))

#+imach
(scl:defsubst pcl-%instance-flavor (instance)
  (sys:%instance-flavor instance))

(scl::defsubst std-instance-p (x)
  (and (sys:instancep x)
       (eq (pcl-%instance-flavor x) (load-time-eval *std-instance-flavor*))))

(scl:defmethod (:print-self std-instance) (stream depth slashify)
  (declare (ignore slashify))
  (print-std-instance scl:self stream depth))

(defmacro %std-instance-wrapper (std-instance)
  `(sys:%instance-ref ,std-instance 1))

(defmacro %std-instance-slots (std-instance)
  `(sys:%instance-ref ,std-instance 2))

(scl:compile-flavor-methods std-instance)


(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (si:%pointer thing)))

;;;
;;; This is hard, I am sweating.
;;; 
(defun function-arglist (function) (zl:arglist function t))

(defun function-pretty-arglist (function) (zl:arglist function))


;;;
;;; This code is adapted from frame-lexical-environment and frame-function.
;;;
(defvar *old-function-name*)
(defvar *boot-state* ())			;Copied from defs.lisp

(defun new-function-name (function)
  (or (and (eq *boot-state* 'complete)
	   (generic-function-p function)
	   (generic-function-name function))
      (funcall *old-function-name* function)))

(eval-when (load)
  (unless (boundp '*old-function-name*)
    (setq *old-function-name* #'si:function-name)
    (setf (symbol-function 'si:function-name) 'new-function-name)))
;
;dbg:
;(progn
;
;(defvar *old-frame-function*)
;
;(defvar *envs->fins* (make-hash-table))
;
;(defun set-env->fin (env fin)
;  (setf (gethash env *envs->fins*) fin))
;
;(defun new-frame-function (frame)
;  (let* ((fn (funcall *old-frame-function* frame))
;	 (location (%pointer-plus frame #+imach (defstorage-size stack-frame) #-imach 0))
;	 (env? #+3600 (location-contents location)
;	       #+imach (%memory-read location :cycle-type %memory-scavenge)))
;    (or (when (and (cl:consp env?)
;		   (not (null (assq :lexical-variable-instructions (debugging-info fn)))))
;	  (gethash env? *envs->fins*))
;	fn)))
;
;(defun pcl::doctor-dfun-for-the-debugger (gf dfun)
;  (when (sys:lexical-closure-p dfun)
;    (let* ((env (si:lexical-closure-environment dfun))
;	   (l2 (last2 env)))
;      (unless (eq (car l2) '.this-is-a-dfun.)
;	(setf (si:lexical-closure-environment dfun)
;	      (nconc env (list '.this-is-a-dfun. gf))))))
;  dfun)
;
;(defun last2 (l)
;  (labels ((scan (2ago tail)
;	     (if (null tail)
;		 2ago
;		 (if (cl:consp tail)
;		     (scan (cdr 2ago) (cdr tail))
;		     nil))))
;    (and (cl:consp l)
;	 (cl:consp (cdr l))
;	 (scan l (cddr l)))))
;
;(eval-when (load)
;  (unless (boundp '*old-frame-function*)
;    (setq *old-frame-function* #'frame-function)
;    (setf (cl:symbol-function 'frame-function) 'new-frame-function)))
;
;)



;; New (& complete) fspec handler.
;;   1. uses a single #'equal htable where stored elements are (fn . plist)
;;       (maybe we should store the method object instead)
;;   2. also implements the fspec-plist operators here.
;;   3. fdefine not only stores the method, but actually does the loading here!
;;

;;;
;;;  genera-low.lisp (replaces old method-function-spec-handler)
;;;

;; New (& complete) fspec handler.
;;   1. uses a single #'equal htable where stored elements are (fn . plist)
;;       (maybe we should store the method object instead)
;;   2. also implements the fspec-plist operators here.
;;   3. fdefine not only stores the method, but actually does the loading here!
;;

(defvar *method-htable* (make-hash-table :test #'equal :size 500))
(si:define-function-spec-handler method (op spec &optional arg1 arg2)
  (if (eq op 'sys:validate-function-spec)
      (and (let ((gspec (cadr spec)))
	     (or (symbolp gspec)
		 (and (listp gspec)
		      (eq (car gspec) 'setf)
		      (symbolp (cadr gspec))
		      (null (cddr gspec)))))
	   (let ((tail (cddr spec)))
	     (loop (cond ((null tail) (return nil))
			 ((listp (car tail)) (return t))
			 ((atom (pop tail)))			 
			 (t (return nil))))))
      (let ((table *method-htable*)
	    (key spec))
	(case op
	  ((si:fdefinedp si:fdefinition)
	   (car (gethash key table nil)))
	  (si:fundefine
	    (remhash key table))
	  (si:fdefine
	    (let ((old (gethash key table nil))
		  (gspec (cadr spec))
		  (quals nil)
		  (specs nil)
		  (ptr (cddr spec)))
	      (setq specs
		    (loop (cond ((null ptr) (return nil))
				((listp (car ptr)) (return (car ptr)))
				(t (push (pop ptr) quals)))))
	      (pcl-fdefine-helper gspec (nreverse quals) specs arg1)
	      (setf (gethash key table) (cons arg1 (cdr old)))))
	  (si:get
	    (let ((old (gethash key table nil)))
	      (getf (cdr old) arg1)))
	  (si:plist
	    (let ((old (gethash key table nil)))
	      (cdr old)))
	  (si:putprop
	    (let ((old (gethash key table nil)))
	      (unless old
		(setf old (cons nil nil))
		(setf (gethash key table) old))
	      (setf (getf (cdr old) arg2) arg1)))
	  (si:remprop
	    (let ((old (gethash key table nil)))
	      (when old
		(remf (cdr old) arg1))))
	  (otherwise
	    (si:function-spec-default-handler op spec arg1 arg2))))))

;; this guy is just a stub to make the fspec handler simpler (and so I could trace it
;; easier).
(defun pcl-fdefine-helper (gspec qualifiers specializers fn)
  (let* ((dlist (scl:debugging-info fn))
	 (class (cadr (assoc 'pcl-method-class dlist)))
	 (doc (cadr (assoc 'pcl-documentation dlist)))
	 (plist (cadr (assoc 'pcl-plist dlist))))
    (load-defmethod (or class 'standard-method)
		    gspec
		    qualifiers
		    specializers
		    (arglist fn)
		    doc
		    (getf plist :isl-cache-symbol)
		    plist
		    fn)))


;; define a few special declarations to get pushed onto the function's debug-info
;; list... note that we do not need to do a (proclaim (declarations ...)) here.
;;
(eval-when (compile load eval)
  (setf (get 'pcl-plist 'si:debug-info) t)
  (setf (get 'pcl-documentation 'si:debug-info) t)
  (setf (get 'pcl-method-class 'si:debug-info) t)
  (setf (get 'pcl-lambda-list 'si:debug-info) t)
)

(eval-when (load eval)
  (setf
    (get 'defmethod      'zwei:definition-function-spec-type) 'defun
    (get 'defmethod-setf 'zwei:definition-function-spec-type) 'defun
    (get 'method 'si:definition-type-name) "method"
    (get 'method 'si:definition-type-name) "method"

    (get 'declass 'zwei:definition-function-spec-type) 'defclass
    (get 'defclass 'si:definition-type-name) "Class"
    (get 'defclass 'zwei:definition-function-spec-finder-template) '(0 1))
  )

;;;
;;; The variable zwei::*sectionize-line-lookahead* controls how many lines the parser
;;;  is willing to look ahead while trying to parse a definition.  Even 2 lines is enough
;;;  for just about all cases, but there isn't much overhead, and 10 should be enough
;;;  to satisfy pretty much everyone... but feel free to change it.
;;;        - MT 880921
;;;
zwei:
(defvar *sectionize-line-lookahead* 3)

zwei:
(DEFMETHOD (:SECTIONIZE-BUFFER MAJOR-MODE :DEFAULT)
	   (FIRST-BP LAST-BP BUFFER STREAM INT-STREAM ADDED-COMPLETIONS)
  ADDED-COMPLETIONS ;ignored, obsolete
  (WHEN STREAM
    (SEND-IF-HANDLES STREAM :SET-RETURN-DIAGRAMS-AS-LINES T))
  (INCF *SECTIONIZE-BUFFER*)
  (LET ((BUFFER-TICK (OR (SEND-IF-HANDLES BUFFER :SAVE-TICK) *TICK*))
	OLD-CHANGED-SECTIONS)
    (TICK)
    ;; Flush old section nodes.  Also collect the names of those that are modified, they are
    ;; the ones that will be modified again after a revert buffer.
    (DOLIST (NODE (NODE-INFERIORS BUFFER))
      (AND (> (NODE-TICK NODE) BUFFER-TICK)
	   (PUSH (LIST (SECTION-NODE-FUNCTION-SPEC NODE)
		       (SECTION-NODE-DEFINITION-TYPE NODE))
		 OLD-CHANGED-SECTIONS))
      (FLUSH-BP (INTERVAL-FIRST-BP NODE))
      (FLUSH-BP (INTERVAL-LAST-BP NODE)))
    (DO ((LINE (BP-LINE FIRST-BP) (LINE-NEXT INT-LINE))
	 (LIMIT (BP-LINE LAST-BP))
	 (EOFFLG)
	 (ABNORMAL T)
	 (DEFINITION-LIST NIL)
	 (BP (COPY-BP FIRST-BP))
	 (FUNCTION-SPEC)
	 (DEFINITION-TYPE)
	 (STR)
	 (INT-LINE)
	 (first-time t)
	 (future-line)				; we actually read into future line
	 (future-int-line)
	 (PREV-NODE-START-BP FIRST-BP)
	 (PREV-NODE-DEFINITION-LINE NIL)
	 (PREV-NODE-FUNCTION-SPEC NIL)
	 (PREV-NODE-TYPE 'HEADER)
	 (PREVIOUS-NODE NIL)
	 (NODE-LIST NIL)
	 (STATE (SEND SELF :INITIAL-SECTIONIZATION-STATE)))
	(NIL)
      ;; If we have a stream, read another line.
      (when (AND STREAM (NOT EOFFLG))
	(let ((lookahead (if future-line 1 *sectionize-line-lookahead*)))
	  (dotimes (i lookahead)		; startup lookahead
	    (MULTIPLE-VALUE (future-LINE EOFFLG)
	      (LET ((DEFAULT-CONS-AREA *LINE-AREA*))
		(SEND STREAM ':LINE-IN LINE-LEADER-SIZE)))
	    (IF future-LINE (SETQ future-INT-LINE (FUNCALL INT-STREAM ':LINE-OUT future-LINE)))
	    (when first-time
	      (setq first-time nil)
	      (setq line future-line)
	      (setq int-line future-int-line))
	    (when eofflg
	      (return)))))

      (SETQ INT-LINE LINE)

      (when int-line
	(MOVE-BP BP INT-LINE 0))		;Record as potentially start-bp for a section

      ;; See if the line is the start of a defun.
      (WHEN (AND LINE
		 (LET (ERR)
		   (MULTIPLE-VALUE (FUNCTION-SPEC DEFINITION-TYPE STR ERR STATE)
		     (SEND SELF ':SECTION-NAME INT-LINE BP STATE))
		   (NOT ERR)))
	(PUSH (LIST FUNCTION-SPEC DEFINITION-TYPE) DEFINITION-LIST)
	(SECTION-COMPLETION FUNCTION-SPEC STR NIL)
	;; List methods under both names for user ease.
	(LET ((OTHER-COMPLETION (SEND SELF ':OTHER-SECTION-NAME-COMPLETION
				      FUNCTION-SPEC INT-LINE)))
	  (WHEN OTHER-COMPLETION
	    (SECTION-COMPLETION FUNCTION-SPEC OTHER-COMPLETION NIL)))
	(LET ((PREV-NODE-END-BP (BACKWARD-OVER-COMMENT-LINES BP ':FORM-AS-BLANK)))
	  ;; Don't make a section node if it's completely empty.  This avoids making
	  ;; a useless Buffer Header section node. Just set all the PREV variables
	  ;; so that the next definition provokes the *right thing*
	  (UNLESS (BP-= PREV-NODE-END-BP PREV-NODE-START-BP)
	    (SETQ PREVIOUS-NODE
		  (ADD-SECTION-NODE PREV-NODE-START-BP
				    (SETQ PREV-NODE-START-BP PREV-NODE-END-BP)
				    PREV-NODE-FUNCTION-SPEC PREV-NODE-TYPE
				    PREV-NODE-DEFINITION-LINE BUFFER PREVIOUS-NODE
				    (IF (LOOP FOR (FSPEC TYPE) IN OLD-CHANGED-SECTIONS
					      THEREIS (AND (EQ PREV-NODE-FUNCTION-SPEC FSPEC)
							   (EQ PREV-NODE-TYPE TYPE)))
					*TICK* BUFFER-TICK)
				    BUFFER-TICK))
	    (PUSH PREVIOUS-NODE NODE-LIST)))
	(SETQ PREV-NODE-FUNCTION-SPEC FUNCTION-SPEC
	      PREV-NODE-TYPE DEFINITION-TYPE
	      PREV-NODE-DEFINITION-LINE INT-LINE))
      ;; After processing the last line, exit.
      (WHEN (OR #+ignore EOFFLG (null line) (AND (NULL STREAM) (EQ LINE LIMIT)))
	;; If reading a stream, we should not have inserted a CR
	;; after the eof line.
	(WHEN STREAM
	  (DELETE-INTERVAL (FORWARD-CHAR LAST-BP -1 T) LAST-BP T))
	;; The rest of the buffer is part of the last node
	(UNLESS (SEND SELF ':SECTION-NAME-TRIVIAL-P)
	  ;; ---oh dear, what sort of section will this be? A non-empty HEADER
	  ;; ---node.  Well, ok for now.
	  (PUSH (ADD-SECTION-NODE PREV-NODE-START-BP LAST-BP
				  PREV-NODE-FUNCTION-SPEC PREV-NODE-TYPE
				  PREV-NODE-DEFINITION-LINE BUFFER PREVIOUS-NODE
				  (IF (LOOP FOR (FSPEC TYPE) IN OLD-CHANGED-SECTIONS
					    THEREIS (AND (EQ PREV-NODE-FUNCTION-SPEC FSPEC)
							 (EQ PREV-NODE-TYPE TYPE)))
				      *TICK* BUFFER-TICK)
				  BUFFER-TICK)
		NODE-LIST)
	  (SETF (LINE-NODE (BP-LINE LAST-BP)) (CAR NODE-LIST)))
	(SETF (NODE-INFERIORS BUFFER) (NREVERSE NODE-LIST))
	(SETF (NAMED-BUFFER-WITH-SECTIONS-FIRST-SECTION BUFFER) (CAR (NODE-INFERIORS BUFFER)))
	(SETQ ABNORMAL NIL)			;timing windows here
	;; Speed up completion if enabled.
	(WHEN SI:*ENABLE-AARRAY-SORTING-AFTER-LOADS*
	  (SI:SORT-AARRAY *ZMACS-COMPLETION-AARRAY*))
	(SETQ *ZMACS-COMPLETION-AARRAY*
	      (FOLLOW-STRUCTURE-FORWARDING *ZMACS-COMPLETION-AARRAY*))
	(RETURN
	  (VALUES 
	    (CL:SETF (ZMACS-SECTION-LIST BUFFER)
		     (NREVERSE DEFINITION-LIST))
	    ABNORMAL))))))

(defun (:property defmethod zwei::definition-function-spec-parser) (bp)
  (zwei:parse-pcl-defmethod-for-zwei bp nil))

;;;
;;; Previously, if a source file in a PCL-based package contained what looks
;;; like flavor defmethod forms (i.e. an (IN-PACKAGE 'non-pcl-package) form
;;; appears at top level, and then a flavor-style defmethod form) appear, the
;;; parser would break.
;;;
;;; Now, if we can't parse the defmethod form, we send it to the flavor
;;; defmethod parser instead.
;;; 
;;; Also now supports multi-line arglist sectionizing.
;;;
zwei:
(defun parse-pcl-defmethod-for-zwei (bp-after-defmethod setfp)
  (block parser
    (flet ((barf (&optional (error t))
	     (return-from parser
	       (cond ((eq error :flavor)
		      (funcall (get 'flavor:defmethod
				    'zwei::definition-function-spec-parser)
			       bp-after-defmethod))
		     (t
		      (values nil nil nil error))))))
      (let ((bp-after-generic (forward-sexp bp-after-defmethod))
	    (qualifiers ())
	    (specializers ())
	    (spec nil)
	    (ignore1 nil)
	    (ignore2 nil))
	(when bp-after-generic
	  (multiple-value-bind (generic error-p)
	      (read-fspec-item-from-interval bp-after-defmethod
					     bp-after-generic)
	    (if error-p
		(barf)				; error here is really bad.... BARF!
		(progn
		  (when (listp generic)
		    (if (and (symbolp (car generic))
			     (string-equal (cl:symbol-name (car generic)) "SETF"))
			(setq generic (second generic)	; is a (setf xxx) form
			      setfp t)
			(barf :flavor)))	; make a last-ditch-effort with flavor parser
		  (let* ((bp1 bp-after-generic)
			 (bp2 (forward-sexp bp1)))
		      (cl:loop
			 (if (null bp2)
			     (barf :more)	; item not closed - need another line!
			     (multiple-value-bind (item error-p)
				 (read-fspec-item-from-interval bp1 bp2)
			       (cond (error-p (barf))	;
				     ((listp item)
				      (setq qualifiers (nreverse qualifiers))
				      (cl:multiple-value-setq (ignore1
								ignore2
								specializers)
					(pcl::parse-specialized-lambda-list item))
				      (setq spec (pcl::make-method-spec 
						   (if setfp
						       `(cl:setf ,generic)
						       generic)
						   qualifiers
						   specializers))
				      (return (values spec
						      'defun
						      (string-interval
							bp-after-defmethod
							bp2))))
				     (t (push item qualifiers)
					(setq bp1 bp2
					      bp2 (forward-sexp bp2))))))))))))))))

zwei:
(progn
  (defun indent-clos-defmethod (ignore bp defmethod-paren &rest ignore)
    (let ((here
	    (forward-over *whitespace-chars* (forward-word defmethod-paren))))
      (loop until (char-equal (bp-char here) #\()
	    do (setf here
		     (forward-over *whitespace-chars* (forward-sexp here))))
      (if (bp-< here bp)
	  (values defmethod-paren nil 2)
	  (values defmethod-paren nil 4))))
  
  (defindentation (pcl::defmethod . indent-clos-defmethod)))

;;;
;;; Teach zwei that when it gets the name of a generic function as an argument
;;; it should edit all the methods of that generic function.  This works for
;;; ED as well as meta-point.
;;;
(zl:advise (flavor:method :SETUP-FUNCTION-SPECS-TO-EDIT zwei:ZMACS-EDITOR)
	   :around
	   setup-function-specs-to-edit-advice
	   ()
  (let ((old-definitions (cadddr arglist))
	(new-definitions ())
	(new nil))
    (dolist (old old-definitions)
      (setq new (setup-function-specs-to-edit-advice-1 old))
      (push (or new (list old)) new-definitions))
    (setf (cadddr arglist) (apply #'append (reverse new-definitions)))
    :do-it))

(defun setup-function-specs-to-edit-advice-1 (spec)
  (and (or (symbolp spec)
	   (and (listp spec) (eq (car spec) 'setf)))
       (gboundp spec)
       (generic-function-p (gdefinition spec))
       (mapcar #'(lambda (m)
		   (make-method-spec spec
				     (method-qualifiers m)
				     (unparse-specializers
				       (method-specializers m))))
	       (generic-function-methods (gdefinition spec)))))

