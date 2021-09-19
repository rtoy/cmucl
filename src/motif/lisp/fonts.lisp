;;;; -*- Mode: Lisp ; Package: Toolkit -*-

(ext:file-comment "$Header: src/motif/lisp/fonts.lisp $")

;;; fonts.lisp -- some machinery for unifying the naming of
;;; traditional Core X11 Fonts with Xft2 fonts. Conceptually almost
;;; all of this this file is not specific to CLM (it's mostly parsing
;;; and some invented heuristics/conventions that could be useful in
;;; X11 context), but it currently only models the minimum properties
;;; of font names necessary to generate the resource specifications
;;; OpenMotif uses to configure fonts. However, the interfaces in this
;;; file were designed to permit retrofitting in a richer model
;;; non-disruptively.

(in-package "TOOLKIT")

;; For reasons that'll be explained as we go, we need to parse (or at
;; least validate) font name strings. Here's the base class for
;; parsing errors.
(define-condition font-name-parse-error (parse-error)
  ((kind :initarg :kind :reader font-name-parse-error-kind)
   (string :initarg :string :reader font-name-parse-error-string)
   (index :initarg :index :reader font-name-parse-error-index)
   (description :initarg :description
		:reader font-name-parse-error-description))
  (:default-initargs :kind nil :description nil)
  (:documentation "Class of error signaled when a string can't be parsed as a font name.")
  (:report
   (lambda (error stream)
     (format stream
	     "Parsing ~S as a font-name~@[ according to ~A syntax~] ended at ~D~@[ ~A~]."
	     (font-name-parse-error-string error)
	     (font-name-parse-error-kind error)
	     (font-name-parse-error-index error)
	     (font-name-parse-error-description error)))))

;; As mentioned, this file currently only offers a trivial model of
;; font specifications. The representation of parsed font names is
;; *not* part of the interface, and subject to change. To insulate
;; prospective clients from that detail, here are some types.
(deftype core-font-name ()
  "Instances of this type are for use with the Core X11 Font system."
  '(satisfies core-font-name-p))
(deftype xlfd-name ()
  "Subtype of CORE-FONT-NAME for XLFD names."
  '(satisfies xlfd-name-p))
(deftype fontconfig-name ()
  "Instances of this type are for use with the Xft2 font system."
  '(satisfies fontconfig-name-p))

;; Core X11 Font names are just strings, ultimately transmitted to the
;; X server for resolution. In general core fonts' names are strings
;; that are opaque to clients. We'll wrap them in an object for
;; discrimination, and let DEFSTRUCT define the predicate we use for
;; the DEFTYPE above.
(defstruct (core-font-name (:type vector) :named (:copier nil))
  (string "" :type string :read-only t))

;; Core X11 Font names can be in XLFD format, but they might
;; not be (e.g., aliases are unlikely to be in XLFD format). Here's
;; the XLFD spec:

;; https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html

;; For now, we don't really need a detailed parse of an XLFD, but
;; we'll pretend as if we've got one. In fact our parser will merely
;; validate the string and then cons up an object for which we've got
;; a predicate.
(defstruct (xlfd-name (:type vector) :named (:copier nil)
		      (:include core-font-name)))

(define-condition xlfd-name-parse-error
    (font-name-parse-error)
  ()
  (:default-initargs :kind "XLFD"))

;; A proper XLFD has 14 hyphens, so 15 fields (inclusive of the
;; registry, which must be the empty string).
(defconstant +xlfd-field-count+ 15)

;; Even though we don't really need a structured XLFD parse, our
;; heuristics require code for validating well-formedness of an XLFD
;; (14 hyphens, optionally excluding wildcards). JUNK-ALLOWED follows
;; the ANSI CL convention. WILDCARD-ALLOWED is just a convenience. It
;; seems that Xorg and Xquartz treat subsets of well-formed XLFDs as
;; usable font names, so this also supports a keyword to make it okay
;; to have fewer than 15 fields).
(defun parse-xlfd-name (string &key (start 0) end
					  junk-allowed subsequence-allowed
					  wildcard-allowed)
  "Parse STRING bounded by START and END as an X Logical Font
Description. If parsing succeeds, return an object for which
XLFD-NAME-P returns true and the index at which parsing
ended. Exceptional conditions: if STRING has a registry but doesn't
have enough fields (13), then signal an error if SUBSEQUENCE-ALLOWED
is false (the default); if string contains a delimiter after the 13th
field, signal an error if JUNK-ALLOWED is false (the default). If
SUBSEQUENCE-ALLOWED is true or JUNK-ALLOWED is true, then return NIL
and the index at which parsing stopped. If WILDCARD-ALLOWED is
false (the default), wildcard characters will cause parsing to end at
the first wildcard character (and so the consequences will depend on
JUNK-ALLOWED); otherwise, wildcard characters will be treated as field
contents."
  (setq end (or end (length string)))
  (let ((index start) (field-count 0))
    (labels
	(;; This is the only way out of PARSE-XLFD-NAME.  It
	 ;; implements all the SUBSEQUENCE-ALLOWED and JUNK-ALLOWED
	 ;; logic. Callers can supply arguments to enrich the error
	 ;; report, though it's not the caller's job to decide whether
	 ;; we've succeeded or not.
	 (finish-parsing (&rest error-description)
	   (if (and (or (= field-count +xlfd-field-count+)
			(and (plusp field-count) subsequence-allowed))
		    (or (= index end) junk-allowed))
	       (return-from parse-xlfd-name
		 (values
		  (make-xlfd-name
		   :string (subseq string start index))
		  index))
	       (error 'xlfd-name-parse-error
		      :string (subseq string start end)
		      :index (- index start)
		      :description
		      (apply #'format nil
			     (if error-description
				 error-description
				 (if (< field-count
					+xlfd-field-count+)
				     (list "with too few fields (~D)"
					   field-count)
				     '("with trailing junk")))))))
	 (next-token ()
	   (loop
	      (when (= index end)
		(return))
	      (when (> (- index start) 255)
		(finish-parsing "due to length limits"))
	      (let ((char (char string index)))
		(cond
		  ;; Fields must be ISO-8859-1 strings.
		  ((> (char-code char) 255)
		   (finish-parsing "due to non-ISO-8859-1 character, ~@C" char))
		  ;; Explicitly disallowed in field values.
		  ((char= #\" char)
		   (finish-parsing "due to a double-quote"))
		  ;; Conditionally allowed.
		  ((and (find (char string index) '(#\? #\*))
			(not wildcard-allowed))
		   (finish-parsing "due to wildcard character, ~@C" char))
		  ;; Field delimiter character, unescapable.
		  ((char= #\- char)
		   (return))
		  (t (incf index)))))
	   (progn (incf field-count)
		  (values index
			  ;; Leave INDEX at END when we're at end of string
			  (when (< index end)
			    (prog1 (char string index)
			      (incf index)))))))
      (unless (< index end)
	(finish-parsing "because the bounded string was empty"))
      (let ((registry-end (next-token)))
	(when (> registry-end start)
	  (finish-parsing "due to unsupported font name registry ~S"
			  (subseq string start registry-end))))
      (loop
	 (let ((delimiter (nth-value 1 (next-token))))
	   (when (= field-count +xlfd-field-count+)
	     (when delimiter
	       (decf index))
	     (finish-parsing))
	   (when (null delimiter)
	     (finish-parsing)))))))

;; Several test cases for PARSE-XLFD-NAME.
#+(or)
(macrolet
  ((test-okay (results string &rest args)
	      `(assert (equalp (ignore-errors
				(multiple-value-list
				 (parse-xlfd-name ,string ,@args)))
			       ',(if results
				     results
				     (list (vector 'core-font-name string 'xlfd-name)
					   (length string))))))
   (test-fail (msg string &rest args &aux (result (gensym)) (error (gensym)))
	      `(multiple-value-bind (,result ,error)
		   (ignore-errors
		    (multiple-value-list
		     (parse-xlfd-name ,string ,@args)))
		 (assert (null ,result))
		 (assert (typep ,error 'xlfd-name-parse-error))
		 ,(when msg
		    `(assert (search ',msg (princ-to-string ,error)))))))
  (test-okay nil "--------------")
  ;; By default, an XLFD with fewer than 14 hyphens is an error.
  (test-fail nil "--------")
  ;; But :SUBSEQUENCE-ALLOWED T will make it allowed.
  (test-okay nil "--------" :subsequence-allowed t)
  ;; By default, a string that has more than 14 fields is an error
  (test-fail nil "--------------nope-")
  ;; But :JUNK-ALLOWED T will make it allowed.
  (test-okay (#(core-font-name "--------------nope" xlfd-name)
	       18)
	     "--------------nope" :junk-allowed t)
  ;; By default, wildcards are disallowed.
  (test-fail nil "-*-------------")
  (test-okay nil "-*-------------" :wildcard-allowed t)
  (test-fail nil "--------------*")
  (test-okay nil "--------------*" :wildcard-allowed t))

;; Xft2 doesn't strictly have its own font names; it uses fontconfig
;; for naming. Fontconfig has a syntax for specifying fonts; here's
;; the spec for that:

;; https://www.freedesktop.org/software/fontconfig/fontconfig-user.html

;; For Motif-y reasons explained below, we must parse a few properties
;; out of fontconfig names. We'll ignore properties we don't care
;; about. We'll use the same basic idea as above: a lightweight
;; representation of the stuff we need, a PARSE-ERROR subclass, and a
;; parsing function.
(defstruct (fontconfig-name (:type vector) :named (:copier nil))
  (foundry nil :type (or null string) :read-only t)
  (family "" :type string :read-only t)
  ;; TODO: SIZE is really a number, but the fontconfig spec doesn't
  ;; document the number format, so for the moment it's a string.
  ;; Probably this ought to get fixed before contemplating exporting
  ;; the accessor name.
  (size nil :type (or null string) :read-only t)
  (weight nil :type (or null string) :read-only t)
  (slant nil :type (or null string) :read-only t)
  ;; This isn't a proper part of a model of a fontconfig name, just an
  ;; internal trick for the heuristics that follow.
  (has-properties-p nil :type boolean :read-only t))

(define-condition fontconfig-name-parse-error
    (font-name-parse-error)
  ()
  (:default-initargs :kind "fontconfig"))

;; This routine attempts to implement a fairly strict idea of
;; well-formedness for fontconfig specs. Any functional disagreement
;; with fontconfig over the domain of well-formed fontconfig names is
;; a bug. (fontconfig's matching of strings that aren't well-formed
;; fontconfig names is none of our business.)
(defun parse-fontconfig-name (string &key (start 0) end junk-allowed)
  (setq end (or end (length string)))
  (let (foundry family size weight slant has-properties-p
		(index start) part-end)
    (labels
	(;; This is the only way out of
	 ;; PARSE-FONTCONFIG-NAME.
	 (finish-parsing (&rest error-description)
	   (if (or (= part-end end) junk-allowed)
	       (return-from parse-fontconfig-name
		 (values
		  (when family
		    (make-fontconfig-name
		     :family family :foundry foundry :size size
		     :weight weight :slant slant
		     :has-properties-p
		     (or has-properties-p slant weight foundry)))
		  ;; Parsing always ends at the index of the end of
		  ;; the part of the name that parsed, even if there's
		  ;; junk after.
		  part-end))
	       (error 'fontconfig-name-parse-error
		      :string (subseq string start end)
		      :index (- index start)
		      :description (when error-description
				     (apply #'format nil error-description)))))
	 ;; Parse the next token starting at INDEX, delimited by any
	 ;; character in DELIMITERS. Note that the family and any
	 ;; property value use backslash to escape the delimiter, but
	 ;; the size and property name are not documented as allowing
	 ;; an escape character. Returns a non-empty token, the
	 ;; delimiter that ended the token, and the delimiter's index.
	 (next-token (delimiters &optional (escapep t))
	   (do ((chars)
		(char (and (< index end) (char string index))
		      (and (< index end) (char string index))))
	       ((or (null char) (find char delimiters))
		(multiple-value-prog1
		    (values (when chars
			      (coerce (nreverse chars) 'string))
			    char
			    index)
		  (incf index)))
	     (when (and escapep (char= #\\ char))
	       (when (= index end)
		 (finish-parsing "after the escape character"))
	       (incf index)
	       (setq char (char string index)))
	     (push char chars)
	     (incf index)))
	 ;; The fontconfig spec doesn't say whether names & their
	 ;; components are matched case-sensitively or
	 ;; case-insensitively. It seems as if it's insensitive, but
	 ;; let's factor it here just in case.
	 (string-equiv (s1 s2)
	   (string-equal s1 s2)))
      (let (delimiter token-end)
	(multiple-value-setq (family delimiter token-end)
	  (next-token '(#\- #\:)))
	(when (null family)
	  (finish-parsing "without any family"))
	;; fontconfig names allow for a comma-separated list of
	;; families. TODO: check if Motif can handle such lists.
	;; Pending that, make it an error to find a comma in the name.
	;; This is a defect in this parser.
	(when (find #\, family)
	  (finish-parsing "with an unsupported syntax (list of families))"))
	;; If we're here, the family is acceptable, so we've reached
	;; the end of this part. Save it for FINISH-PARSING.
	(setq part-end token-end)
	(when (eql #\- delimiter)
	  (multiple-value-setq	(size delimiter token-end)
	    (next-token '(#\:) nil))
	  ;; TODO, maybe: validate that SIZE parses as a number. (But
	  ;; first figure out what the number syntax is; the
	  ;; fontconfig spec doesn't say.)
	  (unless size
	    (finish-parsing "with a hyphen")))
	;; TODO: check if Motif supports lists of sizes.
	;; This is a defect in this parser.
	(when (find #\, size)
	  (finish-parsing "with an unsupported syntax (list of sizes))"))
	(setq part-end token-end)
	(when (eql #\: delimiter)      ;There are properties to parse.
	  (let (name value tmp-end)
	    (loop
	       (setq part-end token-end)
	       ;; We must not set TOKEN-END until we know we've parsed
	       ;; a whole property. So we'll use TMP-END.
	       (multiple-value-setq (name delimiter tmp-end)
		 (next-token '(#\= #\:) nil))
	       (if (null name)
		   (ecase delimiter
		     (#\=
		      (finish-parsing "with an empty property name"))
		     (#\:
		      (finish-parsing "with an empty property"))
		     ((nil)
		      (finish-parsing "with a colon")))
		   (ecase delimiter
		     (#\=
		      (multiple-value-setq (value delimiter tmp-end)
			(next-token '(#\:)))
		      (when (null value)
			(finish-parsing "with an empty property value"))
		      (setq token-end tmp-end
			    has-properties-p t)
		      ;; These are the only properties we care about.
		      (cond ((string-equiv name "weight")
			     (setq weight value))
			    ((string-equiv name "slant")
			     (setq slant value))
			    ((string-equiv name "foundry")
			     (setq foundry value))))
		     ((#\: nil)
		      ;; In this case, the property might be a
		      ;; "symbolic constant" The fontconfig spec says
		      ;; "there are symbolic constants that
		      ;; simultaneously indicate both a name and a
		      ;; value", but it's not clear what those
		      ;; constants are. We'll assume that any
		      ;; construct is both syntactically valid here.
		      (setq token-end tmp-end
			    has-properties-p t)
		      ;; We need to recognize whatever symbolic
		      ;; constants are defined for the weight and
		      ;; slant properties. These are taken from the
		      ;; description of the <const> element of the
		      ;; configuration file format, in case that's
		      ;; what's intended in the fontconfig spec.
		      (cond ((member name
				     '("thin"
				       "extralight"
				       "ultralight"
				       "light"
				       "demilight"
				       "semilight"
				       "book"
				       "regular"
				       "normal"
				       "medium"
				       "demibold"
				       "semibold"
				       "bold"
				       "extrabold"
				       "black"
				       "heavy")
				     :test #'string-equiv)
			     (setq weight name))
			    ((member name
				     '("roman"
				       "italic"
				       "oblique"
				       "ultracondensed"
				       "extracondensed"
				       "condensed"
				       "semicondensed"
				       "normal"
				       "semiexpanded"
				       "expanded"
				       "extraexpanded"
				       "ultraexpanded")
				     :test #'string-equiv)
			     (setq slant name))))))))))
      (finish-parsing))))

;; Some test cases for PARSE-FONTCONFIG-NAME.
#+(or)
(macrolet
    ((test-okay (results string &rest args)
       `(assert (equalp (ignore-errors
			 (multiple-value-list
			  (parse-fontconfig-name ,string ,@args)))
			',(if (listp results)
			      results
			      (list results (length string))))))
     (test-fail (msg string &rest args &aux (result (gensym)) (error (gensym)))
       `(multiple-value-bind (,result ,error)
	    (ignore-errors
	     (multiple-value-list
	      (parse-fontconfig-name ,string ,@args)))
	  (assert (null ,result))
	  (assert (typep ,error 'fontconfig-name-parse-error))
	  ,(when msg
	     `(assert (search ',msg (princ-to-string ,error)))))))
  ;; Just a name
  (test-okay #(fontconfig-name nil "Foo" nil nil nil nil) "Foo")
  ;; Name and size
  (test-okay #(fontconfig-name nil "Foo" "12" nil nil nil) "Foo-12")
  ;; This fully specifies everything we care about.
  (test-okay #(fontconfig-name "Bar" "Foo" "12" "bold" "italic" t)
	     "Foo-12:foundry=Bar:slant=italic:weight=bold")
  ;; Same as previous, but with extra junk (which should be ignored).
  (test-okay #(fontconfig-name "Bar" "Foo" "12" "bold" "italic" t)
	     "Foo-12:abc=def:foundry=Bar:xyz=123:slant=italic:weight=bold")
  ;; Test recognition of symbolic constants for weight and slant.
  (test-okay #(fontconfig-name nil "Foo" "12" "bold" "italic" t)
	     "Foo-12:italic:bold")
  ;; Test recognition that a font has properties (even if we don't
  ;; know what they are).
  (test-okay #(fontconfig-name nil "Foo" "12" nil nil t)
	     "Foo-12:bar=baz")
  ;; Test various invalid (I think) things.
  (test-fail "with a hyphen" "Foo-")
  (test-fail "with a colon" "Foo:")
  (test-fail "with a colon" "Foo-12:")
  (test-fail "empty property" "Foo::")
  (test-fail "empty property" "Foo-12::")
  (test-fail "empty property name" "Foo:=bar")
  (test-fail "empty property value" "Foo:bar="))

;; Now that we have font name parsers, let's build a convention for
;; figuring out when to apply them. Every octet string up to length
;; 255 is a syntactically valid Core X11 Font name; and fontconfig
;; appears not to care whether its input strings are well-formed
;; fontconfig names. So in principle all strings (modulo length and
;; encoding) might be "usable" as a font names in either system.
;;
;; However, in practice, most Core X11 Fonts have XLFD names, and
;; fontconfig's behavior is more predictable when its inputs are
;; well-formed and detailed fontconfig names. Therefore, it seems
;; reasonable to build up some heuristics:
;;
;; 1. a string that starts with a hyphen is an XLFD (fontconfig name
;; can't start with hyphens).
;;
;; 2. a string that's a well-formed fontconfig name containing a colon
;; is a fontconfig name (colons don't seem much used in Core X11 Font
;; names).
;;
;; Here are two helper routines that implement those heuristics.  Note
;; that these two don't partition all strings, e.g., "Times" or
;; "Helvetica-12" won't satisfy either predicate. We'll address those
;; "ambiguous" cases below.
(defun xlfdp (thing)
  "Returns true if THING represents an X Logical Font Description, either
as an XLFD string or the parse of one."
  (etypecase thing
    (xlfd-name
     ;; Note that objects that satisfy this predicate might have been
     ;; created by PARSE-XLFD-NAME calls with non-default
     ;; flags, and so may not be well-formed XLFDs on their own.  If
     ;; the user had the context to do that, then we're not going to
     ;; overrule the decision.
     thing)
    (string (nth-value
	     0
	     (ignore-errors
	      (parse-xlfd-name
	       thing
	       ;; These initargs are arbitrary, but appear to agree
	       ;; with what my X server seems to consider acceptable
	       ;; arguments to XOpenFont.
	       :junk-allowed nil :subsequence-allowed t
	       :wildcard-allowed t))))))

(defun fontconfigp (thing)
  "Returns true in case THING is probably a fontconfig name:
either a parsed fontconfig name, or a string that parses to a
fontconfig name having explicit properties."
  (etypecase thing
    (fontconfig-name
     thing)
    (string
     (let ((thing (ignore-errors
		   (parse-fontconfig-name thing))))
       (when (and thing (fontconfig-name-has-properties-p thing))
	 thing)))))

;; So now we've got heuristic detection of XLFD and fontconfig
;; names. Disambiguating other strings in isolation is inherently
;; arbitrary. However, when we've got an opportunity to look at a set
;; of strings, we can disambiguate using context: let's assume that if
;; any string is an XLFD, then all ambiguous strings are meant as Core
;; X11 Font names; that if any string is a well-formed fontconfig name
;; with properties, then all ambiguous strings are also for Xft2; that
;; if all strings are ambiguous, we'll fallthru to consulting a
;; variable.
(defvar *ambiguous-font-disposition* :xft2)
(declaim (type (member :xft2 :core) *ambiguous-font-disposition*))

;; Finally, it seems that using Core X11 Fonts with Xft2 fonts within
;; a single RenderTable that doesn't work in OpenMotif circa 2021. (I
;; couldn't figure it out, anyhow.) And maybe nobody would want to do
;; so anyway. So for now we'll rule out mix-and-match scenarios.
(defun heuristicate-font-name-types (names)
  (assert (every #'stringp names))
  (flet ((parse-as-core-fonts ()
	   (mapcar #'(lambda (spec)
		       (or (xlfdp spec) (make-core-font-name :string spec)))
		   names))
	 (parse-as-xft2-fonts ()
	   (mapcar #'parse-fontconfig-name names)))
    (cond ((some #'xlfdp names)
	   (when (some #'fontconfigp names)
	     (error "Can't mix fontconfig and Core X11 font names."))
	   (parse-as-core-fonts))
	  ((some #'fontconfigp names)
	   (when (some #'xlfdp names)
	     (error "Can't mix fontconfig and Core X11 font names."))
	   (parse-as-xft2-fonts))
	  (t (ecase *ambiguous-font-disposition*
	       (:core (parse-as-core-fonts))
	       (:xft2 (parse-as-xft2-fonts)))))))

;; Here's the OpenMotif-specific bit. Now that we can heuristically
;; classify a list of fonts, we can pair up tags with heuristicated
;; font names in order to generate OpenMotif resource strings suitable
;; for either fallback resources or writing into X resource files.
(defun generate-heuristicated-font-resources
    (tags fonts &key application-name application-class)
  "Generate a list of OpenMotif RenderTable & Rendition resources
associating FONTS with TAGS. If APPLICATION-NAME or APPLICATION-CLASS
is supplied, the resource keys will be prefixed by that string;
otherwise, the resource key will start with the loose binding
operator, asterisk."
  (declare (type list tags fonts)
	   (type (or null string) application-name application-class))
  (let ((ntags (length tags))
	(nfonts (length fonts)))
    (assert (= ntags nfonts) (tags fonts)
	    "Too ~:[many~;few~] tags (~A) for fonts (~A)."
	    (> ntags nfonts) tags fonts))
  (let ((name/class (or application-name application-class)))
    (nconc
     (mapcan
      (lambda (tag spec)
	(let* ((rendition (if (or (string= "" tag) (null tag))
			      ;; Accept NIL or "" as a the default
			      ;; tag. Default tags' resources are
			      ;; resources of the RenderTable itself.
			      "renderTable"
			      ;; Non-default tags get used as resource
			      ;; names.
			      tag)))
	  ((lambda (resources)
	     (loop for (resname resval) on resources by #'cddr
		   collect (format nil "~@[~A~]*~A.~A: ~A"
				   name/class rendition resname resval)))
	   ;; Core fonts are specified by 2 resource name/value pairs.
	   ;; Xft2 fonts are specified by 4 such pairs.
	   (if (core-font-name-p spec)
	       (list "fontName" (core-font-name-string spec)
		     "fontType" "FONT_IS_FONT")
	       (list* "fontName" (fontconfig-name-family spec)
		      "fontType" "FONT_IS_XFT"
		      (nconc
		       (when (fontconfig-name-foundry spec)
			 (list "foundryName" (fontconfig-name-foundry spec)))
		       (when (fontconfig-name-size spec)
			 (list "fontSize" (fontconfig-name-size spec)))
		       (when (or (fontconfig-name-weight spec)
				 (fontconfig-name-slant spec))
			 (list "fontStyle"
			       (format nil "~:[~@[~A~]~;~:*~A~@[ ~A~]~]"
				       (fontconfig-name-weight spec)
				       (fontconfig-name-slant spec))))))))))
      tags (heuristicate-font-name-types fonts))
     (list (format nil "~@[~A~]*renderTable: ~{~A~^ ~}"
		   name/class
		   (remove "" tags))))))

;; Test cases for GENERATE-HEURISTICATED-FONT-RESOURCES.
#+(or)
(assert
 (equal
  ;; These are the Core X11 Fonts that the CLM Debugger/Inspector have
  ;; used.
  (let ((fonts '("-adobe-helvetica-medium-r-normal--*-120-75-*"
		 "-adobe-helvetica-bold-r-normal--*-120-75-*"
		 "-adobe-helvetica-medium-o-normal--*-120-75-*"))
	(tags '("" "header" "italic")))
    (generate-heuristicated-font-resources tags fonts))
  '("*renderTable.fontName: -adobe-helvetica-medium-r-normal--*-120-75-*"
    "*renderTable.fontType: FONT_IS_FONT"
    "*header.fontName: -adobe-helvetica-bold-r-normal--*-120-75-*"
    "*header.fontType: FONT_IS_FONT"
    "*italic.fontName: -adobe-helvetica-medium-o-normal--*-120-75-*"
    "*italic.fontType: FONT_IS_FONT" "*renderTable:  header italic")))

#+(or)
(assert
 (equal
  ;; Here are some fontconfig names.
  (let ((fonts '("Sans-12:regular"
		 "Sans-12:bold"
		 "Sans-12:italic"
		 "Sans-12:bold:italic"))
	(tags '("" "header" "italic" "foo")))
    (generate-heuristicated-font-resources tags fonts))
  '("*renderTable.fontName: Sans" "*renderTable.fontType: FONT_IS_XFT"
    "*renderTable.fontSize: 12" "*renderTable.fontStyle: regular"
    "*header.fontName: Sans" "*header.fontType: FONT_IS_XFT"
    "*header.fontSize: 12" "*header.fontStyle: bold"
    "*italic.fontName: Sans" "*italic.fontType: FONT_IS_XFT"
    "*italic.fontSize: 12" "*italic.fontStyle: italic"
    "*foo.fontName: Sans" "*foo.fontType: FONT_IS_XFT"
    "*foo.fontSize: 12" "*foo.fontStyle: bold italic"
    "*renderTable:  header italic foo")))
