;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/ryu-print.lisp $")
;;;
;;; **********************************************************************
;;;
;;; CMU Common Lisp interface to Ryu-printf
;;;
(in-package "LISP")
(intl:textdomain "cmucl")

(defvar *use-ryu-printer* t
  "When non-NIL, format directives ~E, ~F, ~G and the float printer used
  by PRINC/PRIN1/PRINT/WRITE route through the Ryu-based formatter
  (FORMAT-E, FORMAT-F, FORMAT-G, and the shortest-form D2S/F2S
  routines).  When NIL, the original Burger and Dybvig based code is
  used.  Some cases (notably ~F with non-zero scale factor K, and
  printing of DOUBLE-DOUBLE-FLOAT values) always fall back to the
  Burger and Dybvig code regardless of this variable.")

;;; Ryu interface

;; This could be larger, but that would mean larger stack size for the
;; foreign call.  This should be large enough for practical use.
(defconstant +d2fixed-max-precision+
  1000
  "Maximum precision (fractional digits for d2fixed.")

(defconstant +d2fixed-buffer-size+
  ;; +320 to account for 300 ingeger digts + sign + dot + terminator +
  ;; slack.
  (+ +d2fixed-max-precision+ 320)
  "Buffer size for d2fixed.")

(defun d2fixed (d precision)
  "Lisp interface to Ryu d2fixed routine (specically d2fixed_buffered).
  D is the number to convert and PRECISION is the number of digits
  after the decimal point.  The total number of digits could be more."
  (declare (double-float d)
	   (type (integer 0 #.+d2fixed-max-precision+) precision))
  (alien:with-alien ((buf (alien:array c-call:char #.+d2fixed-buffer-size+)))
    (alien:alien-funcall
     (alien:extern-alien "d2fixed_buffered"
			 (function c-call:void
				   c-call:double
				   c-call:unsigned-int
				   (* c-call:char)))
     d
     precision
     (alien:cast buf (* c-call:char)))
    (alien:cast buf c-call:c-string)))

(defun d2exp (d precision)
  "Lisp interface to Ryu d2exp (specifically d2exp-buffered).  D is the
  number to convert and PRECISION is the number of digits after the
  decimal point.  The result is of the form \"d.ddddEeee\"."
  (declare (double-float d)
	   (type (integer 0 #.+d2fixed-max-precision+) precision))
  (alien:with-alien ((buf (alien:array c-call:char #.+d2fixed-buffer-size+)))
    (alien:alien-funcall
     (alien:extern-alien "d2exp_buffered"
			 (function c-call:void
				   c-call:double
				   c-call:unsigned-int
				   (* c-call:char)))
     d
     precision
     (alien:cast buf (* c-call:char)))
    (alien:cast buf c-call:c-string)))
			 
(defun d2s (d)
  "Lisp interface to Ryu d2s (specifically d2s_buffered.  D is the number
  to convert and the result is the shortest string that reproduces the
  value when read back in."
  (declare (double-float d))
  (alien:with-alien ((buf (alien:array c-call:char #.+d2fixed-buffer-size+)))
    (alien:alien-funcall
     (alien:extern-alien "d2s_buffered"
			 (function c-call:void
				   c-call:double
				   (* c-call:char)))
     d
     (alien:cast buf (* c-call:char)))
    (alien:cast buf c-call:c-string)))

(defun f2s (s)
  "Lisp interface to Ryu f2s (specifically f2s_buffered.  D is the number
  to convert and the result is the shortest string that reproduces the
  value when read back in."
  (declare (single-float s))
  (alien:with-alien ((buf (alien:array c-call:char 16)))
    (alien:alien-funcall
     (alien:extern-alien "f2s_buffered"
                         (function c-call:void
                                   c-call:float
                                   (* c-call:char)))
     s
     (alien:cast buf (* c-call:char)))
    (alien:cast buf c-call:c-string)))

(declaim (inline float-to-string))
(defun float-to-string (f)
  "Convert F, a single-float or double-float, to a string of the shortest
  form."
  (declare (type (or single-float double-float) f))
  (etypecase f
    (double-float (d2s (abs f)))
    (single-float (f2s (abs f)))))

(defun parsed-exp-form (raw-string)
  "Parse RAW-STRING which is a number in exponential form and return the
  mantissa part as a string and the exponent part as an integer.

  RAW-STRING is of the form \"d.ddddEeee\" where the exponent marker
  must exist and must be \"e\" or \"E\"."
  ;; Parse RAW-STRING, that is in exponential form.  That is, it must
  ;; the form "d.dddEeee".  There cannot be a leading sign.  Returns 
  (let* ((e-pos (position-if #'(lambda (c)
				 (member c '(#\e #\E)))
			     raw-string))
	 (mantissa (subseq raw-string 0 e-pos))
	 (exp (parse-integer raw-string :start (1+ e-pos))))
    ;; The exponent from a double-float is in the range of [-324,
    ;; 308].  Just declare the exponent as a (signed-byte 11).
    (values mantissa
	    (truly-the (signed-byte 11) exp))))

(declaim (inline mantissa-digit-count))
(defun mantissa-digit-count (mantissa)
  "Return the number of significant digits in MANTISSA, a string of
   the form \"d\" or \"d.dddd\" (as produced by d2s/d2exp).  The decimal
   point, if present, is not counted."
  (declare (type string mantissa))
  (- (length mantissa)
     (if (find #\. mantissa) 1 0)))

(defun count-decimal-digits (n)
  "Number of decimal digits in N.  N is the absolute value of a
   double-float's exponent, so 0 <= n <= 324."
  (declare (type (integer 0 324) n))
  (cond ((< n 10)  1)
        ((< n 100) 2)
        (t         3)))

(declaim (inline exponent-digit-count))
(defun exponent-digit-count (e actual-exp)
  "Number of decimal digits needed to display the ACTUAL-EXP in a width
  of E.  If E is NIL, the actual number of digits is returned.
  Otherwise, the max of E and the number of digits is returned."
  (declare (type (or fixnum null) e)
	   (fixnum actual-exp))
  (max (or e 1)
       (count-decimal-digits (abs actual-exp))))

(defun compute-d-for-width (w e k actual-exp is-negative-p at-sign-p)
  "Find the largest d (precision) value that fits in width W given the
  actual exponent (adjusted by k), and whether signs are printed or
  not.

  Returns NIL if no width d can fit in a field of length w."
  (declare (fixnum k actual-exp))
  (let* ((sign-len (if (or is-negative-p at-sign-p) 1 0))
	 (exp-digits (exponent-digit-count e actual-exp))
	 ;; The min output includes the leading sign, and the length
	 ;; of the exponent.  If k > 0, we have a leading digit, a
	 ;; dot, the exponent marker and exponent sign for 4 extra.
	 ;; Otherwise, we have just the dot, exponent marker and
	 ;; exponent sign.
	 (fixed-len (+ sign-len exp-digits
		       (if (plusp k) 4 3)))
	 (d-fit (- w fixed-len)))
    ;; Overflow happens if
    ;;   * d-fit is negative (no digits can be printed)
    ;;   * if k > 0, we need k digits before the dot, and d-fit is too
    ;;     small.
    (cond ((<= d-fit 0)
	   nil)
	  ((and (plusp k) (< d-fit k))
	   nil)
	  (t
	   d-fit))))

(defun compute-exp-output-length (mantissa actual-exp k e is-negative-p at-sign-p
				  drop-leading-zero-p)
  "Compute length of the ~E result with the given parameters, but don't
  build the string.  MANTISSA is \"d[.dddd]\" from d2exp or
  d2s. ACTUAL-EXP is the exponent with scaling factor applied.  If
  DROP-LEADING-ZERO-P is non-NIL, the leading \"0\" before the dot (in
  the K <= 0 form) is omitted from the length."
  (declare (type simple-string mantissa)
	   (fixnum actual-exp k)
	   (type (or null fixnum) e))
  (let* ((sign-len (if (or is-negative-p at-sign-p) 1 0))
         (exp-digits (exponent-digit-count e actual-exp))
         (raw-digits (mantissa-digit-count mantissa))
         (mantissa-len
           (cond
             ((plusp k)
              (cond
                ((> raw-digits k)
                 (1+ raw-digits))	; "D.DDDD"
                ((and (= k 1) (= raw-digits 1))
                 3)			; "D.0" forced
                (t
                 (max raw-digits k))))	; "DDDD"
             (t
              ;; "0.000DDD" or ".000DDD"
              (+ (if drop-leading-zero-p 0 1)
                 1			; the dot
                 (- k)			; leading zeros after the dot
                 raw-digits)))))
    ;; +2 for the exponent marker and sign
    (the (and unsigned-byte fixnum)
		 (+ sign-len mantissa-len exp-digits 2))))

(defun scale-mantissa (stream mantissa-text k drop-leading-zero-p)
  "Write MANTISSA-TEXT to STREAM, shifting the decimal point so that
   the new dot lands K digits in from the start of the digit string.
   MANTISSA-TEXT is 'D' or 'D.DDDD' from d2s/d2exp.
   DROP-LEADING-ZERO-P controls whether the leading '0' before the dot
   is emitted when the result starts with '0.'.  K=0 puts the dot
   before all digits; K=1 leaves it at the original position."
  (declare (type simple-string mantissa-text)
           (fixnum k))
  ;; Fast path: k=1, mantissa already in target form.
  (when (= k 1)
    (write-string mantissa-text stream)
    (unless (find #\. mantissa-text)
      (write-string ".0" stream))
    (return-from scale-mantissa))
  (let* ((has-dot     (find #\. mantissa-text))
         (lead-char   (char mantissa-text 0))
	 ;; Where the stuff after the dot starts
         (tail-start  (if has-dot 2 1))
         (tail-len    (- (length mantissa-text) tail-start))
         (digit-count (1+ tail-len)))
    ;; Several cases to consider:
    ;;
    ;; * k is negative so the new dot preceeds the mantissa.  Pad with
    ;;   zeroes.
    ;; * k is so large that the dot is past the rightmost part of the
    ;;   mantissa.  Append enough zeroes and then add ".0"
    ;; * k is somewhere in the mantissa.  Handling of this case
    ;;   depends on if the new dot is to the left or to the right of
    ;;   the original dot.
    (cond
      ;; Case 2: new dot at or before position 0.
      ((<= k 0)
       ;; Write "0." or "."
       (unless drop-leading-zero-p
	 (write-char #\0 stream))
       (write-char #\. stream)
       ;; Insert zeros after the decimal but before the mantissa.
       (loop repeat (- k)
	     do (write-char #\0 stream))
       ;; Add the mantissa, skipping any existing dot.
       (write-char lead-char stream)
       (when (plusp tail-len)
         (write-string mantissa-text stream :start tail-start)))
      ;; Case 1b: 1 < k < digit-count.
      ((< k digit-count)
       ;; Output the original mantissa up to the original dot.
       (write-char lead-char stream)
       (write-string mantissa-text stream
                     :start tail-start
                     :end (+ tail-start k -1))
       ;; Output the dot at the new position.
       (write-char #\. stream)
       ;; Output everything after the original dot.
       (write-string mantissa-text stream :start (+ tail-start k -1)))
      ;; Case 3: k >= digit-count.
      (t
       ;; Output the original mantissa, then everything after the
       ;; dot (if there was one).
       (write-char lead-char stream)
       (when (plusp tail-len)
         (write-string mantissa-text stream :start tail-start))
       ;; Pad with 0's
       (loop repeat (- k digit-count)
	     do (write-char #\0 stream))
       ;; Finish with ".0"
       (write-string ".0" stream)))))

(declaim (inline pad-overflow))
(defun pad-overflow (stream field-len w overflowchar padchar field-writer)
  "Apply width/pad/overflow rules.  FIELD-WRITER is a thunk of zero
   arguments that writes the field characters to STREAM."
  (declare (fixnum field-len)
	   (type (or null fixnum) w)
	   (function field-writer))
  (cond
    ((null w)
     (funcall field-writer))
    ((and (> field-len w)
	  overflowchar)
     (loop repeat w do
       (write-char overflowchar stream)))
    ((> field-len w)
     (funcall field-writer))
    (t
     (let ((pad (or padchar #\space)))
       (loop repeat (- w field-len)
             do (write-char pad stream)))
     (funcall field-writer))))

(declaim (inline get-sign-and-absolute-value))
(defun get-sign-and-absolute-value (value)
  "Returns two values for the float VALUE: T if the value is negative and
  the double-float absolute value of VALUE."
  (declare (type (or single-float double-float) value))
  (etypecase value
    (double-float
     (values (minusp (float-sign value))
	     (abs value)))
    (single-float
     (values (minusp (float-sign value))
	     (abs (float value 1d0))))))
     
(defun d2exp-precision (d k)
  "Compute precision for d2exp when CL requests D digits and the
  scale factor is K."
  (cond ((plusp k)
	 ;; k digits before the decimal, d-k+1 after, so D is the
	 ;; right precision for d2exp.
	 d)
	((zerop k)
	 ;; Output is 0.dddd, so d2exp needs one less digit.
	 (max (1- d) 0))
	(t
	 ;; 0.000...ddd with d-|k| signfficant digits.  So d2exp needs
	 ;; d - |k| - 1 digits after the decimal point.
	 (max (+ d k -1) 0))))
		  
(defun write-exponent (stream shown-exp e exponentchar)
  "Write the ~E exponent tail \"[marker][sign][digits]\" to STREAM.
   E is the minimum exponent-digit count (zero-padded if shorter);
   EXPONENTCHAR overrides the marker (default #\\d)."
  (declare (fixnum shown-exp)
	   (type (or null fixnum) e))
  (let* ((exp-abs    (abs shown-exp))
	 (exp-digits (count-decimal-digits exp-abs))
	 (exp-width  (max (or e 1) exp-digits)))
    (write-char (or exponentchar #\d) stream)
    (write-char (if (minusp shown-exp) #\- #\+) stream)
    (loop repeat (- exp-width exp-digits)
	  do (write-char #\0 stream))
    (princ exp-abs stream)))

(defun format-e-string (stream mantissa exponent is-negative-p w e k
                        overflowchar padchar exponentchar at-sign-p
                        drop-leading-zero-p)
  "Write the ~E representation of MANTISSA * 10^EXPONENT to STREAM,
  right-justified in a field of width W.  If specified, the PADCHAR is
  inserted.  If OVERFLOWCHAR is given and the result won't fit in the
  field, OVERFLOWCHAR replaces the result.

  K is the scale factor causing the decimal point to be placed K
  digits in from the start and the displayed exponent is adjusted
  appropriately."
  (declare (type simple-string mantissa)
           (fixnum exponent k))
  (let* ((shown-exp (- exponent (1- k)))
         (field-len (compute-exp-output-length mantissa shown-exp k e
                                               is-negative-p at-sign-p
                                               drop-leading-zero-p)))
    (flet ((write-field ()
             (cond (is-negative-p (write-char #\- stream))
                   (at-sign-p     (write-char #\+ stream)))
             (scale-mantissa stream mantissa k drop-leading-zero-p)
	     (write-exponent stream shown-exp e exponentchar)))
      (declare (dynamic-extent #'write-field))
      (pad-overflow stream field-len w overflowchar padchar #'write-field))))

(defun emit-exp-no-fraction (stream mantissa shown-exp int-digits
			     is-negative-p at-sign-p
			     w e overflowchar padchar exponentchar)
  "Emit \"[sign][INT-DIGITS].[marker][exp-sign][exp]\" with no
   fractional digits, right-justified in width W.  MANTISSA is a
   string of significant digits from d2s/d2exp, with or without an
   internal dot; the integer digits are written from it (skipping the
   dot if any), zero-padded to INT-DIGITS."
  (declare (type simple-string mantissa)
	   (fixnum shown-exp int-digits)
	   (type (or null fixnum) w e))
  (let* ((dotpos     (position #\. mantissa))
	 (mant-len   (length mantissa))
	 (sig-digits (mantissa-digit-count mantissa))
	 (exp-width  (exponent-digit-count e shown-exp))
	 (sign-len   (if (or is-negative-p at-sign-p) 1 0))
	 ;; sign + int + dot + marker + exp-sign + exp-width
	 (field-len  (+ sign-len int-digits 3 exp-width)))
    (flet ((write-field ()
	     (cond (is-negative-p (write-char #\- stream))
		   (at-sign-p     (write-char #\+ stream)))
	     ;; Write up to sig-digits from mantissa (skipping the dot),
	     ;; then zero-pad if int-digits exceeds available.
	     (let ((available (min int-digits sig-digits)))
	       (cond ((null dotpos)
		      (write-string mantissa stream :start 0 :end available))
		     ((<= available dotpos)
		      (write-string mantissa stream :start 0 :end available))
		     (t
		      (write-string mantissa stream :start 0 :end dotpos)
		      (write-string mantissa stream
				    :start (1+ dotpos)
				    :end (min mant-len (1+ available))))))
	     (loop repeat (- int-digits sig-digits)
		   do (write-char #\0 stream))
	     (write-char #\. stream)
	     (write-exponent stream shown-exp e exponentchar)))
      (declare (dynamic-extent #'write-field))
      (pad-overflow stream field-len w overflowchar padchar #'write-field))))

(defun format-e (stream value w d e k overflowchar padchar exponentchar at-sign-p)
  (declare (type (or single-float double-float) value)
           (fixnum k)
           (type (or null (and unsigned-byte fixnum)) w d e)
           (optimize (speed 3)))
  (multiple-value-bind (is-negative-p abs-value)
      (get-sign-and-absolute-value value)
    (cond
      (d
       (multiple-value-bind (mantissa exponent)
           (parsed-exp-form (d2exp abs-value
				   (d2exp-precision d k)))
	 (cond
	   ;; CLHS 22.3.3.2: with D specified, exactly d-(k-1)
	   ;; fractional digits appear after the dot.  When that is 0
	   ;; (k >= d+1, plusp k), emit "[int].e[exp]" directly with
	   ;; no forced ".0" -- the d2exp result has been rounded to
	   ;; exactly the digits we need to show.  The k=1, d=0 case
	   ;; is excluded: cmucl has always emitted "D.0eN" there and
	   ;; existing tests rely on it.
	   ((and (>= k 2) (>= k (1+ d)))
	    (emit-exp-no-fraction stream mantissa
				  (- exponent (1- k))
				  k is-negative-p at-sign-p
				  w e overflowchar padchar exponentchar))
	   (t
	    (format-e-string stream mantissa exponent is-negative-p
			     w e k overflowchar padchar exponentchar at-sign-p nil)))))
      (t
       (multiple-value-bind (mantissa exponent)
           (parsed-exp-form (float-to-string value))
         (let* ((actual-exp (- exponent (1- k)))
                (full-len (compute-exp-output-length mantissa actual-exp k e
                                                     is-negative-p at-sign-p
                                                     nil)))
           (cond
             ((or (null w)
		  (<= full-len w))
              (format-e-string stream mantissa exponent is-negative-p
                               w e k overflowchar padchar exponentchar at-sign-p
                               (not (plusp k))))
             (t
              (let* ((d-fit (compute-d-for-width w e k actual-exp is-negative-p at-sign-p))
                     (drop-zero-p (and d-fit (<= k 0))))
                (cond
                  (d-fit
                   (multiple-value-bind (mantissa exponent)
                       (parsed-exp-form (d2exp abs-value
					       (d2exp-precision d-fit k)))
                     (format-e-string stream mantissa exponent is-negative-p
                                      w e k overflowchar padchar exponentchar at-sign-p
                                      drop-zero-p)))
                  (overflowchar
                   ;; Not even the no-fractional-digit form fits and
                   ;; an overflow char was supplied: fill W.
                   (loop repeat w do (write-char overflowchar stream)))
                  ((= k 1)
                   ;; No fractional digit fits.  Emit "[d].e[exp]"
                   ;; directly -- no forced ".0".  CLHS 22.3.3.2
                   ;; "single zero digit ... if the width w permits";
                   ;; here it does not, so the field overflows W.
                   ;; Handled only for k=1 (default); other k values
                   ;; fall through to the old format-e-string path.
                   (multiple-value-bind (one-digit shown-exp)
                       (parsed-exp-form (d2exp abs-value 0))
                     (emit-exp-no-fraction stream one-digit shown-exp 1
					   is-negative-p at-sign-p
					   w e overflowchar padchar
					   exponentchar)))
                  (t
                   ;; k != 1 and no width fits: fall back to the
                   ;; shortest form (may still emit a forced ".0";
                   ;; rare, not yet handled).
                   (format-e-string stream mantissa exponent is-negative-p
                                    w e k overflowchar padchar exponentchar at-sign-p
                                    nil))))))))))))

;;; Ryu ~F
(defun emit-fixed (stream rounded is-negative-p w
		   overflowchar padchar at-sign-p
		   force-trailing-zero-p value-is-nonzero-p)
  "Write a d2fixed result ROUNDED right-justified in W with the usual
   ~F padding and overflow rules.  FORCE-TRAILING-ZERO-P appends a
   single \"0\" after the dot when no fractional digits remain.
   VALUE-IS-NONZERO-P enables the leading-\"0\" drop when the field
   would otherwise overflow W."
  (declare (type simple-string rounded))
  (let* ((len           (length rounded))
	 (dot           (position #\. rounded))
	 (int-end       (or dot len))
	 (frac-start    (if dot (1+ dot) len))
	 (frac-len      (- len frac-start))
	 (frac-out-len  (cond ((plusp frac-len) frac-len)
			      (force-trailing-zero-p 1)
			      (t 0)))
	 (sign-len      (if (or is-negative-p at-sign-p) 1 0))
	 ;; CLHS 22.3.3.1: the leading "0" before the dot is optional
	 ;; when the magnitude is < 1 (integer part is exactly "0")
	 ;; and the original value is nonzero.  Require something to be
	 ;; written after the dot so we never drop down to just ".".
	 (leading-zero-droppable
	   (and (= int-end 1)
		(char= (char rounded 0) #\0)
		value-is-nonzero-p
		(plusp frac-out-len)))
	 (full-len  (+ sign-len int-end 1 frac-out-len))
	 (drop-leading-zero-p
	   (and leading-zero-droppable w (> full-len w)))
	 (field-len (if drop-leading-zero-p (1- full-len) full-len)))
    (flet ((write-field ()
	     (cond (is-negative-p (write-char #\- stream))
		   (at-sign-p     (write-char #\+ stream)))
	     (unless drop-leading-zero-p
	       (write-string rounded stream :start 0 :end int-end))
	     (write-char #\. stream)
	     (cond ((plusp frac-len)
		    (write-string rounded stream :start frac-start :end len))
		   (force-trailing-zero-p
		    (write-char #\0 stream)))))
      (declare (dynamic-extent #'write-field))
      (pad-overflow stream field-len w overflowchar padchar #'write-field))))

(defun format-f-fixed (stream value w d
                       overflowchar padchar at-sign-p)
  "Write the ~F representation of VALUE rounded to exactly D digits
   after the decimal point, right-justified in a field of width W.
   This is the explicit-D path: no \"single zero\" rule applies, so
   D=0 yields \"d.\" with nothing after the dot.  The optional
   leading zero before the decimal point is dropped when keeping it
   would overflow W (CLHS 22.3.3.1; FORMAT.F.13)."
  (declare (type (or single-float double-float) value)
	   (type (integer 0 *) d))
  (multiple-value-bind (is-negative-p abs-value)
      (get-sign-and-absolute-value value)
    (emit-fixed stream (d2fixed abs-value d) is-negative-p w
		overflowchar padchar at-sign-p nil
		(plusp abs-value))))

(defun reshape-fixed-length (digit-count exponent)
  ;; DIGIT-COUNT is the number of digits returned by d2s/f2s.  That no
  ;; more than 20.  EXPONENT is teh power of 10 for a double-float.  A
  ;; signed-byte 10 is large enough to hold this.
  ;;
  ;; We don't need to be super-precise here; we just need to be close
  ;; enough so as not to use bignums.
  (declare (type (integer 1 20) digit-count)
	   (type (signed-byte 10) exponent))
  (let ((k (1+ exponent)))
    (cond ((zerop exponent)
	   ;; Fast path
	   (if (= digit-count 1)
	       3
	       (1+ digit-count)))
	  ((<= k 0)
	   ;; "0." + |k| zeros + digits
	   (+ 2 (- k) digit-count))
	  ((< k digit-count)
	   ;; "ddd.ddd"; add one for the dot.
	   (1+ digit-count))
	  (t
	   ;; "ddddd.0"; k digits + ".0"
	   (+ k 2)))))

(defun emit-shortest (stream mantissa exponent is-negative-p w
                      overflowchar padchar at-sign-p)
  (declare (type simple-string mantissa)
	   (type (signed-byte 10) exponent))
  (let* (;; Number of digits, not including the decimal point.
	 (digit-count (mantissa-digit-count mantissa))
         (sign-len  (if (or is-negative-p at-sign-p) 1 0))
	 (reshaped-len (reshape-fixed-length digit-count exponent))
         (field-len (+ sign-len reshaped-len)))
    (flet ((write-field ()
             (cond (is-negative-p
		    (write-char #\- stream))
                   (at-sign-p
		    (write-char #\+ stream)))
             (scale-mantissa stream mantissa (1+ exponent) nil)))
      (declare (dynamic-extent #'write-field))
      (pad-overflow stream field-len w overflowchar padchar #'write-field))))
	 
(defun format-f-free (stream value w
                      overflowchar padchar at-sign-p)
  (declare (type (or single-float double-float) value))
  (multiple-value-bind (is-negative-p abs-value)
      (get-sign-and-absolute-value value)
    (multiple-value-bind (mantissa exponent)
	(parsed-exp-form (float-to-string value))
      (let* ((digit-count (mantissa-digit-count mantissa))
             (int-len     (max 1 (1+ exponent)))
             (shortest-d  (max 0 (- digit-count 1 exponent)))
             (sign-len    (if (or is-negative-p at-sign-p) 1 0))
             (d-fit       (and w (- w sign-len int-len 1))))
	(cond
          ((or (null w) (>= d-fit shortest-d))
	   ;; No width, or the shortest round-trip form already fits
	   ;; within a field width of W.  Emit it directly.
           (emit-shortest stream mantissa exponent is-negative-p w
                          overflowchar padchar at-sign-p))
	  ((and overflowchar (minusp d-fit))
	   ;; Not even the integer part and decimal point fit, and an
	   ;; overflow character was supplied: fill the field.
	   (loop repeat w
		 do (write-char overflowchar stream)))
	  (t
	   ;; The shortest form does not fit in W.  Round to the largest
	   ;; number of fractional digits D-FIT that fits and emit via
	   ;; EMIT-FIXED, forcing a trailing "0" if the fraction rounds
	   ;; away (CLHS 22.3.3.1; FORMAT.F.46 may drop the leading "0"
	   ;; before the dot if keeping it would still overflow W).
	   (emit-fixed stream
		       (d2fixed abs-value (max 0 d-fit))
		       is-negative-p w overflowchar padchar at-sign-p
		       t (plusp abs-value))))))))

(defun format-f (stream value w d k overflowchar padchar at-sign-p)
  (declare (type (or single-float double-float) value)
	   (fixnum k)
	   (type (or null (and unsigned-byte fixnum)) w d))
  (cond ((not (zerop k))
	 ;;  Complex case that doesn't fit with what d2s and d2fixed
	 ;;  returns.  Especially when d+k is negative so that some
	 ;;  digits are shifted right past the desired precision.
	 ;;  We'd have to round the result.  Just use our existing
	 ;;  code to handle this case with the correct rounding.
	 (format::format-fixed-aux stream value w d k overflowchar padchar at-sign-p))
	(d
	 (format-f-fixed stream value w d overflowchar padchar at-sign-p))
	(t
	 ;; No d, so use d2s to get the shortest digits; convert by
	 ;; placing the decimal poin at the right spot.
	 (format-f-free stream value w overflowchar padchar at-sign-p)))))

;;; Ryu ~G
(defun format-g (stream value w d e k overflowchar padchar exponentchar at-sign-p)
  (declare (type (or single-float double-float) value)
	   (fixnum k)
	   (type (or null (and unsigned-byte fixnum)) w d e))
  (multiple-value-bind (mantissa exponent)
      (parsed-exp-form (float-to-string value))
    (let* ((digit-count (mantissa-digit-count mantissa))
	   (n (1+ exponent))
	   (effective-d (or d (max digit-count (min n 7))))
	   (ee (if e (+ e 2) 4))
	   (ww (cond ((null w)
		      nil)
		     ((minusp (- w ee))
		      nil)
		     (t
		      (- w ee))))
	   (dd (- effective-d n)))
      (cond
	((<= 0 dd effective-d)
	 (format-f stream value ww dd 0 overflowchar padchar at-sign-p)
	 (loop for c from 0 below ee
	       do (write-char #\space stream)))
	(t
	 ;; ~E form.  CLHS 22.3.3.3 specifies the resulting call as
	 ;; "~w,d,e,k,...,E" -- using the user's original D, not the
	 ;; EFFECTIVE-D computed for the ~F-vs-~E decision.  When D is
	 ;; NIL this lets format-e produce its free-format (shortest)
	 ;; output rather than fixed-precision.
	 (format-e stream value w d e k overflowchar padchar exponentchar at-sign-p))))))

