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
  "Lisp interface to Ryu d2fixed routine (specically d2fixed_buffered)"
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
  "Lisp interface to Ryu d2exp (specifically d2exp-buffered)."
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
  "Lisp interface to Ryu d2s (specifically d2s_buffered"
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

(defun parsed-d2exp (pos-x digits)
  (let* ((raw (d2exp pos-x digits))
	 ;; Parse the result from d2exp.  It has the form "d.dddEeee".
	 (e-pos (position #\e raw))
	 (mantissa (subseq raw 0 e-pos))
	 (exp (parse-integer raw :start (1+ e-pos))))
    (values mantissa exp)))

(defun parsed-exp-form (raw-string)
  ;; Parse RAW-STRING, that is in exponential form.  That is, it must
  ;; the form "d.dddEeee".  There cannot be a leading sign.
  (let* ((e-pos (position-if #'(lambda (c)
				 (member c '(#\e #\E)))
			     raw-string))
	 (mantissa (subseq raw-string 0 e-pos))
	 (exp (parse-integer raw-string :start (1+ e-pos))))
    ;; The exponent from a double-float is in the range of [-324,
    ;; 308].  Just declare the exponent as a (signed-byte 11).
    (values mantissa
	    (truly-the (signed-byte 11) exp))))

(defun compute-d-for-width (w e k actual-exp is-negative-p at-sign-p)
  ;; Find the largest d (precision) value that fits in width W given
  ;; the actual exponent (adjusted by k), and whether signs are
  ;; printed or not.
  ;;
  ;; Returns NIL if no width d can fit in a field of length w.
  (declare (fixnum k actual-exp))
  (let* ((sign-len (if (or is-negative-p at-sign-p) 1 0))
	 (exp-digits (max (or e 1)
			  (length (princ-to-string (abs actual-exp)))))
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

(defun count-decimal-digits (n)
  "Number of decimal digits in N.  N is the absolute value of a
   double-float's exponent, so 0 <= n <= 324."
  (declare (type (integer 0 324) n)
	   (optimize speed))
  (cond ((< n 10)  1)
        ((< n 100) 2)
        (t         3)))

(defun compute-exp-output-length (mantissa actual-exp k e is-negative-p at-sign-p
				  drop-leading-zero-p)
  (declare (type simple-string mantissa)
	   (fixnum actual-exp k)
	   (type (or null fixnum) e))
  ;; Compute length of the ~E result with the given parameters, but
  ;; don't build the string.  MANTISSA is "d[.dddd]" from d2exp or d2s.
  ;; ACTUAL-EXP is the exponent with scaling factor applied.  If
  ;; DROP-LEADING-ZERO-P is non-NIL, the leading "0" before the dot
  ;; (in the K <= 0 form) is omitted from the length.
  (let* ((sign-len (if (or is-negative-p at-sign-p) 1 0))
         (exp-digits (max (or e 1)
                          (count-decimal-digits (abs actual-exp))))
         (dotp (find #\. mantissa))
         (raw-digits (let ((len (length mantissa)))
                       (if dotp (1- len) len)))
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
  ;; Compute precision for d2exp when CL requests D digits and the
  ;; scale factor is K."
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
		  
(defun format-e-string (stream mantissa exponent is-negative-p w e k
                        overflowchar padchar exponentchar at-sign-p
                        drop-leading-zero-p)
  (declare (type simple-string mantissa)
           (fixnum exponent k))
  (let* ((shown-exp (- exponent (1- k)))
         (exp-sign (if (minusp shown-exp)
		       #\- #\+))
         (exp-abs (abs shown-exp))
         (exp-marker (or exponentchar #\d))
         (exp-digits (count-decimal-digits exp-abs))
         (exp-width (max (or e 1)
			 exp-digits))
         ;; Full output length: sign + reshaped + marker + exp-sign +
         ;; exp-width.  compute-exp-output-length gives reshape + exp
         ;; body (marker + exp-sign + max(e, exp-digits)), but it also
         ;; includes sign, so use it directly.  Wait, it doesn't
         ;; account for drop-leading-zero, so we use it carefully.
         (field-len (compute-exp-output-length mantissa shown-exp k e
                                               is-negative-p at-sign-p
                                               drop-leading-zero-p)))
    (flet ((write-field ()
             (cond (is-negative-p
		    (write-char #\- stream))
                   (at-sign-p
		    (write-char #\+ stream)))
             (scale-mantissa stream mantissa k drop-leading-zero-p)
             (write-char exp-marker stream)
             (write-char exp-sign stream)
             (loop repeat (- exp-width exp-digits)
		   do (write-char #\0 stream))
             (princ exp-abs stream)))
      (declare (dynamic-extent #'write-field))
      (pad-overflow stream field-len w overflowchar padchar #'write-field))))

(defun format-e (value w d e k overflowchar padchar exponentchar at-sign-p)
  (declare (type (or single-float double-float) value)
           (fixnum k)
           (type (or null (and unsigned-byte fixnum)) w d e)
           (optimize (speed 3)))
  (multiple-value-bind (is-negative-p abs-value)
      (get-sign-and-absolute-value value)
    (with-output-to-string (stream)
      (cond
        (d
         (multiple-value-bind (mantissa exponent)
             (parsed-exp-form (d2exp abs-value
				     (d2exp-precision d k)))
           (format-e-string stream mantissa exponent is-negative-p
                            w e k overflowchar padchar exponentchar at-sign-p nil)))
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
                  (if d-fit
                      (multiple-value-bind (mantissa exponent)
                          (parsed-exp-form (d2exp abs-value
						  (d2exp-precision d-fit k)))
                        (format-e-string stream mantissa exponent is-negative-p
                                         w e k overflowchar padchar exponentchar at-sign-p
                                         drop-zero-p))
                      (format-e-string stream mantissa exponent is-negative-p
                                       w e k overflowchar padchar exponentchar at-sign-p
                                       nil))))))))))))

;;; Ryu ~F
(defun format-f-fixed (stream value w d
                       overflowchar padchar at-sign-p)
  (declare (type (or single-float double-float) value))
  (multiple-value-bind (is-negative-p abs-value)
      (get-sign-and-absolute-value value)
    (let* ((raw-string (d2fixed abs-value d))
           (raw-len    (length raw-string))
           (sign-len   (if (or is-negative-p at-sign-p) 1 0))
           (need-dot   (zerop d))
	   (full-field-len (+ sign-len raw-len (if need-dot 1 0)))
	   ;; CLHS 22.3.3.1: the leading zero before the decimal point
	   ;; is optional when the magnitude is nonzero and less than 1.
	   ;; d2fixed always emits it (e.g. "0.50" for 0.5), so detect
	   ;; that case here and drop the zero if the field would not
	   ;; otherwise fit in W.  For exact zero the leading digit is
	   ;; required, so PLUSP ABS-VALUE gates the dropping.
	   (lpoint-droppable
	     (and (plusp abs-value)
		  (>= raw-len 2)
		  (char= (char raw-string 0) #\0)
		  (char= (char raw-string 1) #\.)))
	   (drop-leading-zero-p
	     (and lpoint-droppable
		  w
		  (> full-field-len w)))
           (field-len (if drop-leading-zero-p
			  (1- full-field-len)
			  full-field-len)))
      (flet ((write-field ()
               (cond (is-negative-p (write-char #\- stream))
                     (at-sign-p     (write-char #\+ stream)))
	       (cond (drop-leading-zero-p
		      (write-string raw-string stream :start 1))
		     (t
		      (write-string raw-string stream)))
               (when need-dot (write-char #\. stream))))
	(declare (dynamic-extent #'write-field))
	(pad-overflow stream field-len w overflowchar padchar #'write-field)))))

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
  (let* ((has-dot (find #\. mantissa))
	 ;; Number of digits, not including the decimal point.
	 (digit-count (- (length mantissa)
			 (if has-dot 1 0)))
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
    (declare (ignore abs-value))
    (multiple-value-bind (mantissa exponent)
	(parsed-exp-form (float-to-string value))
      (let* ((has-dot     (find #\. mantissa))
             (digit-count (if has-dot (1- (length mantissa)) (length mantissa)))
             (int-len     (max 1 (1+ exponent)))
             (shortest-d  (max 0 (- digit-count 1 exponent)))
             (sign-len    (if (or is-negative-p at-sign-p) 1 0))
             (d-fit       (and w (- w sign-len int-len 1))))
	(cond
          ((or (null w) (>= d-fit shortest-d))
	   ;; No width or the shortest form fits within a field width
	   ;; of W.
           (emit-shortest stream mantissa exponent is-negative-p w
                          overflowchar padchar at-sign-p))
	  (overflowchar
	   ;; Shortest form does not fit and OVERFLOWCHAR is set; fill
	   ;; the field with overflow characters.
	   (loop repeat w
		 do (write-char overflowchar stream)))
	  (t
	   ;; Shortest form does not fit and no OVERFLOWCHAR; emit the
	   ;; full shortest form, letting the field expand.  CLHS
	   ;; 22.3.3.1 requires this.
	   (emit-shortest stream mantissa exponent is-negative-p w
			  overflowchar padchar at-sign-p)))))))

(defun format-f (value w d k overflowchar padchar at-sign-p)
  (declare (type (or single-float double-float) value)
	   (fixnum k)
	   (type (or null (and unsigned-byte fixnum)) w d))
  (with-output-to-string (s)
    (cond ((not (zerop k))
	   ;;  Complex case that doesn't fit with what d2s and d2fixed
	   ;;  returns.  Especially when d+k is negative so that some
	   ;;  digits are shifted right past the desired precision.
	   ;;  We'd have to round the result.  Just use our existing
	   ;;  code to handle this case with the correct rounding.
	   (format::format-fixed-aux s value w d k overflowchar padchar at-sign-p))
	  (d
	   (format-f-fixed s value w d overflowchar padchar at-sign-p))
	  (t
	   ;; No d, so use d2s to get the shortest digits; convert by
	   ;; placing the decimal poin at the right spot.
	   (format-f-free s value  w overflowchar padchar at-sign-p)))))

;;; Ryu ~G
(defun format-g (value w d e k overflowchar padchar exponentchar at-sign-p)
  (declare (type (or single-float double-float) value)
	   (fixnum k)
	   (type (or null (and unsigned-byte fixnum)) w d e))
  (multiple-value-bind (mantissa exponent)
      (parsed-exp-form (float-to-string value))
    (let* ((digit-count (- (length mantissa)
			   (if (find #\. mantissa)
			       1 0)))
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
	 (concatenate 'string
		      (format-f value ww dd 0 overflowchar padchar at-sign-p)
		      (make-string ee :initial-element #\space)))
	(t
	 ;; ~E form.  CLHS 22.3.3.3 specifies the resulting call as
	 ;; "~w,d,e,k,...,E" -- using the user's original D, not the
	 ;; EFFECTIVE-D computed for the ~F-vs-~E decision.  When D is
	 ;; NIL this lets format-e produce its free-format (shortest)
	 ;; output rather than fixed-precision.
	 (format-e value w d e k overflowchar padchar exponentchar at-sign-p))))))

