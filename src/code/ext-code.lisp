;;; -*- Log: code.log; Package: Extensions -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/ext-code.lisp $")
;;;
;;;
;;; **********************************************************************
;;;
;;; Spice Lisp extensions to the language.
;;;
;;; These extensions are compiled natively instead of byte-compiled
;;; like the code in code/extensions.lisp.
;;;
;;; **********************************************************************
(in-package "EXTENSIONS")

(intl:textdomain "cmucl")


;;;; **********************************************************************
;;;; C-style hex float printer and parser

;;; **********************************************************************
;;; Write float in C-style hex float literal

(defun trim-trailing-zeros (s)
  "Remove trailing zero characters from string S, preserving internal zeros."
  (let ((last-nonzero (position #\0 s :test #'char/= :from-end t)))
    (if last-nonzero
        (subseq s 0 (1+ last-nonzero))
        "")))

(defun write-hex-float-double (x stream)
  "Print a single-float or double-float in hex format onto STREAM."
  ;; Float type and mantissa width are derived from the type of X.
  (multiple-value-bind (mantissa-bits suffix-char min-c-exp)
      (etypecase x
        (single-float (values 23 #\f -126))
        (double-float (values 52 nil -1022)))
    (when (and (not (float-nan-p x)) (minusp (float-sign x)))
      (write-char #\- stream))
    (let ((x (abs x)))
      (cond
        ((float-nan-p x)
         (write-string "0x0.0p+nan" stream)
         (when suffix-char (write-char suffix-char stream)))

        ((float-infinity-p x)
         (write-string "0x1.0p+inf" stream)
         (when suffix-char (write-char suffix-char stream)))

        ((zerop x)
         (write-string "0x0p+0" stream)
         (when suffix-char (write-char suffix-char stream)))

        (t
         (multiple-value-bind (significand exponent sign)
             (integer-decode-float x)
           (declare (ignore sign))
           (let* ((c-exp      (+ exponent mantissa-bits))
                  (denormalp  (< c-exp min-c-exp))
                  (hex-digits (ceiling mantissa-bits 4))
                  (frac-shift (- (* 4 hex-digits) mantissa-bits))
                  (frac       (if denormalp
                                  (ash significand
                                       (+ (- c-exp min-c-exp) frac-shift))
                                  (ash (logand significand
                                               (1- (ash 1 mantissa-bits)))
                                       frac-shift)))
                  (out-exp    (if denormalp min-c-exp c-exp))
                  (frac-str   (trim-trailing-zeros
                               (format nil "~v,'0X" hex-digits frac))))
             (write-string "0x" stream)
             (write-char (if denormalp #\0 #\1) stream)
             (unless (zerop (length frac-str))
               (write-char #\. stream)
               (write-string frac-str stream))
             (write-char #\p stream)
             (when (>= out-exp 0)
	       (write-char #\+ stream))
             (write-string (format nil "~D" out-exp) stream)
             (when suffix-char
	       (write-char suffix-char stream)))))))
    (values)))

#+double-double
(defun write-hex-float-double-double (x stream)
  "Print a double-double-float in hex format onto STREAM."
  ;; Reconstructs the full significand from hi and lo components using
  ;; exact integer arithmetic before formatting."
  (let* ((hi  (kernel:double-double-hi x))
         (lo  (kernel:double-double-lo x))
         (hi  (abs hi)))
    (when (minusp (float-sign (kernel:double-double-hi x)))
      (write-char #\- stream))
    (cond
      ((zerop hi)
       (write-string "0x0p+0w" stream))
      (t
       (multiple-value-bind (sig-hi exp-hi sign-hi)
           (integer-decode-float hi)
         (declare (ignore sign-hi))
         (multiple-value-bind (sig-lo exp-lo sign-lo)
             (integer-decode-float lo)
           (let* ((signed-sig-lo  (* sign-lo sig-lo))
                  (combined-sig   (if (zerop lo)
                                      sig-hi
                                      (+ (ash sig-hi (- exp-hi exp-lo))
                                         signed-sig-lo)))
                  (combined-exp   (if (zerop lo) exp-hi exp-lo))
                  (total-bits     (integer-length combined-sig))
                  (c-exp          (+ combined-exp total-bits -1))
                  (min-c-exp      -1022)
                  (denormalp      (< c-exp min-c-exp))
                  (raw-frac-bits  (if (zerop lo)
                                      52
                                      (+ (- exp-hi exp-lo)
					 52)))
                  (frac-bits      (* 4 (ceiling raw-frac-bits 4)))
                  (hex-digits     (/ frac-bits 4))
                  (shift          (if denormalp
                                      (+ (- frac-bits (1- total-bits))
                                         (- c-exp min-c-exp))
                                      (- frac-bits (1- total-bits))))
                  (frac           (if denormalp
                                      (ash combined-sig shift)
                                      (logand (ash combined-sig shift)
                                              (1- (ash 1 frac-bits)))))
                  (out-exp        (if denormalp min-c-exp c-exp))
                  (frac-str       (trim-trailing-zeros
                                    (format nil "~v,'0X" hex-digits frac))))
             (write-string "0x" stream)
             (write-char (if denormalp #\0 #\1) stream)
             (unless (zerop (length frac-str))
               (write-char #\. stream)
               (write-string frac-str stream))
             (write-char #\p stream)
             (when (>= out-exp 0)
	       (write-char #\+ stream))
             (write-string (format nil "~D" out-exp) stream)
             (write-char #\w stream))))))
  (values)))

;;; WRITE-HEX-FLOAT  -- Public
;;;
;;; Writes a float value (single, double, or double-double) in hex
;;; format to a stream, defaulting to *standard-output*.
(defun write-hex-float (x &optional (stream *standard-output*))
  "Write float X to STREAM in C-style hex format. STREAM defaults to *standard-output*.

   single-float        => 0x<mantissa>p<exp>f
   double-float        => 0x<mantissa>p<exp>
   double-double-float => 0x<mantissa>p<exp>w

  Negative zero is printed with a leading minus sign."
  (let ((*print-case* :downcase))
    (etypecase x
      (single-float
       (write-hex-float-double x stream))
      (double-float
       (write-hex-float-double x stream))
      #+double-double
      (double-double-float
       (write-hex-float-double-double x stream))))
  (values))

;;; FLOAT-TO-HEX-STRING  -- Public
;;;
;;; Return a string representing a single and double-floats in C-style
;;; hex format.
(defun float-to-hex-string (x)
  "Return a string containing the C-style hex float representation of X.
   single-float        => \"0x<mantissa>p<exp>f\"
   double-float        => \"0x<mantissa>p<exp>\"
   double-double-float => \"0x<mantissa>p<exp>w\""
  (with-output-to-string (s)
    (write-hex-float x s)))

;;; FORMAT-HEX-FLOAT -- Public
;;;
;;; Function that can be used in a FORMAT ~/
(defun format-hex-float (stream x colonp atsignp &rest args)
  "Format function for use with ~/package:format-hex-float/.
   Ignores colon modifier.
   At-sign modifier forces a leading + sign on non-negative values.
   Example: (format t \"~@/ext:format-hex-float/\" 3.0d0) => +0x1.8p+1"
  (declare (ignore colonp args))
  (when (and atsignp
             (not (float-nan-p x))
             (not (minusp (float-sign x))))
    (write-char #\+ stream))
  (write-hex-float x stream))

;;; **********************************************************************
;;; Read C-style hex float literal


(define-condition hex-float-parse-error (parse-error)
  ((input    :initarg :input    :reader hex-float-parse-error-input)
   (position :initarg :position :reader hex-float-parse-error-position)
   (message  :initarg :message  :reader hex-float-parse-error-message))
  (:report (lambda (c s)
             (format s "Hex float parse error~@[ at position ~D~]: ~A~@[ (input: ~S)~]"
                     (hex-float-parse-error-position c)
                     (hex-float-parse-error-message c)
                     (hex-float-parse-error-input c)))))

(defun read-hex-float-from-stream (stream)
  "Read a C-style hex float from STREAM and return a float value.
   Format: [sign] 0x <hex-mantissa> [. <hex-fraction>] p <exp> [f|w]
   'f' suffix => single-float
   'w' suffix => double-double-float (CMUCL native)
   no suffix   => double-float
   The binary exponent (p or P) is required.
   Signals HEX-FLOAT-PARSE-ERROR on malformed input."
  (flet ((parse-error (pos msg &rest args)
           (error 'ext:hex-float-parse-error
                  :position pos
                  :message  (apply #'format nil msg args)
                  :input    nil))
         (pos ()
           (file-position stream)))

    (let ((sign 1)
          (mantissa 0)
          (frac-digits '())
          (exponent 0)
          (suffix nil))

      ;; Optional sign
      (let ((c (peek-char nil stream nil nil)))
        (cond ((null c)
               (parse-error (pos) "Unexpected end of input, expected hex float"))
              ((char= c #\-) (setf sign -1) (read-char stream))
              ((char= c #\+) (read-char stream))))

      ;; Expect "0"
      (let ((c (read-char stream nil nil)))
        (unless (and c (char= c #\0))
          (parse-error (pos) "Expected '0' to begin hex float prefix '0x', got ~S" c)))

      ;; Expect "x" or "X"
      (let ((c (read-char stream nil nil)))
        (unless (and c (member c '(#\x #\X)))
          (parse-error (pos) "Expected 'x' or 'X' after '0', got ~S" c)))

      ;; Hex digits before optional decimal point (mantissa)
      (let ((mantissa-start (pos)))
        (loop for c = (peek-char nil stream nil nil)
              while (and c (digit-char-p c 16))
              do (setf mantissa (+ (* mantissa 16)
                                   (digit-char-p (read-char stream) 16))))
        (let ((mantissa-digitsp (> (pos) mantissa-start)))

          ;; Optional fractional part — collect raw hex digits
          (when (and (peek-char nil stream nil nil)
                     (char= (peek-char nil stream nil nil) #\.))
            (read-char stream)
            (let ((frac-start (pos)))
              (loop for c = (peek-char nil stream nil nil)
                    while (and c (digit-char-p c 16))
                    do (push (digit-char-p (read-char stream) 16) frac-digits))
              (when (and (peek-char nil stream nil nil)
                         (not (digit-char-p (peek-char nil stream nil nil) 16))
                         (not (member (peek-char nil stream nil nil) '(#\p #\P)))
                         (zerop (length frac-digits)))
                (parse-error frac-start "Expected hex digits after decimal point")))
            (setf frac-digits (nreverse frac-digits)))

          ;; Mantissa must have at least one hex digit total
          (unless (or mantissa-digitsp frac-digits)
            (parse-error (pos) "Expected at least one hex digit in mantissa"))))

      ;; Required binary exponent (p or P)
      (let ((c (read-char stream nil nil)))
        (unless (and c (member c '(#\p #\P)))
          (parse-error (pos) "Expected 'p' or 'P' exponent marker, got ~S" c)))

      (let ((exp-sign 1))
        (when (member (peek-char nil stream nil nil) '(#\+ #\-))
          (when (char= (read-char stream) #\-) (setf exp-sign -1)))
        (let ((exp-digits nil)
              (exp-start (pos)))
          (loop for c = (peek-char nil stream nil nil)
                while (and c (digit-char-p c 10))
                do (push (digit-char-p (read-char stream) 10) exp-digits))
          (unless exp-digits
            (parse-error exp-start "Expected decimal digits for exponent"))
          (setf exponent (* exp-sign
                            (reduce (lambda (acc d) (+ (* acc 10) d))
                                    (nreverse exp-digits))))))

      ;; Optional suffix: 'f'/'F' => single, 'w'/'W' => double-double, none => double
      (when (peek-char nil stream nil nil)
        (let ((c (peek-char nil stream nil nil)))
          (cond ((member c '(#\f #\F)) (read-char stream) (setf suffix :single))
                ((member c '(#\w #\W)) (read-char stream) (setf suffix :double-double))
                ((not (or (member c '(#\space #\tab #\newline #\return
                                     #\) #\] #\} #\,))
                          (digit-char-p c 10)))
                 (parse-error (pos) "Unexpected character ~S after exponent" c)))))

      ;; Build exact integer significand, adjust exponent for fractional hex digits.
      ;; Convert significand to float before multiplying by sign so that
      ;; (* -1 0.0d0) = -0.0d0, preserving negative zero.
      (let* ((n-frac       (length frac-digits))
             (significand  (reduce (lambda (acc d) (+ (* acc 16) d))
                                   frac-digits
                                   :initial-value mantissa))
             (adjusted-exp (- exponent (* 4 n-frac))))

        (ecase suffix
          ((nil)
           (scale-float (* sign (float significand 1.0d0)) adjusted-exp))

          (:single
           (coerce (scale-float (* sign (float significand 1.0d0)) adjusted-exp)
                   'single-float))

          (:double-double
           (let* ((sig-bits    (integer-length significand))
                  (split-shift (max 0 (- sig-bits 53)))
                  (sig-hi      (ash significand (- split-shift)))
                  (sig-lo      (- significand (ash sig-hi split-shift)))
                  (exp-hi      (+ adjusted-exp split-shift))
                  (hi          (scale-float (* sign (float sig-hi 1.0d0)) exp-hi))
                  (lo          (scale-float (* sign (float sig-lo 1.0d0)) adjusted-exp)))
             (kernel:make-double-double-float hi lo))))))))

(defun read-hex-float-from-string (s &key (start 0) end)
  "Read a C-style hex float from string S.
   START and END bound the region to read (default: entire string).
   Returns two values: the float and the index of the first character
   not consumed.
   Signals HEX-FLOAT-PARSE-ERROR on malformed input."
  (let ((end (or end (length s))))
    (with-input-from-string (stream s :start start :end end)
      (values (read-hex-float stream)
              (file-position stream)))))

;;; READ-HEX-FLOAT -- Public
;;;
;;; Read a C-style hex float number from either a string or a stream.
(defun read-hex-float (obj)
  "Read a C-style hex float number from OBJ which is either a string or a stream."
  (declare (type (or string stream) obj))
  (etypecase obj
    (string
     (read-hex-float-from-string obj))
    (stream
     (read-hex-float-from-stream obj))))
