;;; -*- Log: code.log; Package: Extensions -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/extensions.lisp $")
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


;;; C-style hex float printer and parser
(defun print-hex-single-float (val)
  "Prints a single-float in C-style hex format."
  (cond ((float-nan-p val)
	 "nan")
        ((float-infinity-p val)
	 (if (plusp val) "inf" "-inf"))
        ((zerop val)
	 (if (eql val -0.0f0)
	     "-0x0.0p+0f" "0x0.0p+0f"))
        (t
         (let* ((bits (ldb (byte 32 0) (kernel:single-float-bits val)))
                (sign (ldb (byte 1 31) bits))
                (exp-bits (ldb (byte 8 23) bits))
                (mantissa (ldb (byte 23 0) bits))
		;; Print lower-case hex digits.
		(*print-case* :downcase))
           (if (zerop exp-bits)
               ;; Subnormal: Leading digit 0, exponent fixed at -126
               (format nil "~A0x0.~6,'0Xp-126f"
                       (if (= sign 1) "-" "")
                       (ash mantissa 1)) ; Align 23 bits to 24 bits (6 hex digits)
               ;; Normalized: Leading digit 1, exponent bias 127
               (format nil "~A0x1.~6,'0Xp~Af"
                       (if (= sign 1) "-" "")
                       (ash mantissa 1) ; Align 23 bits to 24 bits (6 hex digits)
                       (- exp-bits 127)))))))

(defun print-hex-double-float (val)
  "Prints a double-float in C-style hex format."
  (cond ((float-nan-p val)
	 "nan")
        ((float-infinity-p val)
	 (if (plusp val) "inf" "-inf"))
        ((zerop val)
	 (if (eql val -0.0d0)
	     "-0x0.0p+0" "0x0.0p+0"))
        (t
         (multiple-value-bind (hi-bits lo-bits)
	     (kernel:double-float-bits val)
           (let* ((hi (ldb (byte 32 0) hi-bits))
                  (lo (ldb (byte 32 0) lo-bits))
                  (sign (ldb (byte 1 31) hi))
                  (exp-bits (ldb (byte 11 20) hi))
                  ;; Combine 20 bits from high word and 32 bits from low word
                  (mantissa (logior (ash (ldb (byte 20 0) hi) 32)
				    lo))
		  ;; Print lower-case hex digits.
		  (*print-case* :downcase))
             (if (zerop exp-bits)
                 ;; Subnormal: Leading digit 0, exponent fixed at -1022
                 (format nil "~A0x0.~13,'0Xp-1022"
                         (if (= sign 1) "-" "")
                         mantissa)
                 ;; Normalized: Leading digit 1, exponent bias 1023
                 (format nil "~A0x1.~13,'0Xp~A"
                         (if (= sign 1) "-" "")
                         mantissa ; 52 bits fits 13 hex digits perfectly
                         (- exp-bits 1023))))))))

;;; PRINT-HEX-FLOAT  -- Public
;;;
;;; Return a string representing a single and double-floats in C-style
;;; hex format.
(defun print-hex-float (float)
  "Convert FLOAT to C-style hex string.  Infinities are printed as \"-inf\"
  and \"inf\".  NaN is printed as \"nan\"."
  (declare (float float))
  (etypecase float
    (single-float (print-hex-single-float float))
    (double-float (print-hex-double-float float))))

;;; FORMAT-HEX-FLOAT -- Public
;;;
;;; Function that can be used in a FORMAT ~/
(defun format-hex-float (stream arg colon-p at-sign-p &optional width)
  "Formatter for ~/ext:format-hex-float/. 
   @ forces sign (+/-). Colon modifier is ignored as per request."
  (declare (ignore width colon-p))
  (let ((str (if (typep arg 'single-float) 
                 (print-hex-single-float arg)
                 (print-hex-double-float arg))))
    ;; Prepend '+' if @ is used and number isn't negative or special
    (when (and at-sign-p 
               (not (ext:float-nan-p arg))
               (not (ext:float-infinity-p arg))
               (not (char= (char str 0) #\-)))
      (write-char #\+ stream))
    (write-string str stream)))

;;; PARSE-HEX-FLOAT -- Public
;;;
;;; Parse a C-style float hex strings.  Always returns a double-float.
;;; Error-checking is enabled for malformed strings.
(define-condition hex-parse-error (parse-error)
  ((text :initarg :text :reader hex-parse-error-text)
   (message :initarg :message :reader hex-parse-error-message))
  (:report (lambda (c s)
             (format s "Hex float parse error in ~S: ~A" 
                     (hex-parse-error-text c) (hex-parse-error-message c)))))

#+nil
(defun parse-hex-float (str)
  "Parses hex floats using scale-float for the exponent. Strictly hex-literal only."
  (let* ((str (string-trim '(#\Space #\Tab #\Newline #\Return)
			   (string-downcase str)))
         (len (length str)))
    (when (zerop len)
      (error 'hex-parse-error :text str :message "Empty string"))
    
    (let* ((ends-with-f (and (> len 1)
			     (char= (char str (1- len)) #\f)))
           (effective-len (if ends-with-f
			      (1- len) len))
           (prototype (if ends-with-f
			  1.0f0 1.0d0))
           (has-sign (or (char= (char str 0) #\-)
			 (char= (char str 0) #\+)))
           (sign (if (and has-sign
			  (char= (char str 0) #\-))
		     -1 1))
           (start (if has-sign 1 0)))
      
      (unless (and (<= (+ start 2) effective-len) 
                   (string= str "0x" :start1 start :end1 (+ start 2)))
        (error 'hex-parse-error :text str :message "Missing '0x' prefix"))
      
      (let ((p-pos (position #\p str :start start :end effective-len)))
        (unless p-pos
	  (error 'hex-parse-error :text str :message "Missing exponent 'p'"))

        (let* ((sig-start (+ start 2))
               (dot-pos (position #\. str :start sig-start :end p-pos))
               (exp-start (1+ p-pos))
               ;; Leading hex: digits before the dot
               (leading-str (subseq str sig-start (or dot-pos p-pos)))
               ;; Trailing hex: digits after the dot
               (trailing-str (if dot-pos (subseq str (1+ dot-pos) p-pos) ""))
               (has-digits (or (plusp (length leading-str))
			       (plusp (length trailing-str)))))
          
          (unless has-digits
            (error 'hex-parse-error :text str :message "No hex digits in significand"))

          (handler-case
              (let* ((leading-int (if (string= leading-str "") 0 
                                      (parse-integer leading-str :radix 16)))
                     (trailing-len (length trailing-str))
                     (trailing-int (if (string= trailing-str "") 0 
                                       (parse-integer trailing-str :radix 16)))
                     ;; Calculate the significand as a float: leading + (trailing / 16^len)
                     (significand (float (+ leading-int 
                                            (/ trailing-int (expt 16 trailing-len)))
                                         prototype))
                     ;; The exponent after 'p'
                     (raw-exponent (parse-integer str :start exp-start :end effective-len)))
                ;; Use scale-float to apply the binary exponent efficiently
                (* sign (scale-float significand raw-exponent)))
            (error (c)
	      (error 'hex-parse-error :text str :message (format nil "~A" c)))))))))

(defun parse-hex-float-from-stream (stream)
  "Reads hex float from stream using double-float accumulation and a 6-character exponent buffer."
  (let* ((sign 1.0d0)
         (char (peek-char t stream))) ; Skip whitespace
    
    ;; 1. Handle Sign
    (when (member char '(#\+ #\-))
      (when (char= (read-char stream) #\-) (setf sign -1.0d0))
      (setf char (peek-char nil stream)))

    ;; 2. Verify '0x' Prefix
    (unless (and (char-equal (read-char stream) #\0)
                 (char-equal (read-char stream) #\x))
      (error 'hex-parse-error :text "Stream" :message "Missing '0x' prefix"))

    ;; 3. Read Significand
    (let ((val 0.0d0)
          (digits-read 0))
      ;; Integer part loop
      (loop for c = (peek-char nil stream nil nil)
            for digit = (and c (digit-char-p c 16))
            while digit
            do (read-char stream)
               (setf val (+ (* val 16.0d0) digit))
               (incf digits-read))
      
      ;; Fractional part loop
      (when (eql (peek-char nil stream nil nil) #\.)
        (read-char stream) ; Consume #\.
        (loop with weight = (/ 1.0d0 16.0d0)
              for c = (peek-char nil stream nil nil)
              for digit = (and c (digit-char-p c 16))
              while digit
              do (read-char stream)
                 (setf val (+ val (* digit weight)))
                 (setf weight (/ weight 16.0d0))
                 (incf digits-read)))

      (unless (plusp digits-read)
        (error 'hex-parse-error :text "Stream" :message "No hex digits in significand"))

      ;; 4. Handle Exponent 'p'
      (let ((p-char (read-char stream nil)))
        (unless (and p-char (char-equal p-char #\p))
          (error 'hex-parse-error :text "Stream" :message "Missing exponent 'p'"))
        
        ;; Size 6 handles sign + 3-4 digits + buffer
        (let ((exp-str (make-array 6 :element-type 'character 
                                     :fill-pointer 0 
                                     :adjustable t)))
          (loop for c = (peek-char nil stream nil nil)
                while (and c (find c "+-0123456789"))
                do (vector-push-extend (read-char stream) exp-str))
          
          (when (zerop (length exp-str))
            (error 'hex-parse-error :text "Stream" :message "Invalid or missing exponent"))

          (let* ((raw-exp (parse-integer exp-str))
                 (suffix (peek-char nil stream nil #\Space))
                 (is-single (char-equal suffix #\f))
                 ;; Final Construction
                 (result (* sign (scale-float val raw-exp))))
            
            (when is-single (read-char stream)) ; Consume 'f'
            
            (if is-single 
                (float result 1.0f0) 
                result)))))))

(defun parse-hex-float (str)
  (with-input-from-string (s str)
    (parse-hex-float-from-stream s)))
