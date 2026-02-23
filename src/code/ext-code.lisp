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
(defun print-hex-single-float (val &optional force-sign)
  (let* ((bits (kernel:single-float-bits val))
         (u-bits (ldb (byte 32 0) bits))
         (sign-bit (ldb (byte 1 31) u-bits))
         (biased-exp (ldb (byte 8 23) u-bits))
         (fraction (ldb (byte 23 0) u-bits))
         (sign-str (cond ((= sign-bit 1) "-") (force-sign "+") (t ""))))
    (cond 
      ((= biased-exp 255) (if (zerop fraction) (format nil "~Ainf" sign-str) "nan"))
      ((and (zerop biased-exp) (zerop fraction)) (format nil "~A0x0.000000p+0" sign-str))
      ((zerop biased-exp) (format nil "~A0x0.~6,'0xp-126" sign-str fraction))
      (t (let ((exponent (- biased-exp 127)))
           (format nil "~A0x1.~6,'0xp~:[~;+~]~D" sign-str fraction (not (minusp exponent)) exponent))))))

(defun print-hex-double-float (val &optional force-sign)
  (multiple-value-bind (hi lo) (kernel:double-float-bits val)
    (let* ((u-hi (ldb (byte 32 0) hi))
           (sign-bit (ldb (byte 1 31) u-hi))
           (biased-exp (ldb (byte 11 20) u-hi))
           (fraction (logior (ash (ldb (byte 20 0) u-hi) 32) lo))
           (sign-str (cond ((= sign-bit 1) "-") (force-sign "+") (t ""))))
      (cond 
        ((= biased-exp #x7FF) (if (zerop fraction) (format nil "~Ainf" sign-str) "nan"))
        ((and (zerop biased-exp) (zerop fraction)) (format nil "~A0x0.0000000000000p+0" sign-str))
        ((zerop biased-exp) (format nil "~A0x0.~13,'0xp-1022" sign-str fraction))
        (t (let ((exponent (- biased-exp 1023)))
             (format nil "~A0x1.~13,'0xp~:[~;+~]~D" sign-str fraction (not (minusp exponent)) exponent)))))))

#+nil
(defun print-hex-single-float (val &optional force-sign)
  "Converts a single-float to a C-style hex string (32-bit)."
  (let* ((bits (kernel:single-float-bits val))
         (u-bits (ldb (byte 32 0) bits))
         (sign-bit (ldb (byte 1 31) u-bits))
         (biased-exp (ldb (byte 8 23) u-bits))
         (fraction (ldb (byte 23 0) u-bits))
         (sign-str (cond ((= sign-bit 1)
                          "-")
                         (force-sign
                          "+")
                         (t
                          ""))))
    (cond 
      ((= biased-exp 255)
       (if (zerop fraction)
           (format nil "~Ainf" sign-str)
           "nan"))
      ((and (zerop biased-exp)
            (zerop fraction))
       (format nil "~A0x0.000000p+0" sign-str))
      ((zerop biased-exp)
       (let ((*print-case* :downcase))
	 (format nil "~A0x0.~6,'0xp-126" sign-str fraction)))
      (t
       (let ((*print-case* :downcase)
	     (exponent (- biased-exp 127)))
         (format nil "~A0x1.~6,'0xp~:[~;+~]~D"
                 sign-str fraction (not (minusp exponent)) exponent))))))

#+nil
(defun print-hex-double-float (val &optional force-sign)
  "Converts a double-float to a C-style hex string (64-bit)."
  (multiple-value-bind (hi lo)
      (kernel:double-float-bits val)
    (let* ((u-hi (ldb (byte 32 0) hi))
           (sign-bit (ldb (byte 1 31) u-hi))
           (biased-exp (ldb (byte 11 20) u-hi))
           (fraction (logior (ash (ldb (byte 20 0) u-hi) 32) lo))
           (sign-str (cond ((= sign-bit 1)
                            "-")
                           (force-sign "+")
                           (t ""))))
      (cond 
        ((= biased-exp #x7FF)
         (if (zerop fraction)
             (format nil "~Ainf" sign-str)
             "nan"))
        ((and (zerop biased-exp)
              (zerop fraction))
         (format nil "~A0x0.0000000000000p+0" sign-str))
        ((zerop biased-exp)
	 (let ((*print-case* :downcase))
           (format nil "~A0x0.~13,'0xp-1022" sign-str fraction)))
        (t
         (let ((*print-case* :downcase)
	       (exponent (- biased-exp 1023)))
           (format nil "~A0x1.~13,'0xp~:[~;+~]~D"
                   sign-str fraction (not (minusp exponent)) exponent)))))))

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
(defun format-hex-float (stream val &optional colon-p at-p &rest params)
  "Format ~/ directive supporting @ (sign) modifier for single/double floats."
  (declare (ignore colon-p params))
  (write-string
   (typecase val
     (single-float (print-hex-single-float val at-p))
     (double-float (print-hex-double-float val at-p))
     (t (format nil "~A" val)))
   stream))

;;; PARSE-HEX-FLOAT -- Public
;;;
;;; Parse a C-style float hex strings.  Always returns a double-float.
;;; Error-checking is enabled for malformed strings.
(define-condition hex-parse-error (error)
  ((text :initarg :text :reader hex-parse-error-text)
   (message :initarg :message :reader hex-parse-error-message))
  (:report (lambda (c s)
             (format s "Hex float parse error in ~S: ~A" 
                     (hex-parse-error-text c) (hex-parse-error-message c)))))

#+nil
(defun parse-hex-float (str)
  "Parses hex strings by converting the significand to a float, then scaling."
  (let* ((str (string-trim '(#\Space #\Tab #\Newline #\Return) (string-downcase str)))
         (len (length str)))
    (when (zerop len) (error 'hex-parse-error :text str :message "Empty string"))
    (let* ((has-sign (or (char= (char str 0) #\-) (char= (char str 0) #\+)))
           (sign (if (and has-sign (char= (char str 0) #\-)) -1 1))
           (start (if has-sign 1 0)))
      (cond
        ((string= str "inf" :start1 start) 
         (if (= sign 1) double-float-positive-infinity double-float-negative-infinity))
        ((string= str "nan" :start1 start) :nan)
        (t
         (unless (and (<= (+ start 2) len) (string= str "0x" :start1 start :end1 (+ start 2)))
           (error 'hex-parse-error :text str :message "Missing '0x' prefix"))
         (let ((p-pos (position #\p str :start start)))
           (unless p-pos (error 'hex-parse-error :text str :message "Missing exponent 'p'"))
           
           ;; Check for internal whitespace
           (loop for i from start below len
                 when (member (char str i) '(#\Space #\Tab #\Newline #\Return))
                 do (error 'hex-parse-error :text str :message "Internal whitespace detected"))

           (let* ((sig-start (+ start 2))
                  (dot-pos (position #\. str :start sig-start :end p-pos))
                  (exp-start (1+ p-pos)))
             
             (handler-case
                 (let* ((frac-hex-len (if dot-pos (- p-pos (1+ dot-pos)) 0))
                        ;; 1. Combine leading and trailing into one large integer
                        (significand-int 
                         (if (null dot-pos)
                             (parse-integer str :start sig-start :end p-pos :radix 16)
                             (let ((leading (if (= sig-start dot-pos) 0 
                                                (parse-integer str :start sig-start :end dot-pos :radix 16)))
                                   (trailing (if (= (1+ dot-pos) p-pos) 0
                                                 (parse-integer str :start (1+ dot-pos) :end p-pos :radix 16))))
                               (+ (ash leading (* 4 frac-hex-len)) trailing))))
                        ;; 2. Parse decimal exponent
                        (raw-exponent (parse-integer str :start exp-start :end len))
                        ;; 3. Handle the "cliff" logic for 0x0. vs 0x1.
                        (starts-with-zero (char= (char str sig-start) #\0))
                        (actual-exponent (if (and starts-with-zero (not (zerop significand-int)))
                                             -1022
                                             raw-exponent)))
                   
                   ;; 4. Convert integer to float and scale by (exponent - fractional bits)
                   ;; scale-float is bit-exact for binary scaling.
                   (* sign (scale-float (float significand-int 1.0d0) 
                                        (- actual-exponent (* 4 frac-hex-len)))))
               (error (c) (error 'hex-parse-error :text str :message (format nil "~A" c)))))))))))

(defun parse-hex-float (str)
  "Parses C-style hex strings via an exact rational. Strictly validates digit presence."
  (let* ((str (string-trim '(#\Space #\Tab #\Newline #\Return) (string-downcase str)))
         (len (length str)))
    (when (zerop len) (error 'hex-parse-error :text str :message "Empty string"))
    (let* ((has-sign (or (char= (char str 0) #\-) (char= (char str 0) #\+)))
           (sign (if (and has-sign (char= (char str 0) #\-)) -1 1))
           (start (if has-sign 1 0)))
      (cond
        ((string= str "inf" :start1 start) 
         (if (= sign 1) double-float-positive-infinity double-float-negative-infinity))
        ((string= str "nan" :start1 start) :nan)
        (t
         (unless (and (<= (+ start 2) len) (string= str "0x" :start1 start :end1 (+ start 2)))
           (error 'hex-parse-error :text str :message "Missing '0x' prefix"))
         (let ((p-pos (position #\p str :start start)))
           (unless p-pos (error 'hex-parse-error :text str :message "Missing exponent 'p'"))
           
           (loop for i from start below len
                 when (member (char str i) '(#\Space #\Tab #\Newline #\Return))
                 do (error 'hex-parse-error :text str :message "Internal whitespace detected"))

           (let* ((sig-start (+ start 2))
                  (dot-pos (position #\. str :start sig-start :end p-pos))
                  (exp-start (1+ p-pos))
                  ;; Strict Validation: Ensure there is at least one digit in the significand
                  (has-leading (and (not (eql sig-start dot-pos)) (not (eql sig-start p-pos))))
                  (has-trailing (and dot-pos (not (eql (1+ dot-pos) p-pos)))))
             
             (unless (or has-leading has-trailing)
               (error 'hex-parse-error :text str :message "No hex digits in significand"))
             
             (handler-case
                 (let* ((frac-hex-len (if dot-pos (- p-pos (1+ dot-pos)) 0))
                        (significand-int 
                         (if (null dot-pos)
                             (parse-integer str :start sig-start :end p-pos :radix 16)
                             (let ((leading (if (not has-leading) 0 
                                                (parse-integer str :start sig-start :end dot-pos :radix 16)))
                                   (trailing (if (not has-trailing) 0
                                                 (parse-integer str :start (1+ dot-pos) :end p-pos :radix 16))))
                               (+ (ash leading (* 4 frac-hex-len)) trailing))))
                        (raw-exponent (parse-integer str :start exp-start :end len))
                        ;; significand * 2^(exp - 4*frac_len)
                        (rational-val (* significand-int 
                                         (expt 2 (- raw-exponent (* 4 frac-hex-len))))))
                   (* sign (float rational-val 1.0d0)))
               (error (c) (error 'hex-parse-error :text str :message (format nil "~A" c)))))))))))
