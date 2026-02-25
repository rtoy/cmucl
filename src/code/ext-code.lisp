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


;;;; C-style hex float printer and parser

;;; FLOAT-TO-HEX-STRING  -- Public
;;;
;;; Return a string representing a single and double-floats in C-style
;;; hex format.
(defun float-to-hex-string (val &optional at-p)
  "Prints a single or double float in bit-perfect C-style hex.
   If AT-P is true, prepends '+' for non-negative finite values."
  (cond ((ext:float-nan-p val)
	 "0x0.0p+nan")
        ((ext:float-infinity-p val) 
         (if (plusp val)
	     (if at-p
		 "+0x1.0p+inf" "0x1.0p+inf")
	     "-0x1.0p+inf"))
        (t
         (multiple-value-bind (sign exp-bits mantissa bias precision suffix)
             (typecase val
               (single-float
                (let ((bits (ldb (byte 32 0) (kernel:single-float-bits val))))
                  (values (ldb (byte 1 31) bits)
                          (ldb (byte 8 23) bits)
                          (ash (ldb (byte 23 0) bits) 1) ; Align 23 to 6 hex digits
                          127 6 "f")))
               (double-float
                (multiple-value-bind (hi lo) (kernel:double-float-bits val)
                  (values (ldb (byte 1 31) hi)
                          (ldb (byte 11 20) hi)
                          (logior (ash (ldb (byte 20 0) hi) 32) (ldb (byte 32 0) lo))
                          1023 13 "")))
               (t (error "Unsupported float type: ~S" (type-of val))))
           
           (let ((sign-str (cond ((= sign 1) "-")
                                 (at-p "+")
                                 (t ""))))
             (if (and (zerop exp-bits) (zerop mantissa))
                 (format nil "~A0x0.0p+0~A" sign-str suffix)
                 (format nil "~A0x~A.~V,'0Xp~A~A"
                         sign-str
                         (if (zerop exp-bits) "0" "1")
                         precision
                         mantissa
                         (if (zerop exp-bits) (1+ (- bias)) (- exp-bits bias))
                         suffix)))))))

;;; WRITE-HEX-FLOAT -- Public
;;;
;;; Writes a float number in C-style hex format to the given stream.
(defun write-hex-float (float &optional (stream *standard-output*))
  "Convert FLOAT to C-style hex string and write it to STREAM.
  Infinities are printed as \"-inf\" and \"inf\".  NaN is printed as
  \"nan\"."
  (declare (float float))
  (write-string (float-to-hex-string float)
		stream))

;;; FORMAT-HEX-FLOAT -- Public
;;;
;;; Function that can be used in a FORMAT ~/
(defun format-hex-float (stream arg colon-p at-sign-p &optional width)
  "Formatter for ~/ext:format-hex-float/. 
   Uses AT-SIGN-P (@) to force the sign. COLON-P (:) is currently ignored."
  (declare (ignore width colon-p))
  (write-string (float-to-hex-string arg at-sign-p)
                stream))

(define-condition hex-parse-error (parse-error)
  ((text :initarg :text :reader hex-parse-error-text)
   (message :initarg :message :reader hex-parse-error-message))
  (:report (lambda (c s)
             (format s "Hex float parse error in ~S: ~A" 
                     (hex-parse-error-text c) (hex-parse-error-message c)))))

;;; PARSE-HEX-FLOAT-FROM-STREAM -- Public
;;;
;;; Parse a C-style float hex string from a stream.  Invalid formats
;;; signal an error.  A single-float or double-float may be returned.
(defun parse-hex-float-from-stream (stream)
  "Reads a C-style hex float number from STREAM.  A single-float or
  double-float number is returned.  A HEX-PARSE-ERROR is signaled for
  an invalid format."
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

;;; PARSE-HEX-FLOAT -- Public
;;;
;;; Parse a C-style hex float number from either a string or a stream.
(defun parse-hex-float (obj)
  "Parse a C-style hex float number from OBJ which is either a string or a stream."
  (declare (type (or string stream) obj))
  (etypecase obj
    (string
     (with-input-from-string (s obj)
       (parse-hex-float-from-stream s)))
    (stream
     (parse-hex-float-from-stream obj))))
