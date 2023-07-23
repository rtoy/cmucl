;;; -*- Package: C-CALL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/c-call.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains some extensions to the Alien facility to simplify
;;; importing C interfaces.
;;;
(in-package "C-CALL")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")
(use-package "SYSTEM")

(intl:textdomain "cmucl")

(export '(char short int long long-long signed-char unsigned-char unsigned-short unsigned-int
	  unsigned-long unsigned-long-long float double c-string void))
	       

;;;; Extra types.

(def-alien-type char (integer 8))
(def-alien-type short (integer 16))
(def-alien-type int (integer 32))
(def-alien-type long (integer #-alpha 32 #+alpha 64))
(def-alien-type long-long (integer 64))
;; The same as c-call:char, for convenience with C signed-char.
(def-alien-type signed-char (integer 8))

(def-alien-type unsigned-char (unsigned 8))
(def-alien-type unsigned-short (unsigned 16))
(def-alien-type unsigned-int (unsigned 32))
(def-alien-type unsigned-long (unsigned #-alpha 32 #+alpha 64))
(def-alien-type unsigned-long-long (unsigned 64))

(def-alien-type float single-float)
(def-alien-type double double-float)

(def-alien-type-translator void ()
  (parse-alien-type '(values)))



;;;; C string support.

(def-alien-type-class (c-string :include pointer :include-args (to)))

(def-alien-type-translator c-string ()
  (make-alien-c-string-type :to (parse-alien-type 'char)))

(def-alien-type-method (c-string :unparse) (type)
  (declare (ignore type))
  'c-string)

(def-alien-type-method (c-string :lisp-rep) (type)
  (declare (ignore type))
  '(or simple-base-string null (alien (* char))))

(def-alien-type-method (c-string :naturalize-gen) (type alien)
  (declare (ignore type))
  `(if (zerop (sap-int ,alien))
       nil
       (%naturalize-c-string ,alien)))

#-unicode
(def-alien-type-method (c-string :deport-gen) (type value)
  (declare (ignore type))
  `(etypecase ,value
     (null (int-sap 0))
     ((alien (* char)) (alien-sap ,value))
     (simple-base-string (vector-sap ,value))))

#+unicode
(def-alien-type-method (c-string :deport-gen) (type value)
  (declare (ignore type))
  (let ((s (gensym "C-STRING-"))
	(len (gensym "LEN-"))
	(k (gensym "IDX-")))
    `(etypecase ,value
       (null (int-sap 0))
       ((alien (* char)) (alien-sap ,value))
       (simple-base-string
	;; FIXME: What should we do here?  For now, we just create an
	;; 8-bit array and copy our characters (the low 8-bits of each
	;; character!) to the 8-bit array.
	(let* ((,len (length ,value))
	       (,s (make-array (1+ ,len) :element-type '(unsigned-byte 8))))
	  (dotimes (,k ,len)
	    (setf (aref ,s ,k) (logand #xff (char-code (aref ,value ,k)))))
	  (setf (aref ,s ,len) 0)
	  (vector-sap ,s))))))

#-unicode
(defun %naturalize-c-string (sap)
  (declare (type system-area-pointer sap))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((length (loop
		      for offset of-type fixnum upfrom 0
		      until (zerop (sap-ref-8 sap offset))
		      finally (return offset))))
      (let ((result (make-string length)))
	(kernel:copy-from-system-area sap 0
				      result (* vm:vector-data-offset
						vm:word-bits)
				      (* length vm:byte-bits))
	result))))

;; FIXME: What should we do?  For now, just take the 8-bit strings
;; returned from C and create a new Lisp string containing those
;; characters.
#+unicode
(defun %naturalize-c-string (sap)
  (declare (type system-area-pointer sap))
  (let ((length (loop
		   for offset of-type fixnum upfrom 0
		   until (zerop (sap-ref-8 sap offset))
		   finally (return offset))))
    
    (let ((result (make-string length)))
      (dotimes (k length)
	(setf (aref result k) (code-char (sap-ref-8 sap k))))
      result)))
