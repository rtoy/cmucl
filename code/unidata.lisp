;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/unidata.lisp,v 1.1.2.1 2009/04/11 12:04:26 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Unicode Database access

(in-package "LISP")

(defconstant +unidata-path+ #p"ext-formats:unidata.bin")

(defstruct unidata
  range
  name+
  name
  category
  scase
  )

(defvar *unicode-data* (make-unidata))

;;; These need to be synched with tools/build-unidata.lisp

(defstruct nametrie
  (cdbk (ext:required-argument) :read-only t :type simple-vector)
  (keyv (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 8) (*)))
  (keyl (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 8) (*)))
  (codev (ext:required-argument) :read-only t
	 :type (simple-array (signed-byte 32) (*)))
  (nextv (ext:required-argument) :read-only t
	 :type (simple-array (unsigned-byte 32) (*)))
  (namev (ext:required-argument) :read-only t
	 :type (simple-array (unsigned-byte 32) (*))))

(defstruct codeset
  (code (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 32) (*))))

(defstruct rangeset
  (min (ext:required-argument) :read-only t
       :type (simple-array (unsigned-byte 32) (*)))
  (max (ext:required-argument) :read-only t
       :type (simple-array (unsigned-byte 32) (*))))

(defstruct (name (:include codeset))
  (low (ext:required-argument) :read-only t
       :type (simple-array (unsigned-byte 8) (*))))

(defstruct (category (:include rangeset)))

(defstruct (scase (:include codeset))
  (ext (ext:required-argument) :read-only t
       :type (simple-array (unsigned-byte 32) (*))))



(defun search-codeset (code codeset)
  (declare #+nil(optimize (speed 3) (space 0) (debug 0) (safety 0))
	   (type (integer 0 #x10FFFF) code) (type codeset codeset))
  (let* ((data (codeset-code codeset)) (min 0) (max (length data)))
    (declare (type lisp::index min max))
    (loop for n of-type lisp::index = (ash (+ min max) -1) do
      (let* ((data (logand #x1FFFFF (aref data n))))
	(cond ((< code data)
	       (when (= (the lisp::index (setq max n)) min) (return nil)))
	      ((> code data)
	       (when (= (setq min (1+ n)) max) (return nil)))
	      (t (return n)))))))

(defun search-rangeset (code rangeset)
  (declare #+nil(optimize (speed 3) (space 0) (debug 0) (safety 0))
	   (type (integer 0 #x10FFFF) code) (type rangeset rangeset))
  (let* ((hi (rangeset-min rangeset))
	 (lo (rangeset-max rangeset))
	 (min 0)
	 (max (length hi)))
    (declare (type lisp::index min max))
    (loop for n of-type lisp::index = (ash (+ min max) -1) do
      (let* ((dmin (logand #x1FFFFF (aref hi n)))
	     (dmax (logand #x1FFFFF (aref lo n))))
	(cond ((< code dmin)
	       (when (= (the lisp::index (setq max n)) min) (return nil)))
	      ((> code dmax)
	       (when (= (setq min (1+ n)) max) (return nil)))
	      (t (return n)))))))



(defun unidata-locate (stream index)
  (labels ((read16 (stm)
	     (logior (ash (read-byte stm) 8) (read-byte stm)))
	   (read32 (stm)
	     (logior (ash (read16 stm) 16) (read16 stm))))
    (unless (and (= (read32 stream) #x2A554344)
		 (= (read-byte stream) 0))
      (error "The Unicode data file is broken."))
    (let ((a (read-byte stream))
	  (b (read-byte stream))
	  (c (read-byte stream)))
      (unless (and (= a 5) (= b 1) (= c 0))
	(warn "Unicode data file is for Unicode ~D.~D.~D" a b c)))
    (dotimes (i index)
      (when (zerop (read32 stream))
	(return-from unidata-locate nil)))
    (let ((n (read32 stream)))
      (and n (file-position stream n)))))

(defmacro defloader (name (stm locn) &body body)
  `(defun ,name ()
     (labels ((read16 (stm)
		(logior (ash (read-byte stm) 8) (read-byte stm)))
	      (read32 (stm)
		(logior (ash (read16 stm) 16) (read16 stm))))
       (with-open-file (,stm +unidata-path+ :direction :input
			     :element-type '(unsigned-byte 8))
	 (unless (unidata-locate ,stm ,locn)
	   (error "No data in file."))
	 ,@body))))

(defloader load-range (stm 0)
  (let* ((n (read32 stm))
	 (min (make-array n :element-type '(unsigned-byte 32)))
	 (max (make-array n :element-type '(unsigned-byte 32))))
    (read-vector min stm :endian-swap :network-order)
    (read-vector max stm :endian-swap :network-order)
    (setf (unidata-range *unicode-data*)
	(make-rangeset :min min :max max))))

(defloader load-names (stm 1)
  (let* ((cb (1+ (read-byte stm)))
	 (kv (read16 stm))
	 (cv (read32 stm))
	 (codebook (make-array cb))
	 (keyv (make-array kv :element-type '(unsigned-byte 8)))
	 (keyl (make-array kv :element-type '(unsigned-byte 8)))
	 (codev (make-array cv :element-type '(signed-byte 32)))
	 (nextv (make-array cv :element-type '(unsigned-byte 32)))
	 (namev (make-array cv :element-type '(unsigned-byte 32))))
    (dotimes (i cb)
      (let* ((n (read-byte stm))
	     (s (make-string n)))
	(setf (aref codebook i) s)
	(dotimes (i n) (setf (char s i) (code-char (read-byte stm))))))
    (read-vector keyv stm :endian-swap :network-order)
    (read-vector keyl stm :endian-swap :network-order)
    (read-vector codev stm :endian-swap :network-order)
    (read-vector nextv stm :endian-swap :network-order)
    (read-vector namev stm :endian-swap :network-order)
    (setf (unidata-name+ *unicode-data*)
	(make-nametrie :cdbk codebook :keyv keyv :keyl keyl
		       :codev codev :nextv nextv :namev namev))))

(defloader load-name (stm 2)
  (let* ((n (read32 stm))
	 (code (make-array n :element-type '(unsigned-byte 32)))
	 (low (make-array n :element-type '(unsigned-byte 8))))
    (read-vector code stm :endian-swap :network-order)
    (read-vector low stm :endian-swap :network-order)
    (setf (unidata-name *unicode-data*)
	(make-name :code code :low low))))

(defloader load-categories (stm 3)
  (let* ((n (read32 stm))
	 (min (make-array n :element-type '(unsigned-byte 32)))
	 (max (make-array n :element-type '(unsigned-byte 32))))
    (read-vector min stm :endian-swap :network-order)
    (read-vector max stm :endian-swap :network-order)
    (setf (unidata-category *unicode-data*)
	(make-category :min min :max max))))

(defloader load-scase (stm 4)
  (let* ((n (read32 stm))
	 (code (make-array n :element-type '(unsigned-byte 32)))
	 (ext (make-array n :element-type '(unsigned-byte 32))))
    (read-vector code stm :endian-swap :network-order)
    (read-vector ext stm :endian-swap :network-order)
    (setf (unidata-scase *unicode-data*)
	(make-scase :code code :ext ext))))



(defun unicode-name-to-codepoint (name)
  (declare #+nil(optimize (speed 3) (space 0) (debug 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type string name))
  (unless (unidata-name+ *unicode-data*) (load-names))
  (let* ((names (unidata-name+ *unicode-data*))
	 (codebook (nametrie-cdbk names))
	 (current 0)
	 (posn 0)
	 (stack '()))
    (declare (type nametrie names)
	     (type (unsigned-byte 32) current)
	     (type lisp::index posn))
    (loop
      (let ((keyv (ash (aref (nametrie-nextv names) current) -18)))
	(dotimes (i (aref (nametrie-keyl names) keyv)
		    (if stack
			(let ((next (pop stack)))
			  (setq posn (car next) current (cdr next)))
			(return-from unicode-name-to-codepoint nil)))
	  (let* ((str (aref codebook (aref (nametrie-keyv names) (+ keyv i))))
		 (len (length str)))
	    (declare (type simple-base-string str))
	    (when (and (>= (length name) (+ posn len))
		       (string= name str :start1 posn :end1 (+ posn len)))
	      (setq current
		  (+ (logand (aref (nametrie-nextv names) current) #x3FFFF) i))
	      (when (= (incf posn len) (length name))
		(return-from unicode-name-to-codepoint
		  (aref (nametrie-codev names) current)))
	      (return))			; from DOTIMES - loop again
	    (when (or (string= str " ") (string= str "-"))
	      (push (cons posn
			  (+ (logand (aref (nametrie-nextv names) current)
				     #x3FFFF)
			     i))
		    stack))))))))

(defun unicode-name (code)
  (declare #+nil(optimize (speed 3) (space 0) (debug 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type (integer 0 #x10FFFF) code))
  (unless (unidata-name+ *unicode-data*) (load-names))
  (unless (unidata-name *unicode-data*) (load-name))
  (let* ((name (unidata-name *unicode-data*))
	 (i (search-codeset code name)))
    (when i
      (let* ((names (unidata-name+ *unicode-data*))
	     (codebook (nametrie-cdbk names))
	     (namev (nametrie-namev names))
	     (nextv (nametrie-nextv names))
	     (keyv (nametrie-keyv names))
	     (n (logior (ash (ldb (byte 11 21) (aref (name-code name) i)) 8)
			(aref (name-low name) i)))
	     (p (ash (aref namev n) -18))
	     (s (make-string p)))
	(loop while (plusp n) do
	  (let* ((prev (logand (aref namev n) #x3FFFF))
		 (temp (aref nextv prev))
		 (base (logand temp #x3FFFF))
		 (str (aref codebook
			    (aref keyv (+ (ash temp -18) (- n base))))))
	    (declare (type simple-base-string str))
	    (setq p (- p (length str)) n prev)
	    (replace s str :start1 p)))
	s))))

(defun unicode-category (code)
  (declare #+nil(optimize (speed 3) (space 0) (debug 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type (integer 0 #x10FFFF) code))
  (unless (unidata-category *unicode-data*) (load-categories))
  (let* ((cat (unidata-category *unicode-data*))
	 (i (search-rangeset code cat)))
    (if i
	(ldb (byte 11 21) (aref (category-min cat) i))
	#x08)))				; "Cn"

(defun unicode-upper (code)
  (declare #+nil(optimize (speed 3) (space 0) (debug 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type (integer 0 #x10FFFF) code))
  (unless (unidata-scase *unicode-data*) (load-scase))
  (let* ((tbl (unidata-scase *unicode-data*))
	 (i (search-codeset code tbl)))
    (if (or (not i) (logbitp 31 (aref (scase-code tbl) i)))
	code
	(logand (aref (scase-ext tbl) i) #x1FFFFF))))

(defun unicode-lower (code)
  (declare #+nil(optimize (speed 3) (space 0) (debug 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type (integer 0 #x10FFFF) code))
  (unless (unidata-scase *unicode-data*) (load-scase))
  (let* ((tbl (unidata-scase *unicode-data*))
	 (i (search-codeset code tbl)))
    (if (and i (logbitp 31 (aref (scase-code tbl) i)))
	(logand (aref (scase-ext tbl) i) #x1FFFFF)
	code)))

(defun unicode-title (code)
  (declare #+nil(optimize (speed 3) (space 0) (debug 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type (integer 0 #x10FFFF) code))
  (unless (unidata-scase *unicode-data*) (load-scase))
  (let* ((tbl (unidata-scase *unicode-data*))
	 (i (search-codeset code tbl)))
    (if i
	(logior (logand (ash (aref (scase-code tbl) i) -10) #x1FF800)
		(ash (aref (scase-ext tbl) i) -21))
	code)))
