;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; CLX -- exclcmac.cl
;;;           This file provides for inline expansion of some functions.
;;;
;;; Copyright (c) 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.
;;;
;;; Franz Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib :use '(:foreign-functions :lisp :excl))

(import '(excl::defcmacro))

;;
;; Type predicates
;;
(defcmacro card8p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (fixnump ,xx) (> #.(expt 2 8) ,xx) (>= ,xx 0)))))

(defcmacro card16p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (fixnump ,xx) (> #.(expt 2 16) ,xx) (>= ,xx 0)))))

(defcmacro int8p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (fixnump ,xx) (> #.(expt 2 7) ,xx) (>= ,xx #.(expt -2 7))))))

(defcmacro int16p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (fixnump ,xx) (> #.(expt 2 15) ,xx) (>= ,xx #.(expt -2 15))))))

;; Card29p, card32p, int32p are too large to expand inline


;;
;; Type transformers
;;
(defcmacro card8->int8 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card8 ,xx))
       (the int8 (if (logbitp 7 ,xx)
		     (the int8 (- ,xx #x100))
		   ,xx)))))
(defcmacro int8->card8 (x)
  `(locally ,(declare-bufmac)
     (the card8 (ldb (byte 8 0) (the int8 ,x)))))

(defcmacro card16->int16 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card16 ,xx))
       (the int16 (if (logbitp 15 ,xx)
		      (the int16 (- ,xx #x10000))
		    ,xx)))))

(defcmacro int16->card16 (x)
  `(locally ,(declare-bufmac)
     (the card16 (ldb (byte 16 0) (the int16 ,x)))))

(defcmacro card32->int32 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card32 ,xx))
       (the int32 (if (logbitp 31 ,xx)
		      (the int32 (- ,xx #x100000000))
		    ,xx)))))

(defcmacro int32->card32 (x)
  `(locally ,(declare-bufmac)
     (the card32 (ldb (byte 32 0) (the int32 ,x)))))

(defcmacro char->card8 (char)
  `(locally ,(declare-bufmac)
     (the card8 (char-code (the string-char ,char)))))

(defcmacro card8->char (card8)
  `(locally ,(declare-bufmac)
     (the string-char (code-char (the card8 ,card8)))))


;;
;; Array accessors and setters
;;
(defcmacro aref-card8 (a i)
  `(locally ,(declare-bufmac)
	    (the card8 (aref (the buffer-bytes ,a) (the array-index ,i)))))

(defcmacro aset-card8 (v a i)
  `(locally ,(declare-bufmac)
	    (setf (aref (the buffer-bytes ,a) (the array-index ,i))
	      (the card8 ,v))))

(defcmacro aref-int8 (a i)
  `(locally ,(declare-bufmac)
	    (card8->int8 (aref (the buffer-bytes ,a) (the array-index ,i)))))

(defcmacro aset-int8 (v a i)
  `(locally ,(declare-bufmac)
	    (setf (aref (the buffer-bytes ,a) (the array-index ,i))
	      (int8->card8 ,v))))

(defcmacro aref-card16 (a i)
  `(locally ,(declare-bufmac)
     (the card16 (sys:memref (the buffer-bytes ,a)
			     #.(comp::mdparam 'comp::md-svector-data0-adj)
			     (the array-index ,i)
			     :unsigned-word))))
  
(defcmacro aset-card16 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
			     #.(comp::mdparam 'comp::md-svector-data0-adj)
			     (the array-index ,i)
			     :unsigned-word)
       (the card16 ,v))))
  
(defcmacro aref-int16 (a i)
  `(locally ,(declare-bufmac)
     (the int16 (sys:memref (the buffer-bytes ,a)
			    #.(comp::mdparam 'comp::md-svector-data0-adj)
			    (the array-index ,i)
			    :signed-word))))
  
(defcmacro aset-int16 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :signed-word)
       (the int16 ,v))))
  
(defcmacro aref-card32 (a i)
  `(locally ,(declare-bufmac)
     (the card32 (sys:memref (the buffer-bytes ,a)
			     #.(comp::mdparam 'comp::md-svector-data0-adj)
			     (the array-index ,i)
			     :unsigned-long))))
    
(defcmacro aset-card32 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :unsigned-long)
       (the card32 ,v))))

(defcmacro aref-int32 (a i)
  `(locally ,(declare-bufmac)
     (the int32 (sys:memref (the buffer-bytes ,a)
			    #.(comp::mdparam 'comp::md-svector-data0-adj)
			    (the array-index ,i)
			    :signed-long))))
    
(defcmacro aset-int32 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :signed-long)
       (the int32 ,v))))

(defcmacro aref-card29 (a i)
  ;; Don't need to mask bits here since X protocol guarantees top bits zero
  `(locally ,(declare-bufmac)
     (the card29 (sys:memref (the buffer-bytes ,a)
			     #.(comp::mdparam 'comp::md-svector-data0-adj)
			     (the array-index ,i)
			     :unsigned-long))))

(defcmacro aset-card29 (v a i)
  ;; I also assume here Lisp is passing a number that fits in 29 bits.
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(comp::mdparam 'comp::md-svector-data0-adj)
		       (the array-index ,i)
		       :unsigned-long)
       (the card29 ,v))))

;;
;; Font accessors
;;
(defcmacro font-id (font)
  ;; Get font-id, opening font if needed
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-id-internal ,f)
	   (open-font-internal ,f)))))

(defcmacro font-font-info (font)
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-font-info-internal ,f)
	   (query-font ,f)))))

(defcmacro font-char-infos (font)
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-char-infos-internal ,f)
	   (progn (query-font ,f)
		  (font-char-infos-internal ,f))))))


;;
;; Miscellaneous
;;
(defcmacro current-process ()
  `(the (or mp::process null) (and mp::*scheduler-stack-group*
				  mp::*current-process*)))

(defcmacro process-wakeup (process)
  (let ((proc (gensym)))
    `(let ((.pw-curproc. mp::*current-process*)
	   (,proc ,process))
       (when (and .pw-curproc. ,proc)
	 (if (> (mp::process-priority ,proc)
		(mp::process-priority .pw-curproc.))
	     (mp::process-allow-schedule ,proc))))))

#+notyet
(defcmacro buffer-replace (target-sequence source-sequence target-start
					   target-end &optional (source-start 0))
  (let ((tv (gensym)) (sv (gensym)) (ts (gensym)) (te (gensym)) (ss (gensym)))
    `(let ((,tv ,target-sequence) (,sv ,source-sequence)
	   (,ts ,target-start) (,te ,target-end) (,ss ,source-start))
       (declare (type buffer-bytes ,tv ,sv)
		(type array-index ,ts ,te ,ss)
		(optimize (speed 3) (safety 0)))
  
       (let ((source-end (length ,sv)))
	 (declare (type array-index source-end))
    
	 (if* (and (eq ,tv ,sv)
		   (> ,ts ,ss))
	    then (let ((nelts (min (- ,te ,ts)
				   (- source-end ,ss))))
		   (do ((target-index (+ ,ts nelts -1) (1- target-index))
			(source-index (+ ,ss nelts -1) (1- source-index)))
		       ((= target-index (1- ,ts)) ,tv)
		     (declare (type array-index target-index source-index))
		
		     (setf (aref ,tv target-index)
		       (aref ,sv source-index))))
	    else (do ((target-index ,ts (1+ target-index))
		      (source-index ,ss (1+ source-index)))
		     ((or (= target-index ,te) (= source-index source-end))
		      ,tv)
		   (declare (type array-index target-index source-index))

		   (setf (aref ,tv target-index)
		     (aref ,sv source-index))))))))

(defcmacro buffer-new-request-number (buffer)
  (let ((buf (gensym)))
    `(let ((,buf ,buffer))
       (declare (type buffer ,buf))
       (setf (buffer-request-number ,buf)
	 (ldb (byte 16 0) (1+ (buffer-request-number ,buf)))))))

(defcmacro byte-reverse (byte)
  `(aref ,'#.(coerce
	     '#(0 128 64 192 32 160 96 224 16 144 80 208 48 176 112 240
		8 136 72 200 40 168 104 232 24 152 88 216 56 184 120 248
		4 132 68 196 36 164 100 228 20 148 84 212 52 180 116 244
		12 140 76 204 44 172 108 236 28 156 92 220 60 188 124 252
		2 130 66 194 34 162 98 226 18 146 82 210 50 178 114 242
		10 138 74 202 42 170 106 234 26 154 90 218 58 186 122 250
		6 134 70 198 38 166 102 230 22 150 86 214 54 182 118 246
		14 142 78 206 46 174 110 238 30 158 94 222 62 190 126 254
		1 129 65 193 33 161 97 225 17 145 81 209 49 177 113 241
		9 137 73 201 41 169 105 233 25 153 89 217 57 185 121 249
		5 133 69 197 37 165 101 229 21 149 85 213 53 181 117 245
		13 141 77 205 45 173 109 237 29 157 93 221 61 189 125 253
		3 131 67 195 35 163 99 227 19 147 83 211 51 179 115 243
		11 139 75 203 43 171 107 235 27 155 91 219 59 187 123 251
		7 135 71 199 39 167 103 231 23 151 87 215 55 183 119 247
		15 143 79 207 47 175 111 239 31 159 95 223 63 191 127 255)
	     '(vector card8))
	 ,byte))

#|
#+(or allegro-v3.0 allegro-v3.1)
(defcmacro graphic-char-p (char)
  `(let* ((cint (char-int ,char)))
     (if (and (<= #.(char-code #\space) cint)
	      (<= cint #.(char-code #\~)))
	 t
       nil)))
|#

