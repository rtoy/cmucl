;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/iso8859-2.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")


(defconstant +iso-8859-2+
  (make-array 96 :element-type '(unsigned-byte 16)
     :initial-contents #(160 260 728 321 164 317 346 167 168 352 350 356 377
                         173 381 379 176 261 731 322 180 318 347 711 184 353
                         351 357 378 733 382 380 340 193 194 258 196 313 262
                         199 268 201 280 203 282 205 206 270 272 323 327 211
                         212 336 214 215 344 366 218 368 220 221 354 223 341
                         225 226 259 228 314 263 231 269 233 281 235 283 237
                         238 271 273 324 328 243 244 337 246 247 345 367 250
                         369 252 253 355 729)))

(define-external-format :iso8859-2 (:size 1 :documentation
"ISO8859-2 is an 8-bit character encoding generally intended for
Eastern European languages including Bosnian, Croation, Czech, German,
Hungarian, Polish, Romanian, Serbian Latin, Slovak, Slovene, Upper
Sorbian, and Lower Sorbian.  

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +iso-8859-2+ :type (simple-array (unsigned-byte 16) (96)))
   (itable (invert-table table) :type lisp::ntrie16))

  (octets-to-code (state input unput error code)
    `(let ((,code ,input))
       (values (if (< ,code 160) ,code (aref ,table (- ,code 160))) 1)))
  (code-to-octets (code state output error present)
    `(,output (if (< ,code 160)
		  ,code
		  (let ((,present (get-inverse ,itable ,code)))
		    (if ,present
			(+ (the (unsigned-byte 7) ,present) 160)
			(if ,error
			    (locally
				;; No warnings about fdefinition
				(declare (optimize (ext:inhibit-warnings 3)))
			      (funcall ,error "Cannot output codepoint #x~X to ISO8859-2 stream"
				       ,code))
			    #x3F))))))
  ()
  ()
  (octet-count (code state error present)
    `(if (< ,code 160)
	 1
	 (let ((,present (get-inverse ,itable ,code)))
	   (if ,present
	       1
	       (if ,error
		   (locally
		       ;; No warnings about fdefinition
		       (declare (optimize (ext:inhibit-warnings 3)))
		     (funcall ,error "Cannot output codepoint #x~X to ISO8859-2 stream"
			      ,code))
		   1))))))
