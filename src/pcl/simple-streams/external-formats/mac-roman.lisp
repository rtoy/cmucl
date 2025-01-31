;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/mac-roman.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; See http://unicode.org/Public/MAPPINGS/VENDORS/APPLE/ROMAN.TXT.  We
;; only need the entries from 128 to 255.
(defconstant +mac-roman+
  (make-array 128 :element-type '(unsigned-byte 16)
     :initial-contents #(196 197 199 201 209 214 220 225 224 226 228 227 229
			 231 233 232 234 235 237 236 238 239 241 243 242 244
			 246 245 250 249 251 252 8224 176 162 163 167 8226 182
			 223 174 169 8482 180 168 8800 198 216 8734 177 8804
			 8805 165 181 8706 8721 8719 960 8747 170 186 937 230
			 248 191 161 172 8730 402 8776 8710 171 187 8230 160
			 192 195 213 338 339 8211 8212 8220 8221 8216 8217 247
			 9674 255 376 8260 8364 8249 8250 64257 64258 8225 183
			 8218 8222 8240 194 202 193 203 200 205 206 207 204 211
			 212 63743 210 218 219 217 305 710 732 175 728 729 730
			 184 733 731 711)))

(define-external-format :mac-roman (:size 1 :documentation
"MAC-ROMAN is an 8-bit character encoding for Western European
languages including English.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +mac-roman+ :type (simple-array (unsigned-byte 16) (128)))
   (itable (invert-table table) :type lisp::ntrie16))

  (octets-to-code (state input unput error code)
    `(let ((,code ,input))
       (values (if (< ,code 128) ,code (aref ,table (- ,code 128))) 1)))
  (code-to-octets (code state output error present)
    `(,output (if (< ,code 128)
		  ,code
		  (let ((,present (get-inverse ,itable ,code)))
		    (if ,present
			(+ (the (unsigned-byte 7) ,present) 128)
			(if ,error
			    (locally
				;; No warnings about fdefinition
				(declare (optimize (ext:inhibit-warnings 3)))
			      (funcall ,error "Cannot output codepoint #x~X to MAC-ROMAN stream"
				       ,code))
			    #x3F))))))
  ()
  ()
  (octet-count (code state error present)
    `(if (< ,code 128)
	 1
	 (let ((,present (get-inverse ,itable ,code)))
	   (if ,present
	       1
	       (if ,error
		   (locally
		       ;; No warnings about fdefinition
		       (declare (optimize (ext:inhibit-warnings 3)))
		     (funcall ,error "Cannot output codepoint #x~X to MAC-ROMAN stream"
			      ,code))
		   1))))))
