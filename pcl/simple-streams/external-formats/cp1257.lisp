;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/cp1257.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; See http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1257.TXT
;;
;; For undefined characters we use U+FFFE

(defconstant +ms-cp1257+
  (make-array 128
              :element-type '(unsigned-byte 16)
              :initial-contents #(8364 65534 8218 65534 8222 8230 8224
                                  8225 65534 8240 65534 8249 65534 168 711
                                  184 65534 8216 8217 8220 8221 8226 8211
                                  8212 65534 8482 65534 8250 65534 175 731
                                  65534 160 65534 162 163 164 65534 166
                                  167 216 169 342 171 172 173 174 198 176
                                  177 178 179 180 181 182 183 248 185 343
                                  187 188 189 190 230 260 302 256 262 196
                                  197 280 274 268 201 377 278 290 310 298
                                  315 352 323 325 211 332 213 214 215 370
                                  321 346 362 220 379 381 223 261 303 257
                                  263 228 229 281 275 269 233 378 279 291
                                  311 299 316 353 324 326 243 333 245 246
                                  247 371 322 347 363 252 380 382 729)))

(define-external-format :cp1257 (:base :mac-roman :documentation
"CP1257 is a Windows code page for Estonian, Latvian, and Lithuanian.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +ms-cp1257+)))
