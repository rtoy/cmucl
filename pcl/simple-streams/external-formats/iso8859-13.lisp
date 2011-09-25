;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/iso8859-13.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(defconstant +iso-8859-13+
  (make-array 96 :element-type '(unsigned-byte 16)
     :initial-contents #(160 8221 162 163 164 8222 166 167 216 169 342 171 172
                         173 174 198 176 177 178 179 8220 181 182 183 248 185
                         343 187 188 189 190 230 260 302 256 262 196 197 280
                         274 268 201 377 278 290 310 298 315 352 323 325 211
                         332 213 214 215 370 321 346 362 220 379 381 223 261
                         303 257 263 228 229 281 275 269 233 378 279 291 311
                         299 316 353 324 326 243 333 245 246 247 371 322 347
                         363 252 380 382 8217)))

(define-external-format :iso8859-13 (:base :iso8859-2 :documentation
"ISO8859-13 is an 8-bit character encoding for the Baltic languages.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +iso-8859-13+)))
