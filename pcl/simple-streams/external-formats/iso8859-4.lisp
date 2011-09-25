;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/iso8859-4.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(defconstant +iso-8859-4+
  (make-array 96 :element-type '(unsigned-byte 16)
     :initial-contents #(160 260 312 342 164 296 315 167 168 352 274 290 358
                         173 381 175 176 261 731 343 180 297 316 711 184 353
                         275 291 359 330 382 331 256 193 194 195 196 197 198
                         302 268 201 280 203 278 205 206 298 272 325 332 310
                         212 213 214 215 216 370 218 219 220 360 362 223 257
                         225 226 227 228 229 230 303 269 233 281 235 279 237
                         238 299 273 326 333 311 244 245 246 247 248 371 250
                         251 252 361 363 729)))

(define-external-format :iso8859-4 (:base :iso8859-2 :documentation
"ISO8859-4 is an 8-bit character encoding for North European languages
including Estonian, Latvian, Lithuanian, Greenlandic, and Sami.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +iso-8859-4+)))
