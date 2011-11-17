;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/mac-latin2.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(defconstant +mac-latin2+
  (make-array 128 :element-type '(unsigned-byte 16)
     :initial-contents #(196 256 257 201 260 214 220 225 261 268 228 269 262
                         263 233 377 378 270 237 271 274 275 278 243 279 244
                         246 245 250 282 283 252 8224 176 280 163 167 8226 182
                         223 174 169 8482 281 168 8800 291 302 303 298 8804
                         8805 299 310 8706 8721 322 315 316 317 318 313 314 325
                         326 323 172 8730 324 327 8710 171 187 8230 160 328 336
                         213 337 332 8211 8212 8220 8221 8216 8217 247 9674 333
                         340 341 344 8249 8250 345 342 343 352 8218 8222 353
                         346 347 193 356 357 205 381 382 362 211 212 363 366
                         218 367 368 369 370 371 221 253 311 379 321 380 290
                         711)))

(define-external-format :mac-latin2 (:base :mac-roman :documentation

"MAC-LATIN2 is an 8-bit character encoding for Central European text
on Apple Macintosh computers.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +mac-latin2+)))
