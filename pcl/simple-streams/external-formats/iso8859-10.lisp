;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/iso8859-10.lisp,v 1.1.2.1 2008/07/02 01:22:10 rtoy Exp $")

(defconstant +iso-8859-10+
  (make-array 96 :element-type '(unsigned-byte 16)
     :initial-contents #(160 260 274 290 298 296 310 167 315 272 352 358 381
                         173 362 330 176 261 275 291 299 297 311 183 316 273
                         353 359 382 8213 363 331 256 193 194 195 196 197 198
                         302 268 201 280 203 278 205 206 207 208 325 332 211
                         212 213 214 360 216 370 218 219 220 221 222 223 257
                         225 226 227 228 229 230 303 269 233 281 235 279 237
                         238 239 240 326 333 243 244 245 246 361 248 371 250
                         251 252 253 254 312)))

(define-external-format :iso8859-10 (:iso8859-2)
  ((table +iso-8859-10+)))
