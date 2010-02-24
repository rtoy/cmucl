;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/iso8859-2.lisp,v 1.2 2009/06/11 16:04:02 rtoy Rel $")

(in-package "STREAM")

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

(define-external-format :iso8859-2 (:size 1)
  ((table +iso-8859-2+ :type (simple-array (unsigned-byte 16) (96)))
   (itable (invert-table table) :type lisp::ntrie16))

  (octets-to-code (state input unput code)
    `(let ((,code ,input))
       (values (if (< ,code 160) ,code (aref ,table (- ,code 160))) 1)))
  (code-to-octets (code state output present)
    `(,output (if (< ,code 160)
		  ,code
		  (let ((,code (get-inverse ,itable ,code)))
		    (if ,code (+ (the (unsigned-byte 7) ,code) 160) #x3F))))))
