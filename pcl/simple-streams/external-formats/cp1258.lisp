;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/cp1258.lisp,v 1.2 2009/06/11 16:04:02 rtoy Exp $")

(in-package "STREAM")

(defconstant +ms-cp1258+
  (make-array 128 :element-type '(unsigned-byte 16)
     :initial-contents #(8364 65534 8218 402 8222 8230 8224 8225 710 8240 65534
                         8249 338 65534 65534 65534 65534 8216 8217 8220 8221
                         8226 8211 8212 732 8482 65534 8250 339 65534 65534 376
                         160 161 162 163 164 165 166 167 168 169 170 171 172
                         173 174 175 176 177 178 179 180 181 182 183 184 185
                         186 187 188 189 190 191 192 193 194 258 196 197 198
                         199 200 201 202 203 768 205 206 207 272 209 777 211
                         212 416 214 215 216 217 218 219 220 431 771 223 224
                         225 226 259 228 229 230 231 232 233 234 235 769 237
                         238 239 273 241 803 243 244 417 246 247 248 249 250
                         251 252 432 8363 255)))

(define-external-format :cp1258 (:mac-roman)
  ((table +ms-cp1258+)))
