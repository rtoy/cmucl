;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/iso8859-14.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(defconstant +iso-8859-14+
  (make-array 96 :element-type '(unsigned-byte 16)
     :initial-contents #(160 7682 7683 163 266 267 7690 167 7808 169 7810 7691
                         7922 173 174 376 7710 7711 288 289 7744 7745 182 7766
                         7809 7767 7811 7776 7923 7812 7813 7777 192 193 194
                         195 196 197 198 199 200 201 202 203 204 205 206 207
                         372 209 210 211 212 213 214 7786 216 217 218 219 220
                         221 374 223 224 225 226 227 228 229 230 231 232 233
                         234 235 236 237 238 239 373 241 242 243 244 245 246
                         7787 248 249 250 251 252 253 375 255)))

(define-external-format :iso8859-14 (:base :iso8859-2 :documentation
"ISO8859-14 is an 8-bit character encoding intended for the Celtic
languages such as Irish, Manx, Scottish Gaelic, Welsh, and Breton.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +iso-8859-14+)))
