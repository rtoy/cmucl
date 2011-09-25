;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/iso8859-9.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(defconstant +iso-8859-9+
  (make-array 96 :element-type '(unsigned-byte 16)
     :initial-contents #(160 161 162 163 164 165 166 167 168 169 170 171 172
                         173 174 175 176 177 178 179 180 181 182 183 184 185
                         186 187 188 189 190 191 192 193 194 195 196 197 198
                         199 200 201 202 203 204 205 206 207 286 209 210 211
                         212 213 214 215 216 217 218 219 220 304 350 223 224
                         225 226 227 228 229 230 231 232 233 234 235 236 237
                         238 239 287 241 242 243 244 245 246 247 248 249 250
                         251 252 305 351 255)))

(define-external-format :iso8859-9 (:base :iso8859-2 :documentation
"ISO8859-9 is an 8-bit character encoding for the Turkish language.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +iso-8859-9+)))
