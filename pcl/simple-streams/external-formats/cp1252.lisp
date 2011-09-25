;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/cp1252.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; See http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT

(defconstant +ms-cp1252+
  (make-array 128
              :element-type '(unsigned-byte 16)
              :initial-contents #(8364 65534 8218 402 8222 8230 8224 8225
                                  710 8240 352 8249 338 65534 381 65534
                                  65534 8216 8217 8220 8221 8226 8211 8212
                                  732 8482 353 8250 339 65534 382 376 160
                                  161 162 163 164 165 166 167 168 169 170
                                  171 172 173 174 175 176 177 178 179 180
                                  181 182 183 184 185 186 187 188 189 190
                                  191 192 193 194 195 196 197 198 199 200
                                  201 202 203 204 205 206 207 208 209 210
                                  211 212 213 214 215 216 217 218 219 220
                                  221 222 223 224 225 226 227 228 229 230
                                  231 232 233 234 235 236 237 238 239 240
                                  241 242 243 244 245 246 247 248 249 250
                                  251 252 253 254 255)))

(define-external-format :cp1252 (:base :mac-roman :documentation
"CP1252 is a Windows code page for the Latin alphabet.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +ms-cp1252+)))
