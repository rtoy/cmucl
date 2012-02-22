;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/cp1255.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; See http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1255.TXT
;;
;; For undefined characters we use U+FFFE

(defconstant +ms-cp1255+
  (make-array 128
              :element-type '(unsigned-byte 16)
              :initial-contents #(8364 65534 8218 402 8222 8230 8224 8225
                                  710 8240 65534 8249 65534 65534 65534
                                  65534 65534 8216 8217 8220 8221 8226
                                  8211 8212 732 8482 65534 8250 65534
                                  65534 65534 65534 160 161 162 163 8362
                                  165 166 167 168 169 215 171 172 173 174
                                  175 176 177 178 179 180 181 182 183 184
                                  185 247 187 188 189 190 191 1456 1457
                                  1458 1459 1460 1461 1462 1463 1464 1465
                                  65534 1467 1468 1469 1470 1471 1472 1473
                                  1474 1475 1520 1521 1522 1523 1524 65534
                                  65534 65534 65534 65534 65534 65534 1488
                                  1489 1490 1491 1492 1493 1494 1495 1496
                                  1497 1498 1499 1500 1501 1502 1503 1504
                                  1505 1506 1507 1508 1509 1510 1511 1512
                                  1513 1514 65534 65534 8206 8207 65534)))

(define-external-format :cp1255 (:base :mac-roman :documentation
"CP1255 is a Windows code page for Hebrew.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +ms-cp1255+)))
