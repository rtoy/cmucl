;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/cp1256.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; See http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1256.TXT
;;
;; For undefined characters we use U+FFFE

(defconstant +ms-cp1256+
  (make-array 128
              :element-type '(unsigned-byte 16)
              :initial-contents #(8364 1662 8218 402 8222 8230 8224 8225
                                  710 8240 1657 8249 338 1670 1688 1672
                                  1711 8216 8217 8220 8221 8226 8211 8212
                                  1705 8482 1681 8250 339 8204 8205 1722
                                  160 1548 162 163 164 165 166 167 168 169
                                  1726 171 172 173 174 175 176 177 178 179
                                  180 181 182 183 184 185 1563 187 188 189
                                  190 1567 1729 1569 1570 1571 1572 1573
                                  1574 1575 1576 1577 1578 1579 1580 1581
                                  1582 1583 1584 1585 1586 1587 1588 1589
                                  1590 215 1591 1592 1593 1594 1600 1601
                                  1602 1603 224 1604 226 1605 1606 1607
                                  1608 231 232 233 234 235 1609 1610 238
                                  239 1611 1612 1613 1614 244 1615 1616
                                  247 1617 249 1618 251 252 8206 8207
                                  1746)))

(define-external-format :cp1256 (:base :mac-roman :documentation
"CP1256 is a Windows code page for Arabic.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +ms-cp1256+)))
