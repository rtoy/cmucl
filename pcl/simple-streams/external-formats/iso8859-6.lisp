;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/simple-streams/external-formats/iso8859-6.lisp,v 1.1.2.2 2009/03/28 13:40:41 rtoy Exp $")

(in-package "STREAM")

(defconstant +iso-8859-6+
  (make-array 96 :element-type '(unsigned-byte 16)
     :initial-contents #(160 65534 65534 65534 164 65534 65534 65534 65534
                         65534 65534 65534 1548 173 65534 65534 65534 65534
                         65534 65534 65534 65534 65534 65534 65534 65534 65534
                         1563 65534 65534 65534 1567 65534 1569 1570 1571 1572
                         1573 1574 1575 1576 1577 1578 1579 1580 1581 1582 1583
                         1584 1585 1586 1587 1588 1589 1590 1591 1592 1593 1594
                         65534 65534 65534 65534 65534 1600 1601 1602 1603 1604
                         1605 1606 1607 1608 1609 1610 1611 1612 1613 1614 1615
                         1616 1617 1618 65534 65534 65534 65534 65534 65534
                         65534 65534 65534 65534 65534 65534 65534)))

(define-external-format :iso8859-6 (:iso8859-2)
  ((table +iso-8859-6+)))
