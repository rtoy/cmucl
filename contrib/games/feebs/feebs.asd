;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :feebs
  :name "feebs"
  :description "Planet of the Feebs. A somewhat educational simulation game."
  :components
  ((:file "feebs")
   (:file "brains")))