;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :contrib-games-feebs
  :name "feebs"
  :description "Planet of the Feebs. A somewhat educational simulation game."
  :components
  ((:file "feebs")
   (:file "brains" :depends-on ("feebs"))))
