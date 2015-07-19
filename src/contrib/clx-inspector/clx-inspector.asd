;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(require :clx)

(defsystem :clx-inspector
  :name "INSPECT"
  :author "Skef Wholey et. al."
  :maintainer "Fred Gilham"
  :license "Public Domain"
  :description "Graphical Inspector"
  :long-description "Inspector that uses pop-up windows to display the
  objects. Updates the values of the objects in the background."
  :components
  ((:file "clx-inspector")))


