;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :contrib-psgraph
  :name "PSGrapher"
  :author "Joseph Bates et al."
  :licence "Public Domain"
  :description "MIT Regression Tester"
  :long-description "The PSgrapher is a set of Lisp routines that can be called to produce
Postscript commands that display a directed acyclic graph."
  :components
  ((:file "psgraph")))


