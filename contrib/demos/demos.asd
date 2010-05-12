;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :demos
  :name "demos"
  :maintainer "CMU Common Lisp Group"
  :licence "Public Domain"
  :description "MIT Regression Tester"
  :long-description "Graphics demonstration programs for CMU Common Lisp using version 11
of the X Window System."
  :components
  ((:file "demos")))


