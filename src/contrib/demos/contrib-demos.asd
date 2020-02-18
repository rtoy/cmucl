;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

;; It would be nice to make demos :depend-on :clx, but we can't
;; because we don't currently have an asdf definition for :clx.

(require :clx)

(defsystem :contrib-demos
  :name "demos"
  :maintainer "CMU Common Lisp Group"
  :licence "Public Domain"
  :description "MIT Regression Tester"
  :long-description "Graphics demonstration programs for CMU Common Lisp using version 11
of the X Window System."
  :components
  ((:file "demos")))


