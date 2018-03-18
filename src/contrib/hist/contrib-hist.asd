;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :contrib-hist
  :name "hist"
  :maintainer "Scott E. Fahlman"
  :licence "Public Domain"
  :long-description "Simple histogram facility using Format strings for output."
  :components
  ((:file "hist")))


