;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :contrib-packed-sse2
  :name "cmu-packed-sse2"
  :maintainer "Cmucl project"
  :licence "Public Domain"
  :long-description "Functions to work with packed SSE2 objects"
  :components
  ((:file "packed-sse2")))
