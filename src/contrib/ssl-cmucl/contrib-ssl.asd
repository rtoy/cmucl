;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :contrib-ssl
  :name "tcp-forwarder"
  :author "Eric Marsden"
  :license "GPL 2"
  :maintainer "CMU Common Lisp Group"
  :long-description "Provides SSL streams"
  :components
  ((:file "ssl-cmucl")))
