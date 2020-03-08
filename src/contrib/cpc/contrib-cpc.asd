;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

#+solaris
(defsystem :contrib-cpc
  :name "tcp-forwarder"
  :author "Eric Marsden"
  :maintainer "CMU Common Lisp Group"
  :long-description "Interface to CPU Performace Counters Library provided with Solaris"
  :components
  ((:file "cpc")))
