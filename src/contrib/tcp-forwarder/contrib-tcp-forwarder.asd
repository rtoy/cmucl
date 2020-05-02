;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :contrib-tcp-forwarder
  :name "tcp-forwarder"
  :author "Eric Marsden"
  :maintainer "CMU Common Lisp Group"
  :long-description "A port forwarder, or redirector, redirecting TCP connections to another port on another machine."
  :components
  ((:file "tcp-forwarder")))
