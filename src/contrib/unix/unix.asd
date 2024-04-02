;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :unix
  :name "cmu-unix"
  :maintainer "Cmucl project"
  :licence "Public Domain"
  :long-description "Interface to other Unix functions"
  :components
  ((:file
    #-linux "unix"
    #+linux "unix-glibc2")))
