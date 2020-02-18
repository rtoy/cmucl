;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :contrib-embedded-c
  :name "embedded-c"
  :author "Helmut Eller"
  :license "Public Domain"
  :long-description "This package provides a macro to embed compiled C code in 
a Lisp fasl file.  It's inspired by GForth FFI."
  :components
  ((:file "embedded-c")))