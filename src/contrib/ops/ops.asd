;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :ops
  :name "ops"
  :licence "Public Domain"
  :description "MIT Regression Tester"
  :long-description "Interpreter for Ops5, a programming language for production systems."
  :components
  ((:file "ops-util")
   (:file "ops-compile")
   (:file "ops-rhs")
   (:file "ops-match")
   (:file "ops-main")
   (:file "ops-backup")
   (:file "ops-io")
   (:file "ops")))


