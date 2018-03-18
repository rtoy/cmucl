;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :contrib-follows-mouse
  :name "follows-mouse"
  :maintainer "Todd Kaufman"
  :licence "Public Domain"
  :long-description "This Hemlock customization causes Hemlock's current window to be set to
whatever Hemlock window the mouse enters, except the echo area.
of the X Window System."
  :components
  ((:file "follow-mouse")))


