;;; -*- Package: User -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/interface/initial.lisp,v 1.3 1993/08/22 20:31:36 wlott Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "USER")

(pushnew :motif *features*)

(setf (getf ext:*herald-items* :motif)
      `("    Motif toolkit and graphical debugger 1.0"))

(defpackage "INTERFACE"
  (:use "TOOLKIT" "LISP" "PCL" "EXTENSIONS" "KERNEL")
  (:shadow "CLASS-DIRECT-SUPERCLASSES")
  (:export "*HEADER-FONT*" "*ITALIC-FONT*" "*ENTRY-FONT*" "*INTERFACE-STYLE*"
	   "USE-GRAPHICS-INTERFACE" "VERIFY-SYSTEM-SERVER-EXISTS"
	   "CREATE-INTERFACE-SHELL" "POPUP-INTERFACE-PANE"
	   "CREATE-INTERFACE-PANE-SHELL" "FIND-INTERFACE-PANE"
	   "DESTROY-INTERFACE-PANE" "CREATE-HIGHLIGHT-BUTTON" "CREATE-VALUE-BOX"
	   "SET-VALUE-BOX" "WITH-WIDGET-CHILDREN" "INTERFACE-ERROR"
	   "PRINT-FOR-WIDGET-DISPLAY" "WITH-BUSY-CURSOR"
	   "CREATE-INTERFACE-MENU" "CREATE-CACHED-MENU"
	   "GRAB-OUTPUT-AS-STRING" "*ALL-FONTS*" "LISP-CONTROL-PANEL"))
