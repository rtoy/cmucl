;;; -*- Package: User -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/interface/initial.lisp $")
;;;
;;; **********************************************************************
;;;
(in-package "CL-USER")

(pushnew :motif *features*)

(setf (getf ext:*herald-items* :motif)
      `("    Motif toolkit and graphical debugger 1.0"))

(provide :clm)

(defpackage "INTERFACE"
  (:use "TOOLKIT" "LISP" "EXTENSIONS" "KERNEL")
  (:shadow "CLASS-DIRECT-SUPERCLASSES")
  (:export "*INTERFACE-STYLE*" "+HEADER-TAG+" "+ITALIC-TAG+"
           "*DEFAULT-FONT-NAME*" "*HEADER-FONT-NAME*" "*ITALIC-FONT-NAME*"
           "*AMBIGUOUS-FONT-DISPOSITION*"
	   "USE-GRAPHICS-INTERFACE" "VERIFY-SYSTEM-SERVER-EXISTS"
	   "CREATE-INTERFACE-SHELL" "POPUP-INTERFACE-PANE"
	   "CREATE-INTERFACE-PANE-SHELL" "FIND-INTERFACE-PANE"
	   "DESTROY-INTERFACE-PANE" "CREATE-HIGHLIGHT-BUTTON" "CREATE-VALUE-BOX"
	   "SET-VALUE-BOX" "WITH-WIDGET-CHILDREN" "INTERFACE-ERROR"
	   "PRINT-FOR-WIDGET-DISPLAY" "WITH-BUSY-CURSOR"
	   "CREATE-INTERFACE-MENU" "CREATE-CACHED-MENU"
	   "GRAB-OUTPUT-AS-STRING" "LISP-CONTROL-PANEL"))
