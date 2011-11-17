;;;(in-package :xlib)
;;;(common-lisp:use-package (list :common-lisp))

#+cmu
(ext:file-comment "$Id: clx-module.lisp,v 1.2 2009/06/17 18:22:45 rtoy Rel $")

(provide :clx)
(load "clx:defsystem.lisp")
(load-clx (translate-logical-pathname "CLX:"))
