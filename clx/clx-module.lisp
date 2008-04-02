;;;(in-package :xlib)
;;;(common-lisp:use-package (list :common-lisp))

#+cmu
(ext:file-comment "$Id: clx-module.lisp,v 1.1 2007/08/21 15:49:27 fgilham Exp $")

(provide :clx)
(load "clx:defsystem.lisp")
(load-clx (translate-logical-pathname "CLX:"))
