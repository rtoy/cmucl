;;; -*- Mode: Lisp; Package: Xlib; Log: clx.log -*-

;;; Load this file if you want to compile CLX in its entirety.
#+nil
(proclaim '(optimize (speed 3) (safety 0) (space 1)
		     (compilation-speed 0)))


;;; Hide CLOS from CLX, so objects stay implemented as structures.
;;;
(when (find-package "CLOS")
  (rename-package (find-package "CLOS") "NO-CLOS-HERE"))
(when (find-package "PCL")
  (rename-package (find-package "PCL") "NO-PCL-HERE"))


(when (find-package "XLIB")
  (rename-package (find-package "XLIB") "OLD-XLIB"))

;(make-package "XLIB" :use '("LISP"))

(compile-file "clx:defsystem.lisp" :error-file nil :load t)

(with-compilation-unit ()
  (xlib:compile-clx (pathname "clx:")))
