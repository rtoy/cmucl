;;;
;;; Boot file for removing the "" nickname of the KEYWORD package,
;;; removing the USER nickname from CL-USER, and removing the
;;; LISP nickname from COMMON-LISP, the latter by introducing
;;; a new COMMON-LISP package which LISP uses.
;;;
;;; To bootstrap, copy this file to target:bootstrap.lisp
;;; using Pierre Mai's build scripts, and do a full build.
;;;

;;;
;;; Remove the nickname "" from the KEYWORD package.
;;;
;;; Remove the nickname USER from CL-USER.
;;;
;;; Make a new package COMMON-LISP, from which LISP inherits
;;; the standard symbols.
;;;
;;; Move DEBUG and FLOATING-POINT-INEXACT from EXT to CL.
;;;

(in-package :lisp)

(rename-package "KEYWORD" "KEYWORD" nil)

(cl:rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("CL-USER"))
(unuse-package "COMMON-LISP" "COMMON-LISP-USER")
(use-package "LISP" "COMMON-LISP-USER")

(rename-package "COMMON-LISP" "COMMON-LISP" nil)
(rename-package "COMMON-LISP" "LISP" nil)
(make-package "COMMON-LISP" :nicknames '("CL") :use nil)

(let ((cl (find-package "CL"))
      (lisp (find-package "LISP")))
  (do-external-symbols (sym lisp)
    (unintern sym lisp)
    (let ((syms (list sym)))
      (import syms cl)
      (export syms cl)
      (import syms lisp)
      (export syms lisp))))

(cl:use-package "CL" "LISP")

(in-package :cl-user)

;;; end of file.
