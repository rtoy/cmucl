;;;
;;; Boot file for removing the "" nickname of the KEYWORD package.
;;; To bootstrap, copy this file to target:bootstrap.lisp
;;; using Pierre Mai's build scripts, and do a full build.
;;;

(rename-package "KEYWORD" "KEYWORD")

;;; end of file.
