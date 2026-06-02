;; Bootstrap file for adding the Extended_Pictographic word-break
;; support (Unicode 17.0 word-break rule WB3c).
;;
;; The new function LISP::UNICODE-EXTENDED-PICTOGRAPHIC-P is imported
;; into the UNICODE package by code/exports.lisp.  When the new
;; exports.lisp is compiled by the bootstrapping lisp, that symbol does
;; not yet exist in the LISP package, so the (:import-from "LISP" ...)
;; clause would fail.  Intern it here first so the package definition
;; can be read.

(in-package :lisp)

(intern "UNICODE-EXTENDED-PICTOGRAPHIC-P" "LISP")
