;; Bootstrap file for adding support for localization.

(defvar lisp::*environment-list-initialized* nil)

(defpackage "INTL"
  (:use "COMMON-LISP")
  (:export "SETLOCALE" "TEXTDOMAIN" "GETTEXT" "DGETTEXT" "NGETTEXT" "DNGETTEXT"
           "*TRANSLATABLE-DUMP-STREAM*" "READ-TRANSLATABLE-STRING"
	   "*LOCALE-DIRECTORIES*"))

(with-open-file (s "target:code/intl.lisp")
  (compile-from-stream s))

(intl::install)


(in-package "C")
;; The textdomain for the documentation
(define-info-type function textdomain (or string null) nil)
(define-info-type variable textdomain (or string null) nil)
(define-info-type type textdomain (or string null) nil)
(define-info-type typed-structure textdomain (or string null) nil)
(define-info-type setf textdomain (or string null) nil)

