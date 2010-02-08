;; Bootstrap file for adding support for localization.
(defpackage "INTL"
  (:use "COMMON-LISP")
  (:export "SETLOCALE" "TEXTDOMAIN" "GETTEXT" "DGETTEXT" "NGETTEXT" "DNGETTEXT"
           "*TRANSLATABLE-DUMP-STREAM*" "READ-TRANSLATABLE-STRING"
	   "*LOCALE-DIRECTORIES*"))

(with-open-file (s "target:code/intl.lisp")
  (compile-from-stream s))

(intl::install)
