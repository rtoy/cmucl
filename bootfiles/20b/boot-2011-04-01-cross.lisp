;; Add -unidata command line option to allow user to tell cmucl where
;; the unidata.bin file is.
;;
;; To build 2011-06, you need to do a cross-compile.  Use this as the
;; cross-compile bootstrap file.

#+x86
(load "target:tools/cross-scripts/cross-x86-x86.lisp")

#+sparc
(load "target:tools/cross-scripts/cross-sparc-sparc.lisp")
