;;; Simple cross-compile file for moving closure_tramp and
;;; undefined_tramp to Lisp assembly routines.
;;;
;;; But note that to do the cross-compile using the 2010-06 binaries,
;;; we need a cross-bootstrap file.  Thus, use boot-2010-07-1.lisp as
;;; the cross-bootstrap file.  The cross-compile can be done as follows:
;;;
;;; src/tools/cross-build-world.sh -rl -B src/bootfiles/20a/boot-2010-07-1.lisp <target>
;;;     <cross> src/bootfiles/20a/boot-2010-07-1-cross <oldlisp> <lisp options>


#+x86
(load "target:tools/cross-scripts/cross-x86-x86")

#+sparc
(load "target:tools/cross-scripts/cross-sparc-sparc")
