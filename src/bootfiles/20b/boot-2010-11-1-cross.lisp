;;; Bootstrap for building the 2010-12 snapshot.  Adding slots to the
;;; backend requires a cross-compile.
;;;
;;; Answer CLOBBER-IT for the restart about changing the size of the
;;; backend structure.

#+x86
(load "target:tools/cross-scripts/cross-x86-x86")

#+sparc
(load "target:tools/cross-scripts/cross-sparc-sparc")
