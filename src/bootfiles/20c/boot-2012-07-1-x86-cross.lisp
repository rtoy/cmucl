;;; Bootstrap for building the 2012-08 snapshot from the 2012-07
;;; snapshot.  The inline allocation routines are changing.

;;; Nothing fancy needed; just use do a simple cross-build-world using
;;; this as the script.
#+x86
(load "target:tools/cross-scripts/cross-x86-x86")

