#!/bin/sh

if [ "$1" = "" ]
then
	echo "Usage: $0 target-directory [build-binary] [build-flags...]"
	exit 1
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"
LISP="${2:-lisp}"
if [ $# -ge 2 ]
then
	shift 2
else
	shift
fi

$LISP "$@" -noinit -nositeinit <<EOF
(in-package :cl-user)

;;(setf lisp::*enable-package-locked-errors* nil)

(setf (ext:search-list "target:")
      '("$TARGET/" "src/"))

(when (probe-file "target:bootstrap.lisp")
  (load "target:bootstrap.lisp"))

(load "target:setenv")

(pushnew :no-clx *features*)
(pushnew :no-clm *features*)
(pushnew :no-hemlock *features*)

(load "target:code/exports")
(load "target:tools/setup" :if-source-newer :load-source)
(comf "target:tools/setup" :load t)

(when (probe-file "home:.cmucl-fcn.lisp")
  (compile-file "home:.cmucl-fcn" :output-file "/tmp/cmucl-fcn.fasl" :load t)
  (pushnew #'rlt-before-gc-hook ext:*before-gc-hooks*)
  (setf ext:*after-gc-hooks*
	      (append ext:*after-gc-hooks* (list #'rlt-after-gc-hook))))

(setq *gc-verbose* nil)
(setq *interactive* nil)
(setq debug:*debug-print-level* nil)
(setq debug:*debug-print-length* nil)

#+nil
(when (probe-file "verify-hash.lisp")
  (compile-file "verify-hash" :output-file "/tmp/verify-hash.fasl" :load t)
  (setf ext:*after-gc-hooks*
        (append ext:*after-gc-hooks* (list #'check-all-hashes))))

(load "target:tools/worldcom")
#-(or no-compiler runtime) (load "target:tools/comcom")
;; Compile at least new-genesis, so that genesis doesn't take ages
#+(or no-compiler runtime) (comf "target:compiler/generic/new-genesis")
#-(or no-pcl runtime) (load "target:tools/pclcom")

(setq *gc-verbose* t *interactive* t)

(load "target:tools/worldbuild")
(ext:quit)
EOF
