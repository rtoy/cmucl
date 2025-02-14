#!/bin/sh

if [ "$1" = "" ]
then
	echo "Usage: $0 target-directory"
	exit 1
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"
shift

$TARGET/lisp/lisp \
	-lib $TARGET/lisp -noinit -nositeinit -batch "$@" <<EOF || exit 3
(in-package :cl-user)

(setf lisp::*enable-package-locked-errors* nil)
(setf (ext:search-list "target:")
      '("$TARGET/" "src/"))

(setf *default-pathname-defaults* (ext:default-directory))
(intl:install)
(intl::translation-enable)
(load "target:setenv")

(pushnew :no-clx *features*)
(pushnew :no-clm *features*)
(pushnew :no-hemlock *features*)

(compile-file "target:tools/setup" :load t)
(setq *gc-verbose* nil *interactive* nil)
(load "target:tools/clxcom")
(load "target:clx/clx-library")
#-ppc
(load "target:tools/clmcom")
(load "target:tools/hemcom")

EOF

# Find GNU make:

if [ "$MAKE" = "" ]
then    
    MAKE="`which gmake`"

    # Some versions of which set an error code if it fails.  Others
    # say "no foo in <path>".  In either of these cases, just assume
    # make is GNU make.

    if [ $? -ne 0 ]; then
	MAKE="make"
    fi
    if echo "X$MAKE" | grep '^Xno' > /dev/null; then
	MAKE="make"
    fi
fi

export MAKE

# Don't bother building motifd on ppc; we'll probably never support that again.

SKIPMOTIF=no
case `uname -s` in
  Darwin)
      case `uname -p` in
	powerpc) SKIPMOTIF=yes ;;
      esac ;;
esac

if [ "$SKIPMOTIF" = "no" ]; then
    ${MAKE} -C $TARGET/motif/server clean && ${MAKE} -C $TARGET/motif/server
fi

