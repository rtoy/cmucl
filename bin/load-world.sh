#!/bin/sh

usage()
{
    echo "load-world.sh [-?pv:] target-directory"
    echo "   -p      Skip loading of PCL (Mostly for cross-compiling)"
    echo "   -v ver  Set the value of *lisp-implementation-version* to VER"
    echo "   -?      This help"
    exit 1
}

SKIP_PCL=
NO_PCL_FEATURE=

while getopts "p?v:" arg
do
  case $arg in
      p) SKIP_PCL="yes"
         shift;;
      v) VERSION="$OPTARG"
	 shift 2
	 ;;
      \?) usage ;;
  esac
done

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"

# If -p given, we want to skip loading of PCL.  Do this by pushing
# :no-pcl onto *features*

if [ -n "$SKIP_PCL" ]; then
    NO_PCL_FEATURE="(pushnew :no-pcl *features*)"
fi

# If version string not given, use the git version
if [ -z "$VERSION" ]; then
    # Try to get the version from cmucl-version.h so that we match
    # exactly what was used for building lisp.  Otherwise, fall back
    # to git-version.sh
    FILE="src/lisp/cmucl-version.h"
    if [ -f "$FILE" ]; then
	VERSION=`grep CMUCL_VERSION "$FILE" | sed 's/^.*CMUCL_VERSION //'`
    else
	VERSION=`bin/git-version.sh`
    fi
fi

$TARGET/lisp/lisp -core $TARGET/lisp/kernel.core <<EOF
(in-package :cl-user)

(setf (ext:search-list "target:")
      '("$TARGET/" "src/"))

(load "target:setenv")

(pushnew :no-clx *features*)
(pushnew :no-clm *features*)
(pushnew :no-hemlock *features*)
$NO_PCL_FEATURE

(load "target:tools/worldload")
$VERSION

EOF
