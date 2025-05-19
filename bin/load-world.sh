#!/bin/sh

usage()
{
    cat <<EOF
load-world.sh [-?p] target-directory
   -p    Skip loading of PCL (Mostly for cross-compiling)
   -h    This help
   -?    This help

Load the world in target-directory
EOF
    exit 1
}

SKIP_PCL=
NO_PCL_FEATURE=

while getopts "ph?" arg
do
  case $arg in
      p) SKIP_PCL="yes"
         shift;;
      h|\?) usage ;;
  esac
done

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="$(echo "$1" | sed 's:/*$::')"

# If -p given, we want to skip loading of PCL.  Do this by pushing
# :no-pcl onto *features*

if [ -n "$SKIP_PCL" ]; then
    NO_PCL_FEATURE="(pushnew :no-pcl *features*)"
fi

# If version string given, use it, otherwise use the default.
if [ -n "$2" ]; then
    VERSION="$2"
fi

"$TARGET"/lisp/lisp -core "$TARGET"/lisp/kernel.core <<EOF
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
