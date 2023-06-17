#!/bin/sh 

# Build CMUCL from source.  The intent of this script is to make it a
# little easier invoke the other scripts and build CMUCL.  In the
# simplest case where your lisp is named cmulisp and no special
# bootfiles are needed, you would build CMUCL using:
#
#    src/tools/build.sh -C ""
#
# This will create a set of directories named build-2, build-3, and
# build-4 and CMUCL will be built 3 consecutive times, using the
# version of CMUCL from the previous build.
#
#
# You can control which of the builds are done by using the -1, -2, -3
# options, but it is up to you to make sure the previous builds exist.
#
# A more realistic example would be
#
#    src/tools/build.sh -v "My build" -B boot-19b.lisp -o "my-lisp -noinit"
#
# where you need to load the bootfile boot-19b.lisp and your lisp is
# not named cmulisp, but my-lisp.
#
# For more complicated builds, you will need to run create-target.sh
# manually, and adjust the bootstrap or setenv files by hand.  Once
# this is done, you can run build.sh to build everything.  Just be
# sure to leave off the -C option.
#
# Cross compiling is not supported with this script.  You will have to
# do that by hand.
#
# For more information see src/BUILDING.
#
# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/build.sh,v 1.38 2010/11/10 19:17:55 rtoy Exp $
#

ENABLE2="yes"
ENABLE3="yes"
ENABLE4="yes"

version=21d
SRCDIR=src
BINDIR=bin
TOOLDIR=$BINDIR
OLDLISPFLAGS="-noinit -nositeinit"
OLDLISP="cmulisp"
GIT_FILE_COMMENT="yes"
export GIT_FILE_COMMENT

SKIPUTILS=no

# If gmake exists, assume it is GNU make and use it.
if [ -z "$MAKE" ]; then
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

usage ()
{
    echo "build.sh [-123obvuBCUG?]"
    echo "    -1        Skip build 1"
    echo "    -2        Skip build 2"
    echo "    -3        Skip build 3"
    echo "    -o x      Use specified Lisp to build.  Default is cmulisp"
    echo "               (only applicable for build 1)"
    echo '    -b d      The different build directories are named ${d}-2, ${d}-3 ${d}-4'
    echo '               If -b is not given, a suitable name based on the OS is used.' 
    echo '    -v v      Use the given string as the version.  Default is'
    echo "               based on git describe"
    echo "    -u        Don't build CLX, CLM, or Hemlock"
    echo '    -i n      Make build "n" interactive, so output is sent to *standard-output*'
    echo '               instead of the log file. "n" should be a string consisting of'
    echo '                the numbers 1, 2, or 3.'
    echo "    -B file   Use file as a boot file.  May be be specified more than once"
    echo "               The file is relative to the bootfiles/<version> directory"
    echo '    -C [l m]  Create the build directories.  The args are what'
    echo '               you would give to create-target.sh for the lisp'
    echo '               and motif variant.'
    echo '    -P        On the last build, do NOT generate cmucl.pot and do NOT update'
    echo '               the translations.'
    echo "    -?        This help message"
    echo "    -w        Specify a different build-world.sh script"
    echo "    -U        If translations are done, overwrite the files with the"
    echo "               translations instead of computing and displaying the diffs."
    echo "    -O opt    Any additional command-line flags to use when building."
    echo "               The flags always include -noinit -nositeinit"
    echo "    -R        Force recompiling the C runtime.  Normally, just runs make to "
    echo "               recompile anything that has changed."
    echo "    -G        Don't use git to fill file-comment information"
    exit 1
}

# Figure out if we need to run build-world twice
case `uname -s` in
  SunOS) BUILD_WORLD2=yes ;;
  Darwin)
      case `uname -m` in
	ppc) BUILD_WORLD2=yes ;;
      esac ;;
esac

buildit ()
{
    if echo $INTERACTIVE_BUILD | grep $BUILD > /dev/null; then
	INTERACTIVE=t
    else
	INTERACTIVE=nil
    fi

    if [ ! -d $TARGET ]; then
	if [ -n "$CREATE_DIRS" ]; then
	    $TOOLDIR/create-target.sh $TARGET $CREATE_OPT
	fi
    fi

    if [ "$ENABLE" = "yes" ]; 
    then
	$TOOLDIR/clean-target.sh $CLEAN_FLAGS $TARGET || { echo "Failed: $TOOLDIR/clean-target.sh"; exit 1; }
	time $BUILDWORLD $TARGET $OLDLISP $BOOT || { echo "Failed: $BUILDWORLD"; exit 1; }
	if [ "$REBUILD_LISP" = "yes" ]; then
	    $TOOLDIR/rebuild-lisp.sh $TARGET
	fi
	# else
	    # Set the LANG to C.  For whatever reason, if I (rtoy) don't
	    # do this on my openSuSE system, any messages from gcc are
	    # basically garbled.  This should be harmless on other
	    # systems.
	# LANG=C $MAKE -C $TARGET/lisp $MAKE_TARGET || { echo "Failed: $MAKE -C $TARGET/lisp"; exit 1; }
        # fi

	if [ "$BUILD_WORLD2" = "yes" ];
	then
	    $BUILDWORLD $TARGET $OLDLISP $BOOT || { echo "Failed: $BUILDWORLD"; exit 1; }
	fi
	$TOOLDIR/load-world.sh $TARGET "$VERSION" || { echo "Failed: $TOOLDIR/load-world.sh"; exit 1; }

	$TARGET/lisp/lisp -batch -noinit -nositeinit < /dev/null || { echo "Failed: $TARGET/lisp/lisp -batch -noinit"; exit 1; }
	return 0;
    fi
}

BUILDWORLD="$TOOLDIR/build-world.sh"
BUILD_POT="yes"
UPDATE_TRANS=

while getopts "123PRGo:b:v:uB:C:Ui:w:O:?" arg
do
    case $arg in
	1) ENABLE2="no" ;;
	2) ENABLE3="no" ;;
	3) ENABLE4="no" ;;
	o) OLDLISP=$OPTARG ;;
	b) BASE=$OPTARG ;;
	v) VERSION="$OPTARG $GIT_HASH" ;;
	u) SKIPUTILS="yes" ;;
	C) CREATE_OPT="$OPTARG"
	   CREATE_DIRS=yes ;;
	B) bootfiles="$bootfiles $OPTARG" ;;
        i) INTERACTIVE_BUILD="$OPTARG" ;;
        P) BUILD_POT=no ;;
        w) BUILDWORLD="$OPTARG" ;;
        U) UPDATE_TRANS="yes";;
	O) OLDLISPFLAGS="$OLDLISPFLAGS $OPTARG" ;;
        R) REBUILD_LISP="yes";;
	G) GIT_FILE_COMMENT="no";;
	\?) usage
	    ;;
    esac
done

# If -b not given, try to derive one instead of just using "build".
if [ -z "$BASE" ]; then
    case `uname -s` in
      Darwin)
          case `uname -p` in
            powerpc) BASE=ppc ;;
            i386) BASE=darwin ;;
          esac ;;
      SunOS)
	  case `uname -m` in
	    sun4*) BASE=sparc ;;
	    i86pc) BASE=sol-x86 ;;
	  esac ;;
      Linux) BASE=linux ;;
      # Add support for FreeBSD and NetBSD?  Otherwise default to just build.
      *) BASE=build ;;
    esac
fi

echo base = $BASE

bootfiles_dir=$SRCDIR/bootfiles/$version
if [ -n "$bootfiles" ]; then
    for file in $bootfiles; do
	BOOT="$BOOT -load $bootfiles_dir/$file"
    done
fi

build_started=`date`
echo "//starting build: $build_started"

TARGET=$BASE-2
ENABLE=$ENABLE2
MAKE_TARGET=all
export INTERACTIVE

BUILD=1
OLDLISP="$OLDLISP $OLDLISPFLAGS"
buildit

bootfiles=

TARGET=$BASE-3
OLDLISP="${BASE}-2/lisp/lisp $OLDLISPFLAGS"
ENABLE=$ENABLE3

BUILD=2
# We shouldn't have to run build-world again because nothing should
# have changed in the C code after the first build.
BUILD_WORLD2=
buildit

TARGET=$BASE-4
CLEAN_FLAGS="-K all"
OLDLISP="${BASE}-3/lisp/lisp $OLDLISPFLAGS"
ENABLE=$ENABLE4

if [ "${BUILD_POT}" = "yes" ]; then
   MAKE_POT=yes
   if [ "${UPDATE_TRANS}" = "yes" ]; then
       MAKE_TARGET="all translations-update"
   else
       MAKE_TARGET="all translations"
   fi
   export MAKE_POT
fi

BUILD=3
echo "MAKE_TARGET = ${MAKE_TARGET}"
buildit

# Asdf and friends are part of the base install, so we need to build
# them now.
$TARGET/lisp/lisp -noinit -nositeinit -batch << EOF || exit 3
(in-package :cl-user)
(setf (ext:search-list "target:")
      '("$TARGET/" "src/"))
(setf (ext:search-list "modules:")
      '("target:contrib/"))

(compile-file "modules:asdf/asdf")
(compile-file "modules:defsystem/defsystem")
(intl::install)
(ext:without-package-locks
  (let ((path #-linux "modules:unix/unix"
              #+linux "modules:unix/unix-glibc2"))
    (ensure-directories-exist (compile-file-pathname path))
    (compile-file path)))
EOF


if [ "$SKIPUTILS" = "no" ];
then
    OLDLISP="${BASE}-4/lisp/lisp $OLDLISPFLAGS"
    time $TOOLDIR/build-utils.sh $TARGET
fi

build_finished=`date`
echo
echo "//build started:  $build_started"
echo "//build finished: $build_finished"
