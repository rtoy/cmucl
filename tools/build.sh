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
# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/build.sh,v 1.28.2.2 2010/02/12 13:19:49 rtoy Exp $
#

ENABLE2="yes"
ENABLE3="yes"
ENABLE4="yes"

version=20a
SRCDIR=src
TOOLDIR=$SRCDIR/tools
VERSION="CVS Head `date '+%Y-%m-%d %H:%M:%S'`"
BASE=build
OLDLISP="cmulisp -noinit"

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
    echo "build.sh [-123obvuBC?]"
    echo "    -1        Skip build 1"
    echo "    -2        Skip build 2"
    echo "    -3        Skip build 3"
    echo "    -o x      Use specified Lisp to build.  Default is cmulisp"
    echo "               (only applicable for build 1)"
    echo '    -b d      The different build directories are named ${d}-2, ${d}-3 ${d}-4'
    echo '               with a default of "build"'
    echo '    -v v      Use the given string as the version.  Default is'
    echo "               today's date"
    echo "    -u        Don't build CLX, CLM, or Hemlock"
    echo '    -i n      Make build "n" interactive, so output is sent to *standard-output*'
    echo '               instead of the log file. "n" should be a string consisting of'
    echo '                the numbers 1, 2, or 3.'
    echo "    -B file   Use file as a boot file.  Maybe be specified more than once"
    echo "               The file is relative to the bootfiles/<version> directory"
    echo '    -C [l m]  Create the build directories.  The args are what'
    echo '               you would give to create-target.sh for the lisp'
    echo '               and motif variant.'
    echo '    -f mode   FPU mode:  x87, sse2, or auto.  Default is auto'
    echo '    -P        On the last build, generate cmucl.pot"
    echo "    -?        This help message"

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
	time $TOOLDIR/build-world.sh $TARGET $OLDLISP $BOOT || { echo "Failed: $TOOLDIR/build-world.sh"; exit 1; }
	$MAKE -C $TARGET/lisp || { echo "Failed: $MAKE -C $TARGET/lisp"; exit 1; }
	if [ "$BUILD_WORLD2" = "yes" ];
	then
	    $TOOLDIR/build-world.sh $TARGET $OLDLISP $BOOT || { echo "Failed: $TOOLDIR/build-world.sh"; exit 1; }
	fi
	$TOOLDIR/load-world.sh $TARGET "$VERSION" || { echo "Failed: $TOOLDIR/load-world.sh"; exit 1; }

	$TARGET/lisp/lisp -batch -noinit $FPU_MODE < /dev/null || { echo "Failed: $TARGET/lisp/lisp -batch -noinit $FPU_MODE"; exit 1; }
	return 0;
    fi
}

FPU_MODE=
while getopts "123Po:b:v:uB:C:i:f:?" arg
do
    case $arg in
	1) ENABLE2="no" ;;
	2) ENABLE3="no" ;;
	3) ENABLE4="no" ;;
	o) OLDLISP=$OPTARG ;;
	b) BASE=$OPTARG ;;
	v) VERSION="$OPTARG" ;;
	u) SKIPUTILS="yes" ;;
	C) CREATE_OPT="$OPTARG"
	   CREATE_DIRS=yes ;;
	B) bootfiles="$bootfiles $OPTARG" ;;
        i) INTERACTIVE_BUILD="$OPTARG" ;;
	f) FPU_MODE="-fpu $OPTARG" ;;
        P) BUILD_POT=yes ;;
	\?) usage
	    ;;
    esac
done

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
export INTERACTIVE

BUILD=1
buildit

bootfiles=

TARGET=$BASE-3
OLDLISP="${BASE}-2/lisp/lisp -noinit $FPU_MODE"
ENABLE=$ENABLE3

BUILD=2
# We shouldn't have to run build-world again because nothing should
# have changed in the C code after the first build.
BUILD_WORLD2=
buildit

TARGET=$BASE-4
CLEAN_FLAGS="-K all"
OLDLISP="${BASE}-3/lisp/lisp -noinit $FPU_MODE"
ENABLE=$ENABLE4

if [ "${BUILD_POT}" = "yes" ]; then
   MAKE_POT=yes
   export MAKE_POT
fi

BUILD=3
buildit

if [ "$SKIPUTILS" = "no" ];
then
    OLDLISP="${BASE}-4/lisp/lisp -noinit $FPU_MODE"
    time $TOOLDIR/build-utils.sh $TARGET $FPU_MODE
fi

build_finished=`date`
echo
echo "//build started:  $build_started"
echo "//build finished: $build_finished"
