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

ENABLE2="yes"
ENABLE3="yes"
ENABLE4="yes"

version=19e
SRCDIR=src
TOOLDIR=$SRCDIR/tools
TIMER="time"
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
	$TOOLDIR/clean-target.sh $CLEAN_FLAGS $TARGET
	$TIMER $TOOLDIR/build-world.sh $TARGET $OLDLISP $BOOT
	(cd $TARGET/lisp; $MAKE)
	if [ "$BUILD_WORLD2" = "yes" ];
	then
	    $TOOLDIR/build-world.sh $TARGET $OLDLISP
	fi
	$TOOLDIR/load-world.sh $TARGET "$VERSION"
	if ls $TARGET/lisp/lisp*.core 2>&1 >/dev/null; then
	    true
	else
	    echo "Failed to build $TARGET!"
	    exit 1
	fi
    fi
}

FPU_MODE=
while getopts "123o:b:v:uB:C:i:f:?" arg
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
buildit

TARGET=$BASE-4
CLEAN_FLAGS="-K all"
OLDLISP="${BASE}-3/lisp/lisp -noinit $FPU_MODE"
ENABLE=$ENABLE4

BUILD=3
buildit

if [ "$SKIPUTILS" = "no" ];
then
    OLDLISP="${BASE}-4/lisp/lisp -noinit $FPU_MODE"
    $TIMER $TOOLDIR/build-utils.sh $TARGET $FPU_MODE
fi

build_finished=`date`
echo
echo "//build started:  $build_started"
echo "//build finished: $build_finished"
