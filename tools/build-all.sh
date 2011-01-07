#! /bin/sh

# Build all variants of cmucl.  This means build x87 and sse2 versions
# for both unicode and non-unicode variants.
# Basically call build.sh to do the builds.

usage ()
{
    echo "build-all.sh [bBCvU] [-o old] [-8 old8bit]"
    echo ""
    echo "Build all variants of cmucl for a specific architecture."
    echo "This means build the unicode and non-unicode variants."
    echo "For x86 architectures, the x87 and sse2 cores are also built."
    echo "This script basically automates calling build.sh for all the"
    echo "variants, using the most common options.  If there are special"
    echo "requirements, then you must use build.sh itself instead of this"
    echo "script."
    echo ""
    echo "    -b d      Basename of the different build directories."
    echo "               ${d}-2, ${d}-3, ${d}-4 for unicode"
    echo "               ${d}-8bit-2, ${d}-8bit-3, ${d}-8bit-4 for non-unicode"
    echo "    -B file   Use file as a boot file.  Maybe be specified more than once"
    echo "               The file is relative to the bootfiles/<version> directory"
    echo '    -C [l m]  Create the build directories.  The args are what'
    echo '               you would give to create-target.sh for the lisp'
    echo '               and motif variant.'
    echo '    -v v      Use the given string as the version.  Default is'
    echo "               today's date"
    echo "    -o x      Use specified Lisp to build unicode version."
    echo "               (only applicable for build 1)"
    echo "    -8 x      Use specified Lisp to build 8-bit version."
    echo "               (only applicable for build 1)"
    echo "    -U        Update and overwite the CVS translations files."
}

VERSION="CVS Head `date '+%Y-%m-%d %H:%M:%S'`"
BASE=build
CREATE_OPT=""

while getopts "UB:b:v:C:o:8:?" arg
do
    case $arg in
      b) BASE="$OPTARG" ;;
      B) bootfiles="$bootfiles -B $OPTARG" ;;
      C) CREATE_OPT="$OPTARG" ;;
      o) OLDLISP="$OPTARG" ;;
      8) OLD8="$OPTARG" ;;
      v) VERSION="$OPTARG" ;;
      U) UPDATE_TRANS="-U" ;;
      \?) usage ;;
    esac
done

if [ "$BASE" = "" ]; then
    echo "-b option required"
    exit 1
fi

if [ "$OLDLISP" = "" -a "$OLD8" = "" ]; then
    echo "-o or -8 option required"
    exit 1
fi

buildx86 ()
{
    if [ -n "$OLD8" ]; then
	# Build non-unicode versions
	set -x
	src/tools/build.sh -f x87 -b ${BASE}-8bit $bootfiles -v "$VERSION" -C "${CREATE_OPT}" ${UPDATE_TRANS} -o "$OLD8"
	src/tools/build.sh -f sse2 -b ${BASE}-8bit $bootfiles -v "$VERSION" -C "${CREATE_OPT}" ${UPDATE_TRANS} -o "$OLD8"
	set +x
    fi
    # Build the unicode versions
    if [ -n "$OLDLISP" ]; then
	set -x
	src/tools/build.sh -f x87 -b ${BASE} $bootfiles -v "$VERSION" -C "${CREATE_OPT}" ${UPDATE_TRANS} -o "$OLDLISP"
	src/tools/build.sh -f sse2 -b ${BASE} $bootfiles -v "$VERSION" -C "${CREATE_OPT}" ${UPDATE_TRANS} -o "$OLDLISP"
	set +x
    fi
}

buildsun4 ()
{
    # Build non-unicode versions
    if [ -n "$OLD8" ]; then
	set -x
	src/tools/build.sh -b ${BASE}-8bit $bootfiles -v "$VERSION" -C "$CREATE_OPT" ${UPDATE_TRANS} -o "$OLD8"
	set +x
    fi
    # Build the unicode version.
    if [ -n "$OLDLISP" ]; then
	set -x
	src/tools/build.sh -b ${BASE} $bootfiles -v "$VERSION" -C "$CREATE_OPT" ${UPDATE_TRANS} -o "$OLDLISP"
	set +x
    fi
}

case `uname -m` in
  i386*|x86*) buildx86 ;;
  sun*) buildsun4 ;;
  *) echo "Unsupported architecture:  `uname -m`" ;;
esac
