#! /bin/sh

# Build all variants of cmucl.  This means build x87 and sse2 versions
# for both unicode and non-unicode variants.
# Basically call build.sh to do the builds.

usage ()
{
    echo "build-all.sh [-bBCvUP] [-o old] [-8 old8bit]"
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
    echo "               If -b is not given, a suitable name is chosen"
    echo "               based on the OS."
    echo "               ${d}-8bit-2, ${d}-8bit-3, ${d}-8bit-4 for non-unicode"
    echo "    -B file   Use file as a boot file.  Maybe be specified more than once"
    echo "               The file is relative to the bootfiles/<version> directory"
    echo '    -C [l m]  Create the build directories.  The args are what'
    echo '               you would give to create-target.sh for the lisp'
    echo '               and motif variant.'
    echo '    -v v      Use the given string as the version.'
    echo "    -o x      Use specified Lisp to build unicode version."
    echo "               (only applicable for build 1)"
    echo "    -8 x      DEPRECATED: Use specified Lisp to build 8-bit version."
    echo "               (only applicable for build 1)"
    echo "    -U        Update and overwite the translations files."
    echo "    -P        On the last build, (re)generate cmucl.pot and the"
    echo "               translations"
    echo "    -R        Force recompilation of C runtime"
}

CREATE_OPT=""
UPDATE_POT="-P"

while getopts "PRUB:b:v:C:o:8:?" arg
do
    case $arg in
      b) BASE="$OPTARG" ;;
      B) bootfiles="$bootfiles -B $OPTARG" ;;
      C) CREATE_OPT="$OPTARG" ;;
      o) OLDLISP="$OPTARG" ;;
      8) OLD8="$OPTARG" 
	  echo "WARNING: -8 is deprecated";;
      v) VERSION="$OPTARG"; VERSION_SET=true ;;
      U) UPDATE_TRANS="-U" ;;
      P) UPDATE_POT="" ;;
      R) RECOMPILEC="-R" ;;
      \?) usage; exit 1 ;;
    esac
done

# If -b not given, try to derive one instead of just using "build".
if [ -z "$BASE" ]; then
    case `uname -s` in
      Darwin) # We only support darwin-x86 now.  No ppc available anymore.
	  BASE=darwin ;;
      SunOS)
	  case `uname -m` in
	    sun4u) BASE=sparc ;;
	    i86pc) BASE=sol-x86 ;;
	  esac ;;
      Linux) BASE=linux ;;
      # Add support for FreeBSD and NetBSD?  Otherwise default to just build.
      *) BASE=build ;;
    esac
fi

if [ "$OLDLISP" = "" -a "$OLD8" = "" ]; then
    echo "-o or -8 option required"
    exit 1
fi

BINDIR=bin
buildx86 ()
{
    if [ -n "$OLD8" ]; then
	# Build non-unicode versions
	set -x
	$BINDIR/build.sh -f x87 -b ${BASE}-8bit $bootfiles ${VERSION:+-v "$VERSION"} -C "${CREATE_OPT}" ${UPDATE_TRANS} ${UPDATE_POT} ${RECOMPILEC} -o "$OLD8"
	$BINDIR/build.sh -f sse2 -b ${BASE}-8bit $bootfiles ${VERSION:+-v "$VERSION"} -C "${CREATE_OPT}" ${UPDATE_TRANS} ${UPDATE_POT} ${RECOMPILEC} -o "$OLD8"
	set +x
    fi
    # Build the unicode versions
    if [ -n "$OLDLISP" ]; then
	set -x
	$BINDIR/build.sh -f x87 -b ${BASE} $bootfiles ${VERSION:+-v "$VERSION"} -C "${CREATE_OPT}" ${UPDATE_TRANS} ${UPDATE_POT} ${RECOMPILEC} -o "$OLDLISP"
	$BINDIR/build.sh -f sse2 -b ${BASE} $bootfiles ${VERSION:+-v "$VERSION"} -C "${CREATE_OPT}" ${UPDATE_TRANS} ${UPDATE_POT} ${RECOMPILEC} -o "$OLDLISP"
	set +x
    fi
}

buildsun4 ()
{
    if [ "$VERSION_SET" = true ]; then
      VERS="-v '"$VERSION"'"
    fi
    # Build non-unicode versions
    if [ -n "$OLD8" ]; then
	set -x
	$BINDIR/build.sh -b ${BASE}-8bit $bootfiles ${VERS} -C "$CREATE_OPT" ${UPDATE_TRANS} ${UPDATE_POT} ${RECOMPILEC} -o "$OLD8"
	set +x
    fi
    # Build the unicode version.
    if [ -n "$OLDLISP" ]; then
	set -x
	$BINDIR/build.sh -b ${BASE} $bootfiles ${VERS} -C "$CREATE_OPT" ${UPDATE_TRANS} ${UPDATE_POT} ${RECOMPILEC} -o "$OLDLISP"
	set +x
    fi
}

case `uname -m` in
  i386*|x86*|i86pc) buildx86 ;;
  sun*) buildsun4 ;;
  *) echo "Unsupported architecture:  `uname -m`" ;;
esac
