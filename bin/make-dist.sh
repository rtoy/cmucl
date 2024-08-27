#!/bin/sh

# Build a binary distribution of CMUCL.  This script takes the result
# from build.sh and packages up everything into two tarballs.  One
# contains the core of cmucl; the other contains extras like clx, clm,
# and hemlock.  Optionally a source distrubition is also created.
#
# Alternatively, you can install everything into a directory, as if
# you extracted the two tarballs and the source distribution into that
# directory.
#
# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/make-dist.sh,v 1.20 2011/04/11 16:34:49 rtoy Exp $

usage() {
    echo "make-dist.sh: [-h] [-C compress] [-G group] [-O owner] [-I destdir] [-M mandir] [-A arch] [-V version] [-o OS] dir"
    echo "  -h           This help"
    echo "  -C compress  Compression method to use for the tar archives.  Must be one of"
    echo "                 bzip2, xz, or gzip.  The default depends on the OS"
    echo "  -G group     Group to use"
    echo "  -O owner     Owner to use"
    echo "  -I destdir   Install directly to given directory instead of creating a tarball"
    echo "  -M mandir    Install manpages in this subdirectory.  Default is man/man1"
    echo "  -S           Create a source distribution (requires GNU tar)"
    echo "                 The compressed tar file is named cmucl-src-<VERSION>.tar.<ext>"
    echo "                 If -I is also given, the -S means that the sources are "
    echo "                 installed in the <destdir>/src"
    echo "  -A arch      Architecture (x86, sparc, ppc, etc.)"
    echo "  -o OS        OS (linux, solaris10, etc.)"
    echo "  -V version   Version (usually date and/or other version info)"
    echo "   dir         Directory where the build is located"
    echo ""
    echo "If the -I option is given, directly install all of the files to the"
    echo "specified directory.  Otherwise, Make a CMUCL distribution consisting"
    echo "of two tar files.  One holds the main files including the C runtime,"
    echo "the lisp core, and PCL library. The second tar file contains extra"
    echo "libraries such as CLX, CLM, and Hemlock."
    echo ""
    echo "The tar files have the form cmucl-<version>-<arch>-<os>.tar.<c>"
    echo "and cmucl-<version>-<arch>-<os>.extra.tar.<c> where <version>,"
    echo "<arch>, and <os> are given values, and <c> is gz or bz2 depending"
    echo "on the selected compression method."
    echo ""
    echo "If version is not given, then a version is determined automatically"
    echo "based on the result of git describe."
    echo ""
    echo "If arch and os are not given, the script will attempt to figure an"
    echo "appropriate value for arch and os from the running system."
    echo ""
    echo "Creating a source distribution requires GNU tar.  If 'tar' is not GNU"
    echo "tar, use the environment variable 'GTAR' to specify GNU tar.  You can"
    echo "use 'GTAR=gtar make-dist.sh -S ...' in this case."
    exit 1
}

def_arch_os () {
    case `uname -s` in
      SunOS)
	  case `uname -m` in
	    sun*)
		ARCH=sparc ;;
	    i*)
		ARCH=x86
		GTAR=gtar ;;
	  esac
	  uname_r=`uname -r`
	  case $uname_r in
	    5.*) rel=`echo $uname_r | sed 's/5\.//'`;;
	    *) rel=$uname_r;;
	  esac
	  OS=solaris$rel
	  ;;
      Linux)
	  ARCH=x86
	  OS=linux
	  ;;
      Darwin)
          OS=darwin
          # x86 or ppc?
          case `uname -m` in
	      i386|x86_64) ARCH=x86 ;;
	      *) ARCH=ppc ;;
	  esac ;;
      NetBSD)
	  ARCH=x86
	  OS=netbsd
	  ;;
      FreeBSD)
	  ARCH=x86
	  OS=freebsd_`uname -r | tr 'A-Z' 'a-z'`
	  ;;
      esac
}

# Figure out the architecture and OS in case options aren't given
def_arch_os

# Choose a version based on the git hash as the default version.  We
# only compute a default if the git hash looks like a snapshot
# ("snapshot-yyyy-mm") or a release number..
GIT_HASH="`(cd src; git describe --dirty 2>/dev/null)`"

echo GIT_HASH = ${GIT_HASH}

if expr "X${GIT_HASH}" : 'Xsnapshot-[0-9][0-9][0-9][0-9]-[01][0-9]' > /dev/null; then
    DEFAULT_VERSION=`expr "${GIT_HASH}" : "snapshot-\(.*\)"`
fi

if expr "X${GIT_HASH}" : 'X[0-9][0-9][a-f]' > /dev/null; then
    DEFAULT_VERSION="${GIT_HASH}"
fi

# Default compression is -J (xz).  These variables are passed to the
# other scripts via the environmen, so export them.
COMPRESS=-J
COMPRESS_EXT=xz
COMPRESS_NAME=xz
export COMPRESS COMPRESS_EXT COMPRESS_NAME

while getopts "C:G:O:I:M:hSA:o:V:?" arg
do
    case $arg in
	C) COMPRESS_ARG=$OPTARG ;;
	G) GROUP=$OPTARG ;;
	O) OWNER=$OPTARG ;;
        I) INSTALL_DIR=$OPTARG ;;
        M) MANDIR=$OPTARG ;;
        S) MAKE_SRC_DIST=yes ;;
        A) ARCH=$OPTARG ;;
        o) OS=$OPTARG ;;
        V) VERSION=$OPTARG ;;
	h | \?) usage; exit 1 ;;
    esac
done

shift `expr $OPTIND - 1`

# Directory is required; exit if not given
if [ $# -lt 1 ]; then
    usage
fi

# Verify that the -C option is valid
if [ -n "$COMPRESS_ARG" ]; then
    case $COMPRESS_ARG in
	bzip2)
	    COMPRESS=-j
	    COMPRESS_EXT=bz2
	    COMPRESS_NAME=bzip2
	    ;;
	xz) # Defaults work
	    ;;
	gzip)
	    COMPRESS=-z
	    COMPRESS_EXT=gz
	    COMPRESS_NAME=gzip
	    ;;
	*) echo '-C option "'$COMPRESS_ARG'" must be one of bzip2, xz or gzip'
	   exit 1
	   ;;
    esac
fi

if [ -z "$VERSION" ]; then
    # If a default version exists, use it. Otherwise this is an
    # error---at least one of these must not be empty.
    if [ -z "${DEFAULT_VERSION}" ]; then
	echo "Version (-V) must be specified because default version cannot be determined."
	usage
    else
	VERSION=${DEFAULT_VERSION}
    fi
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

if [ -z "$INSTALL_DIR" ]; then
    if [ -z "$ARCH" ]; then
	echo "Unknown architecture.  Please specify one"
	usage
    fi

    if [ -z "$OS" ]; then
	echo "Unknown OS.  Please specify one"
	usage
    fi
fi   

TARGET="`echo $1 | sed 's:/*$::'`"

if [ -n "$INSTALL_DIR" ]; then
    VERSION="today"
fi

echo cmucl-$VERSION-$ARCH-$OS
ROOT=`dirname $0`

OPTIONS="-t ${GTAR:-tar} ${GROUP:+ -G ${GROUP}} ${OWNER:+ -O ${OWNER}} ${INSTALL_DIR:+ -I ${INSTALL_DIR}}"
MANDIR="${MANDIR:+ -M ${MANDIR}}"

set -x
echo Creating distribution for $ARCH $OS
$ROOT/make-main-dist.sh $OPTIONS ${MANDIR} $TARGET $VERSION $ARCH $OS || exit 1
$ROOT/make-extra-dist.sh $OPTIONS $TARGET $VERSION $ARCH $OS || exit 2

if [ X"$MAKE_SRC_DIST" = "Xyes" ]; then
    OPTIONS="${INSTALL_DIR:+ -I ${INSTALL_DIR}}"
    $ROOT/make-src-dist.sh $OPTIONS -t ${GTAR:-tar} $VERSION
fi
