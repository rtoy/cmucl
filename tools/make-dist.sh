#!/bin/sh

usage() {
    echo "make-dist.sh: [-hbg] [-G group] [-O owner] dir version arch os"
    echo "  -h           This help"
    echo "  -b           Use bzip2 compression"
    echo "  -g           Use gzip compression"
    echo "  -G group     Group to use"
    echo "  -O owner     Owner to use"
    echo "   dir         Directory where the build is located"
    echo "   version     Version (usually date and/or other version info)"
    echo "   arch        Architecture (x86, sparc, etc.)"
    echo "   os          OS (linux, solaris8, etc.)"
    exit 1
}

while getopts "G:O:bgh?" arg
do
    case $arg in
	G) GROUP=$OPTARG ;;
	O) OWNER=$OPTARG ;;
	b) ENABLE_BZIP=-b ;;
	g) ENABLE_GZIP=-g  ;;
	h | \?) usage; exit 1 ;;
    esac
done
	
shift `expr $OPTIND - 1`

if [ $# -ne 4 ]; then
    usage
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"
VERSION=$2
ARCH=$3
OS=$4
ROOT=`dirname $0`

# If no compression options given, default to gzip
if [ -z "$ENABLE_GZIP" -a -z "$ENABLE_BZIP" ]; then
    ENABLE_GZIP="-g"
fi

OPTIONS="${GROUP:+ -G ${GROUP}} ${OWNER:+ -O ${OWNER}} $ENABLE_GZIP $ENABLE_BZIP"

$ROOT/make-main-dist.sh $OPTIONS $TARGET $VERSION $ARCH $OS || exit 1
$ROOT/make-extra-dist.sh $OPTIONS $TARGET $VERSION $ARCH $OS || exit 2
