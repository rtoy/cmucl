#!/bin/sh

usage() {
    echo "make-src-dist.sh: -C option -E ext [-h?] [-t gnutar] [-I destdir] [version]"
    echo "  -h           This help"
    echo "  -?           This help"
    echo "  -t tar       Name/path to GNU tar"
    echo "  -C option    Tar option for compressing the tarball; required."
    echo "  -E ext       Extension to use for the tarball.  Must be consistent with"
    echo "                 -C option.  Required."
    echo "  -I destdir   Install directly to given directory instead of creating a tarball"
    echo "   version     The version.  Defaults to the current date"
    echo ""
    echo "This is generally called by make-dist.sh and not normally invoked by the user"
    echo ""
    echo "Create a tar ball of the cmucl sources."
}

while getopts "C:E:h?t:I:" arg
do
    case $arg in
	C) COMPRESS=$OPTARG ;;
	E) COMPRESS_EXT=$OPTARG ;;
        t) GTAR=$OPTARG ;;
        I) INSTALL_DIR=$OPTARG ;;
	h | \?) usage; exit 1 ;;
    esac
done

shift `expr $OPTIND - 1`

# -C and -E options are required
if [ -z "$COMPRESS" ]; then
    echo "-C option is required"
    exit 2
fi

if [ -z "$COMPRESS_EXT" ]; then
    echo "-E option is required"
    exit 2
fi

# If no version is given, default to today's date
if [ -n "$1" ]; then
    VERSION=$1
else
    VERSION="`date '+%Y-%m-%d-%H:%M:%S'`"
fi

echo Creating source distribution
GTAR_OPTIONS="--exclude=.git --exclude='*.pot.~*~'"
if [ -z "$INSTALL_DIR" ]; then
    # echo "  Compressing with $ZIP"
    ${GTAR:-tar} ${GTAR_OPTIONS} ${COMPRESS} -cf cmucl-src-$VERSION.tar.$COMPRESS_EXT bin src tests
else
    # Install in the specified directory
    ${GTAR:-tar} ${GTAR_OPTIONS} -cf - bin src tests | (cd $INSTALL_DIR; ${GTAR:-tar} xf -)
fi
