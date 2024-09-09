#!/bin/sh

usage() {
    cat <<EOF
`basename $0` -C option -E ext [-h?] [-t gnutar] [-I destdir] [version]
  -h           This help
  -?           This help
  -t tar       Name/path to GNU tar
  -C option    Tar option for compressing the tarball; required.
  -E ext       Extension to use for the tarball.  Must be consistent with
                 -C option.  Required.
  -I destdir   Install directly to given directory instead of creating a tarball
   version     The version.  Defaults to the current date

This is generally called by make-dist.sh and not normally invoked by the user

Create a tar ball of the cmucl sources."
EOF
    exit 1
}

GTAR=tar

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
    ${GTAR} ${GTAR_OPTIONS} ${COMPRESS} -cf cmucl-src-$VERSION.tar.$COMPRESS_EXT bin src tests
else
    # Install in the specified directory
    ${GTAR} ${GTAR_OPTIONS} -cf - bin src tests | (cd $INSTALL_DIR; ${GTAR:-tar} xf -)
fi
