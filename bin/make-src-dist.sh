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

DESTDIR=${INSTALL_DIR:-release-$$}

echo Creating source distribution
set -x
GTAR_OPTIONS="--exclude=.git --exclude='*.pot.~*~'"
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/share/cmucl/$VERSION/
install ${GROUP} ${OWNER} -m 0755 bin/run-unit-tests.sh $DESTDIR/bin
${GTAR} ${GTAR_OPTIONS} -cf - src tests | (cd $DESTDIR/share/cmucl/$VERSION; ${GTAR} xf -)
if [ -z "$INSTALL_DIR" ]; then
    # echo "  Compressing with $ZIP"
    ls $DESTDIR/share/cmucl/$VERSION/
    ${GTAR} ${GTAR_OPTIONS} ${COMPRESS} -C $DESTDIR -cf cmucl-src-$VERSION.tar.$COMPRESS_EXT share/cmucl/$VERSION/src
else
    # Install in the specified directory
    ${GTAR} ${GTAR_OPTIONS} -cf - src tests | (cd $DESTDIR/share/cmucl/$VERSION; ${GTAR:-tar} xf -)
fi
