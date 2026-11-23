#!/bin/sh

usage() {
    cat <<EOF
`basename $0` -C option [-h?] [-t tar] [-I destdir] [version]
  -h           This help
  -?           This help
  -t tar       Tar program to use
  -C option    Program for compressing the tarball: xz, bzip2, or gzip;
                 required.
  -I destdir   Install directly to given directory instead of creating a tarball
   version     The version.  Defaults to the current date

This is generally called by make-dist.sh and not normally invoked by the user

Create a tar ball of the cmucl sources."
EOF
    exit 1
}

GTAR=tar

while getopts "C:h?t:I:" arg
do
    case $arg in
	C) COMPRESS=$OPTARG ;;
        t) GTAR=$OPTARG ;;
        I) INSTALL_DIR=$OPTARG ;;
	h | \?) usage; exit 1 ;;
    esac
done

shift `expr $OPTIND - 1`

# -C option is required and determines the extension
case $COMPRESS in
    xz) COMPRESS_EXT=xz ;;
    bzip2) COMPRESS_EXT=bz2 ;;
    gzip) COMPRESS_EXT=gz ;;
    "") echo "-C option is required"
	exit 2 ;;
    *) echo '-C option "'$COMPRESS'" must be one of bzip2, xz or gzip'
       exit 2 ;;
esac

# If no version is given, default to today's date
if [ -n "$1" ]; then
    VERSION=$1
else
    VERSION="`date '+%Y-%m-%d-%H:%M:%S'`"
fi

DESTDIR=${INSTALL_DIR:-release-$$}

echo Creating source distribution
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/share/cmucl/$VERSION/
install ${GROUP} ${OWNER} -m 0755 bin/run-unit-tests.sh $DESTDIR/bin
${GTAR} cf - src tests | (cd $DESTDIR/share/cmucl/$VERSION; ${GTAR} xf -)
find $DESTDIR/share/cmucl/$VERSION \( -name .git -o -name '*.pot.~*~' \) -prune -exec rm -rf {} +
if [ -z "$INSTALL_DIR" ]; then
    # echo "  Compressing with $ZIP"
    ls $DESTDIR/share/cmucl/$VERSION/
    (cd $DESTDIR && ${GTAR} cf - share/cmucl/$VERSION/src) | ${COMPRESS} > cmucl-src-$VERSION.tar.$COMPRESS_EXT
fi
