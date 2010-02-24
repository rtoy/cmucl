#!/bin/sh

while getopts "bgh?t:I:" arg
do
    case $arg in
	b) ENABLE_BZIP=-b ;;
	g) ENABLE_GZIP=-g  ;;
        t) GTAR=$OPTARG ;;
        I) INSTALL_DIR=$OPTARG ;;
	h | \?) usage; exit 1 ;;
    esac
done

shift `expr $OPTIND - 1`

VERSION=$1

echo Creating source distribution
if [ -n "$ENABLE_GZIP" ]; then
    ZIP="gzip -c"
    ZIPEXT="gz"
fi
if [ -n "$ENABLE_BZIP" ]; then
    ZIP="bzip2"
    ZIPEXT="bz2"
fi

if [ -z "$INSTALL_DIR" ]; then
    echo "  Compressing with $ZIP"
    ${GTAR:-tar} --exclude=CVS -cf - src | ${ZIP} > cmucl-src-$VERSION.tar.$ZIPEXT
else
    # Install in the specified directory
    ${GTAR:-tar} --exclude=CVS -cf - src | (cd $INSTALL_DIR; ${GTAR:-tar} xf -)
fi
