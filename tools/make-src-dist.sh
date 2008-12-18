#!/bin/sh

while getopts "bgh?t:" arg
do
    case $arg in
	b) ENABLE_BZIP=-b ;;
	g) ENABLE_GZIP=-g  ;;
        t) GTAR=$OPTARG ;;
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

echo "  Compressing with $ZIP"
${GTAR:-tar} --exclude=CVS -cf - src | ${ZIP} > cmucl-src-$VERSION.tar.$ZIPEXT
