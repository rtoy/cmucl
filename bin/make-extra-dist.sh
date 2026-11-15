#!/bin/sh

usage() {
    cat <<EOF
`basename $0` -C option [-t tar] [-I destdir] [-G group] [-O owner]
  -h           This help
  -?           This help
  -t tar       Tar program to use
  -C option    Program for compressing the tarball: xz, bzip2, or gzip;
                 required.
  -I destdir   Install directly to given directory instead of creating a tarball
  -G group     Group to use
  -O owner     Owner to use

This is generally called by make-dist.sh and not normally invoked by the user

Create a tarball of the extra components for cmucl.  This includes things like 
CLX; Hemlock; CLM; contrib library not already included in the main
distribution; locale messages.
EOF
    exit 1
}

GTAR=tar
while getopts "C:G:O:I:t:h?" arg
do
    case $arg in
	C) COMPRESS=$OPTARG ;;
	G) GROUP="-g $OPTARG" ;;
	O) OWNER="-o $OPTARG" ;;
        I) INSTALL_DIR=$OPTARG ;;
	t) GTAR=$OPTARG ;;
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

if [ "$1" = "" -o "$2" = "" -o "$3" = "" -o "$4" = "" ]
then
    usage
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

DESTDIR=${INSTALL_DIR:-release-$$}
TARGET="`echo $1 | sed 's:/*$::'`"
VERSION=$2
ARCH=$3
OS=$4

CMUCLLIBVER="lib/cmucl/$VERSION"

case $ARCH in
	x86*)		FASL="sse2f" ;;
	sparc*)		FASL=sparcf ;;
	alpha*)		FASL=axpf ;;
	ppc*)		FASL=ppcf ;;
	mips*)		FASL=sgif ;;
	*)
		echo "Unknown FASL type for architecture $ARCH"
		exit 2
		;;
esac

# Frob PATH to use /usr/ucb/install for Solaris
if [ `uname -s` = "SunOS" ]
then
	PATH=/usr/ucb:$PATH
fi

if [ -z "$INSTALL_DIR" ]; then
    echo Cleaning $DESTDIR
    [ -d $DESTDIR ] && rm -rf $DESTDIR
fi

echo Installing extra components
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib

install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib/subsystems

install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib/contrib

for ext in $FASL
do
  install ${GROUP} ${OWNER} -m 0644 $TARGET/clx/clx-library.$ext \
      $DESTDIR/$CMUCLLIBVER/lib/subsystems/
  install ${GROUP} ${OWNER} -m 0644 $TARGET/hemlock/hemlock-library.$ext \
      $DESTDIR/$CMUCLLIBVER/lib/subsystems/
  install ${GROUP} ${OWNER} -m 0644 $TARGET/interface/clm-library.$ext  \
      $DESTDIR/$CMUCLLIBVER/lib/subsystems/
done

install ${GROUP} ${OWNER} -m 0644 src/hemlock/XKeysymDB \
	src/hemlock/hemlock11.cursor src/hemlock/hemlock11.mask \
	$TARGET/hemlock/spell-dictionary.bin \
	$DESTDIR/$CMUCLLIBVER/lib/
install ${GROUP} ${OWNER} -m 0755 src/hemlock/mh-scan $DESTDIR/$CMUCLLIBVER/lib/
install ${GROUP} ${OWNER} -m 0755 $TARGET/motif/server/motifd \
	$DESTDIR/$CMUCLLIBVER/lib/

# Install the contrib stuff.  Create the directories and then copy the files.
#
# asdf, defsystem and unix are part of the main distribution, so skip
# these directories.
for d in `(cd src; find contrib -type d -print | grep -v asdf | grep -v defsystem)`
do
    install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib/$d
done

for f in `(cd src/contrib; find . -type f -print | grep -v asdf | grep -v defsystem | grep -v unix)`
do
    DIR=`dirname $f`
    install ${GROUP} ${OWNER} -m 0644 src/contrib/$f $DESTDIR/$CMUCLLIBVER/lib/contrib/$DIR
done

# Install all the locale data.

for d in `(cd src/i18n/; find locale -type d -print)`
do
    install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib/$d
done

# Install mo files.  Ignore any emacs-style backup files.
for f in `(cd $TARGET/i18n; find locale -type f -print | grep -v '~')`
do
    DIR=`dirname $f`
    install ${GROUP} ${OWNER} -m 0644 $TARGET/i18n/$f $DESTDIR/$CMUCLLIBVER/lib/$DIR
done

if [ -z "$INSTALL_DIR" ]; then
    sync ; sleep 1 ; sync ; sleep 1 ; sync
    echo Tarring extra components
    (cd $DESTDIR && $GTAR cf - lib) | $COMPRESS > cmucl-$VERSION-$ARCH-$OS.extra.tar.$COMPRESS_EXT

    echo Cleaning $DESTDIR
    [ -d $DESTDIR ] && rm -rf $DESTDIR
    echo Done
fi
