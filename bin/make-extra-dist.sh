#!/bin/sh

usage() {
    cat <<EOF
`basename $0` -C option -E ext [-t tar] [-I destdir] [-G group] [-O owner]
  -h           This help
  -?           This help
  -t tar       Tar program to use
  -C option    Tar option for compressing the tarball; required.
  -E ext       Extension to use for the tarball.  Must be consistent with
                 -C option.  Required.
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
while getopts "C:E:G:O:I:t:h?" arg
do
    case $arg in
	C) COMPRESS=$OPTARG ;;
	E) COMPRESS_EXT=$OPTARG ;;
	G) GROUP="-g $OPTARG" ;;
	O) OWNER="-o $OPTARG" ;;
        I) INSTALL_DIR=$OPTARG ;;
	t) GTAR=$OPTARG ;;
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
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib

install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib/subsystems

install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib/contrib

for ext in $FASL
do
  install ${GROUP} ${OWNER} -m 0644 $TARGET/clx/clx-library.$ext \
      $DESTDIR/lib/cmucl/lib/subsystems/
  install ${GROUP} ${OWNER} -m 0644 $TARGET/hemlock/hemlock-library.$ext \
      $DESTDIR/lib/cmucl/lib/subsystems/
  install ${GROUP} ${OWNER} -m 0644 $TARGET/interface/clm-library.$ext  \
      $DESTDIR/lib/cmucl/lib/subsystems/
done

# Not sure we really need these, but we'll install them in the
# ext-formats directory.  (Should they go somewhere else?)
#install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib/ext-formats
#for f in src/i18n/NameAliases.txt src/i18n/UnicodeData.txt
#do
#    echo $f
#    install ${GROUP} ${OWNER} -m 0644 $f $DESTDIR/lib/cmucl/lib/ext-formats/
#done

# install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib/fonts/
# install ${GROUP} ${OWNER} -m 0644 misc/8x13u.snf misc/fonts.dir \
#	$DESTDIR/lib/cmucl/lib/fonts/
install ${GROUP} ${OWNER} -m 0644 src/hemlock/XKeysymDB \
	src/hemlock/hemlock11.cursor src/hemlock/hemlock11.mask \
	$TARGET/hemlock/spell-dictionary.bin \
	$DESTDIR/lib/cmucl/lib/
install ${GROUP} ${OWNER} -m 0755 src/hemlock/mh-scan $DESTDIR/lib/cmucl/lib/
install ${GROUP} ${OWNER} -m 0755 $TARGET/motif/server/motifd \
	$DESTDIR/lib/cmucl/lib/

# Install the contrib stuff.  Create the directories and then copy the files.
#
# asdf, defsystem and unix are part of the main distribution, so skip
# these directories.
for d in `(cd src; find contrib -type d -print | grep -v "asdf\|defsystem")`
do
    install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib/$d
done

for f in `(cd src/contrib; find . -type f -print | grep -v "asdf\|defsystem\|unix")`
do
    FILE=`basename $f`
    DIR=`dirname $f`
    install ${GROUP} ${OWNER} -m 0644 src/contrib/$f $DESTDIR/lib/cmucl/lib/contrib/$DIR
done

# Install all the locale data.

for d in `(cd src/i18n/; find locale -type d -print)`
do
    install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib/$d
done

# Install mo files.  Ignore any emacs-style backup files.
for f in `(cd $TARGET/i18n; find locale -type f -print | grep -v '~.*~\|.*~')`
do
    FILE=`basename $f`
    DIR=`dirname $f`
    install ${GROUP} ${OWNER} -m 0644 $TARGET/i18n/$f $DESTDIR/lib/cmucl/lib/$DIR
done

# Install po files.  (Do we really need to distribute the po files?)
#for f in `(cd $TARGET/i18n; find locale -type f -print | grep -v '~.*~\|.*~')`
#do
#    FILE=`basename $f`
#    DIR=`dirname $f`
#    install ${GROUP} ${OWNER} -m 0644 $TARGET/i18n/$f $DESTDIR/lib/cmucl/lib/$DIR
#done

if [ -z "$INSTALL_DIR" ]; then
    sync ; sleep 1 ; sync ; sleep 1 ; sync
    echo Tarring extra components
    $GTAR -C $DESTDIR $COMPRESS -cf cmucl-$VERSION-$ARCH-$OS.extra.tar.$COMPRESS_EXT lib

    echo Cleaning $DESTDIR
    [ -d $DESTDIR ] && rm -rf $DESTDIR
    echo Done
fi
