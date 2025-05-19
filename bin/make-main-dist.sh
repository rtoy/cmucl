#!/bin/sh

usage() {
    cat <<EOF
`basename $0`  -C option -E ext [-h?] [-t tar][-I destdir] [-G group] [-O owner] [-M mandir]
        target-directory version arch os
  -h           This help
  -?           This help
  -t tar       Tar program to use
  -C option    Tar option for compressing the tarball; required.
  -E ext       Extension to use for the tarball.  Must be consistent with
                 -C option.  Required.
  -I destdir   Install directly to given directory instead of creating a tarball
  -G group     Group to use
  -O owner     Owner to use
  -M mandir    Install manpages in this subdirectory.  Default is man/man1

This is generally called by make-dist.sh and not normally invoked by the user

Create a tarball consisting of the main components needed to distribute
a binary installation of cmucl.  This includes the C executable and support
libraries; the subsystems like Gray streams, and simple streams; external
formats; contribs like asdf and defsystem; manpages and READMEs."
EOF
    exit 1
}
    
GTAR=tar
while getopts "C:E:G:O:I:M:t:h?" arg
do
    case $arg in
	C) COMPRESS=$OPTARG ;;
	E) COMPRESS_EXT=$OPTARG ;;
	G) GROUP="-g $OPTARG" ;;
	O) OWNER="-o $OPTARG" ;;
        I) INSTALL_DIR=$OPTARG ;;
        M) MANDIR=$OPTARG ;;
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

VERSION=$2
ARCH=$3
OS=$4

# Where to install the main library of cmucl files
CMUCLLIBVER="lib/cmucl/$VERSION"

# Where to install everything
DESTDIR=${INSTALL_DIR:-release-$$}

# Where to install docs
DOCDIR=${DOCDIR:-share/cmucl/$VERSION/doc}

# Where to install man pages
MANDIR=${MANDIR:-share/man/man1}

TARGET="`echo $1 | sed 's:/*$::'`"

# Core file to look for.
CORE=lisp.core
case $ARCH in
	x86*)		FASL=sse2f
                        CORE="lisp-sse2.core" ;;
	sparc*)		FASL=sparcf ;;
	alpha*)		FASL=axpf ;;
	ppc*)		FASL=ppcf ;;
	mips*)		FASL=sgif ;;
	*)
		echo "Unknown FASL type for architecture $ARCH"
		exit 2
		;;
esac

case $OS in
  freebsd*)	
      EXECUTABLE=true
      SCRIPT=FreeBSD
      ;;
  linux*)		
      EXECUTABLE=true
      SCRIPT=Linux
      ;;
  solaris*)		
      EXECUTABLE=true
      SCRIPT=SunOS
      ;;
  darwin*)
      EXECUTABLE=true
      ;;
  *)
      EXECUTABLE=""
      ;;
esac

# Frob PATH to use /usr/ucb/install for Solaris
if [ `uname -s` = "SunOS" ]
then
	PATH=/usr/ucb:$PATH
fi

if [ -z "$INSTALL_DIR" ]; then
    # Clean out DESTDIR, if we're not installing there.
    echo Cleaning $DESTDIR
    [ -d $DESTDIR ] && rm -rf $DESTDIR
fi

# set -x
echo Installing main components
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/bin
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib/subsystems
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib/ext-formats
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/${DOCDIR}
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/${MANDIR}
install ${GROUP} ${OWNER} -m 0755 $TARGET/lisp/lisp $DESTDIR/bin/lisp-$VERSION
# Install symlink for lisp
(cd $DESTDIR/bin; ln -fs lisp-$VERSION lisp)
# Install symlink for man pages
(cd $DESTDIR/${MANDIR}
 ln -fs lisp-$VERSION.1 lisp.1
 ln -fs cmucl-$VERSION.1 cmucl.1)

if [ "$EXECUTABLE" = "true" ]
then
    install ${GROUP} ${OWNER} -m 0644 $TARGET/lisp/lisp.a $DESTDIR/$CMUCLLIBVER/lib/
    install ${GROUP} ${OWNER} -m 0644 $TARGET/lisp/exec-init.o $DESTDIR/$CMUCLLIBVER/lib/
    install ${GROUP} ${OWNER} -m 0644 $TARGET/lisp/exec-final.o $DESTDIR/$CMUCLLIBVER/lib/
    install ${GROUP} ${OWNER} -m 0755 src/tools/linker.sh $DESTDIR/$CMUCLLIBVER/lib/
    if [ -f src/tools/$SCRIPT-cmucl-linker-script ]; then
	install ${GROUP} ${OWNER} -m 0755 src/tools/$SCRIPT-cmucl-linker-script $DESTDIR/$CMUCLLIBVER/lib/
    fi
fi
for corefile in $TARGET/lisp/$CORE
do
  install ${GROUP} ${OWNER} -m 0644 $corefile $DESTDIR/$CMUCLLIBVER/lib/
done
install ${GROUP} ${OWNER} -m 0755 src/tools/load-foreign.csh src/tools/config \
	$DESTDIR/$CMUCLLIBVER/lib/
install ${GROUP} ${OWNER} -m 0644 src/tools/config.lisp \
	$DESTDIR/$CMUCLLIBVER/lib/
install ${GROUP} ${OWNER} -m 0644 src/code/default-site-init.lisp \
	$DESTDIR/$CMUCLLIBVER/lib/
install ${GROUP} ${OWNER} -m 0644 $TARGET/lisp/lisp.nm $TARGET/lisp/lisp.map \
	$TARGET/lisp/internals.h $TARGET/lisp/internals.inc $DESTDIR/$CMUCLLIBVER/
install ${GROUP} ${OWNER} -m 0755 src/tools/sample-wrapper $DESTDIR/$CMUCLLIBVER/

for f in gray-streams gray-compat simple-streams iodefs
do
    install ${GROUP} ${OWNER} -m 0644 $TARGET/pcl/$f-library.$FASL $DESTDIR/$CMUCLLIBVER/lib/subsystems/
done

for f in src/pcl/simple-streams/external-formats/*.lisp src/pcl/simple-streams/external-formats/aliases src/i18n/unidata.bin
do
    install ${GROUP} ${OWNER} -m 0644 $f $DESTDIR/$CMUCLLIBVER/lib/ext-formats/
done

# set -x
# Create the directories for asdf and defsystem
for f in asdf defsystem asdf/doc defsystem/docs
do
    install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib/contrib/$f
done

case `uname -s` in
  Linux*) UCONTRIB="unix-glibc2" ;;
  *) UCONTRIB="unix" ;;
esac

install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/$CMUCLLIBVER/lib/contrib/unix
install ${GROUP} ${OWNER} -m 0644 $TARGET/contrib/unix/$UCONTRIB.$FASL $DESTDIR/$CMUCLLIBVER/lib/contrib/unix
install ${GROUP} ${OWNER} -m 0644 src/contrib/load-unix.lisp $DESTDIR/$CMUCLLIBVER/lib/contrib
install ${GROUP} ${OWNER} -m 0644 src/contrib/unix/${UCONTRIB}.lisp $DESTDIR/$CMUCLLIBVER/lib/contrib/unix

# Copy the source files for asdf and defsystem
for f in `(cd src; find contrib/asdf contrib/defsystem -type f -print | grep -v CVS)`
do
    install ${GROUP} ${OWNER} -m 0644 src/$f $DESTDIR/$CMUCLLIBVER/lib/$f
done

# Install the fasl files for asdf and defsystem
for f in asdf defsystem
do
    install ${GROUP} ${OWNER} -m 0644 $TARGET/contrib/$f/$f.$FASL $DESTDIR/$CMUCLLIBVER/lib/contrib/$f
done

install ${GROUP} ${OWNER} -m 0644 src/general-info/cmucl.1 \
	$DESTDIR/${MANDIR}/cmucl-$VERSION.1
install ${GROUP} ${OWNER} -m 0644 src/general-info/lisp.1 \
	$DESTDIR/${MANDIR}/lisp-$VERSION.1
install ${GROUP} ${OWNER} -m 0644 src/general-info/README $DESTDIR/${DOCDIR}
if [ -f src/general-info/release-$VERSION.txt ] 
then
	install ${GROUP} ${OWNER} -m 0644 src/general-info/release-$VERSION.txt \
		$DESTDIR/${DOCDIR}
fi

if [ -z "$INSTALL_DIR" ]; then
    sync ; sleep 1 ; sync ; sleep 1 ; sync
    echo Tarring main components
    $GTAR -C $DESTDIR $COMPRESS -cf cmucl-$VERSION-$ARCH-$OS.tar.$COMPRESS_EXT .

    echo Cleaning $DESTDIR
    [ -d $DESTDIR ] && rm -rf $DESTDIR
    echo Done
fi
