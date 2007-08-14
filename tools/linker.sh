#!/bin/sh -x

# $Id: linker.sh,v 1.7 2007/08/14 15:57:48 rtoy Exp $

# This file was written by Fred Gilham and is placed in the public domain.
# It comes without warranty of any kind.

if [ $# -ne 3 ]
    then
    echo "Usage: `basename $0` <c-compiler> <initial function address> <executable file>"
    exit 1
fi

CCOMPILER=$1
shift;

if [ $CCOMPILER = "cc" ]; then
    # Sun C compiler

    # Can't set PATH because we don't really know where the compiler
    # is.  The user has to have it in his path.

    CC=`which cc`
    if [ -z "$CC" ]; then
	echo 'Cannot find Sun C.  Is it available and in $PATH?'
	exit 1
    fi
    CRTPATH=`dirname $CC`/../prod
    LIBROOT=$CRTPATH/lib

else
    # Gcc
    PATH=/bin:/usr/bin:/usr/local/bin

    GCC=`which gcc`

    if [ -z "$GCC" ]; then
	echo 'Cannot find GCC.  How did you build lisp?'
	exit 1
    fi

    # Uniform method for finding GCC C runtime object files suggested by Ray Toy
    CRTPATH=`$GCC -print-libgcc-file-name`
    LIBROOT=`dirname $CRTPATH`
fi

echo "LIBROOT is $LIBROOT"

OPSYS=`uname`
VER=''

# Default values
OUTPUT="-o $2"
LINKER=/usr/bin/ld
CMUCLLIB=`dirname $0`
OBJS="--whole-archive $CMUCLLIB/lisp.a --no-whole-archive"
FLAGS='-export-dynamic'

SCRIPT="-T $CMUCLLIB/$OPSYS$VER-cmucl-linker-script"

# This is a hack.
# These are the default values.
#
# BIFLAG flags the executable as having a builtin lisp image.  It should be
# a valid address because it will be dereferenced.  It should also not point
# to an integer 0 because that would make the flag false. We use the first
# address in the process memory image, which should point to the ELF header.

# XXXX The process image start address can change depending on the OS
# (at least).
BIFLAG='--defsym builtin_image_flag=0x08048000'

# IFADDR is the initial function address, needed to start lisp processing.
IFADDR="--defsym initial_function_addr=$1"

# Set OS-specific variables.
case "$OPSYS" in
    Linux )
	# Handle x86_64 version system.  This should be revisited when there's a 64-bit version
	# of CMUCL on Linux.  Note that -m32 won't work in older versions of GCC which we may
	# still want to support.
	ARCH=`uname -m`
	if [ "$ARCH" = "x86_64" ]
	then
	    CRTPATH=`$GCC -m32 -print-libgcc-file-name`
	    LIBROOT=`dirname $CRTPATH`
	fi
	STARTCRT="/usr/lib/crt1.o /usr/lib/crti.o $LIBROOT/crtbegin.o"
	ENDCRT="$LIBROOT/crtend.o /usr/lib/crtn.o"
	DLINKER='-dynamic-linker /lib/ld-linux.so.2'
	LIBS="-L$LIBROOT -ldl -lm -lgcc -lc -lgcc"
	;;
    FreeBSD )
	DLINKER='-dynamic-linker /usr/libexec/ld-elf.so.1'
	STARTCRT="$LIBROOT/crt1.o $LIBROOT/crti.o $LIBROOT/crtbegin.o"
	ENDCRT="$LIBROOT/crtend.o $LIBROOT/crtn.o"
	LIBS='-lm -lgcc -lc -lgcc'
	;;
    SunOS )
	if [ $CCOMPILER = "cc" ]; then
	    # These values were obtained by running cc -# hello.c and
	    # looking at the linker command.
	    STARTCRT="$LIBROOT/crti.o $LIBROOT/crt1.o $LIBROOT/misalign.o $LIBROOT/values-xa.o"
	    ENDCRT="$LIBROOT/crtn.o"
	    LIBS="-Y P,$LIBROOT/v8plus:$LIBROOT:/usr/ccs/lib:/lib:/usr/lib -Qy -lm -lc -lsocket -lnsl -ldl"
	else
	    STARTCRT="$LIBROOT/crt1.o $LIBROOT/crti.o $LIBROOT/crtbegin.o"
	    ENDCRT="$LIBROOT/crtend.o $LIBROOT/crtn.o"
	    LIBS="-L$LIBROOT -lm -lgcc -lc -lgcc -lsocket -lnsl -ldl"
	fi
	LINKER="/usr/ccs/bin/ld"
	OBJS="-z allextract $CMUCLLIB/lisp.a CORRO.o CORSTA.o CORDYN.o -z defaultextract"
	SCRIPT="$CMUCLLIB/$OPSYS$VER-cmucl-linker-script"
	# Is this right?  I just made this point to &main, and the
	# first word of main isn't all zeros.
	BIFLAG=0x14120
	IFADDR=
	sed -e "s;@BIFLAG@;$BIFLAG;" -e "s;@IFADDR@;$1;" $SCRIPT > sunos-map-file
	SCRIPT="-M sunos-map-file"
	# Remove the sunos-map-file when the script exits.
	trap 'rm -f sunos-map-file' 0
	FLAGS=
	BIFLAG=
	;;
    * )
	echo "$0: unknown operating system $OPSYS."
	exit 1
	;;
esac

$LINKER $SCRIPT $DLINKER $OUTPUT $STARTCRT $FLAGS $BIFLAG $IFADDR $OBJS $LIBS $ENDCRT

exit 0
