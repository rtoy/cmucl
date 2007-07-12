#!/bin/sh -x

# $Id: linker.sh,v 1.2 2007/07/12 04:50:22 fgilham Exp $

# This file was written by Fred Gilham and is placed in the public domain.
# It comes without warranty of any kind.

PATH=/bin:/usr/bin:/usr/local/bin

GCC=`which gcc`

if [ -z "$GCC" ]
    then
    echo 'Cannot find GCC.  How did you build lisp?'
    exit 1
fi

set_libroot()
{
    LIBROOT=`$GCC -v 2>&1 | \
	grep 'Reading' | \
	sed -e 's/Reading specs from //' | \
	sed -e 's|/specs||'`

    echo "LIBROOT is $LIBROOT"
}


find_libroot()
{
    LIBROOT=''
    GCC_VERSION=`$GCC -v 2>&1 | \
     grep 'gcc version' | \
     sed -e 's/gcc version //' | \
     sed -e 's/ .*//'`
    if [ -z "$GCC_VERSION" ]; then
	echo "Can't get the version from GCC.  Something is very wrong."
	exit 1
    fi

    CRT_DIR=`find /usr/lib -name $GCC_VERSION -print`

    if [ -f $CRT_DIR/crtbegin.o ] && [ -f $CRT_DIR/crtend.o ]; then
	LIBROOT=$CRT_DIR
    else
	if [ -f /usr/lib/crtbegin.o ] && [ -f /usr/lib/crtend.o ]; then
	    # I doubt that this is right.  After all, this is Linux.  You
	    # don't expect to find things in reasonable places.
	    LIBROOT=/usr/lib
	fi
    fi

    if [ -z $LIBROOT ]; then
	echo "Can't find the directory with the C runtime object files.  Exiting."
	exit 1
    fi

    echo "LIBROOT is $LIBROOT"
}

if [ $# -ne 2 ]
    then
    echo "Usage: `basename $0` <initial function address> <executable file>"
    exit 1
fi

OPSYS=`uname`
VER=''

# Set OS-specific variables.
case "$OPSYS" in
    Linux )
	# See if this is a version of Linux where GCC reports
	# where it gets the specs from.  Extract the location of
	# the C runtime object files from that.
	set_libroot
	if [ -z "$LIBROOT" ]; then
	    # We have to do it the hard way.
	    find_libroot
	fi
	DLINKER='-dynamic-linker /lib/ld-linux.so.2'
	STARTCRT="$LIBROOT/../../../crt1.o $LIBROOT/../../../crti.o $LIBROOT/crtbegin.o"
	ENDCRT="$LIBROOT/crtend.o $LIBROOT/../../../crtn.o"
	LIBS="-L$LIBROOT -ldl -lm -lgcc -lc -lgcc"
	;;
    FreeBSD )
	LIBROOT=/usr/lib
	DLINKER='-dynamic-linker /usr/libexec/ld-elf.so.1'
	STARTCRT="$LIBROOT/crt1.o $LIBROOT/crti.o $LIBROOT/crtbegin.o"
	ENDCRT="$LIBROOT/crtend.o $LIBROOT/crtn.o"
	LIBS='-lm -lgcc -lc -lgcc'
	;;
    * )
	echo "$0: unknown operating system $OPSYS."
	exit 1
	;;
esac

OUTPUT="-o $2"
LINKER=/usr/bin/ld
CMUCLLIB=`dirname $0`
OBJS="--whole-archive $CMUCLLIB/lisp.a --no-whole-archive"
FLAGS='-export-dynamic'

SCRIPT="-T $CMUCLLIB/$OPSYS$VER-cmucl-linker-script"

# This is a hack.
# BIFLAG flags the executable as having a builtin lisp image.  It should be
# a valid address because it will be dereferenced.  It should also not point
# to an integer 0 because that would make the flag false. We use the first
# address in the process memory image, which should point to the ELF header.

# XXXX The process image start address can change depending on the OS
# (at least).
BIFLAG='--defsym builtin_image_flag=0x08048000'

# IFADDR is the initial function address, needed to start lisp processing.
IFADDR="--defsym initial_function_addr=$1"

$LINKER $SCRIPT $DLINKER $OUTPUT $STARTCRT $FLAGS $BIFLAG $IFADDR $OBJS $LIBS $ENDCRT

exit 0
