#!/bin/sh -x

# $Id: linker.sh,v 1.1 2007/07/07 16:00:49 fgilham Exp $

# This file was written by Fred Gilham and is placed in the public domain.
# It comes without warranty of any kind.

if [ $# -ne 2 ]
    then
    echo "Usage: `basename $0` <initial function address> <executable file>"
    exit 1
fi

OPSYS=`uname`
VER=""

# Set OS-specific variables.  Things marked XXXX can possibly change
# with different operationg systems, different distributions (Linux)
# or different releases (FreeBSD).
case "$OPSYS" in
    Linux )
	# XXXX  This can change depending on any number of things....
	LIBROOT=/usr/lib/gcc-lib/i386-redhat-linux/2.96
	DLINKER="-dynamic-linker /lib/ld-linux.so.2"
	STARTCRT="$LIBROOT/../../../crt1.o $LIBROOT/../../../crti.o $LIBROOT/crtbegin.o"
	ENDCRT="$LIBROOT/crtend.o $LIBROOT/../../../crtn.o"
	LIBS="-L$LIBROOT -ldl -lm -lgcc -lc -lgcc"
	;;
    FreeBSD )
	# XXXX I guess we have to just keep adding to this.
	# XXXXX Maybe not.  Seems like there's more compatibility than I
	# thought between binary versions.
	RELEASE=`uname -r`
#	case $RELEASE in
#	    4* ) VER=4 ;;
#	    5* ) VER=5 ;;
#	    *  ) echo "$0: unknown FreeBSD version $RELEASE" ; exit 1 ;;
#	esac
	LIBROOT=/usr/lib
	DLINKER="-dynamic-linker /usr/libexec/ld-elf.so.1"
	STARTCRT="$LIBROOT/crt1.o $LIBROOT/crti.o $LIBROOT/crtbegin.o"
	ENDCRT="$LIBROOT/crtend.o $LIBROOT/crtn.o"
	LIBS="-lm -lgcc -lc -lgcc"
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
FLAGS="-export-dynamic"

# XXXX The linker script is a nasty portability issue. 
SCRIPT="-T $CMUCLLIB/$OPSYS$VER-cmucl-linker-script"

# BIFLAG flags the executable as having a builtin lisp image.  It
# should be a valid address because it will be dereferenced.  It
# should also not point to an integer 0. We use the first address in
# the process memory image, which should point to the ELF header.

# XXXX The process image start address can change depending on the OS
# (at least).
BIFLAG="--defsym builtin_image_flag=0x08048000"

# IFADDR is the initial function address, needed to start lisp processing.
IFADDR="--defsym initial_function_addr=$1"

$LINKER $SCRIPT $DLINKER $OUTPUT $STARTCRT $FLAGS $BIFLAG $IFADDR $OBJS $LIBS $ENDCRT
