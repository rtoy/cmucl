#!/bin/sh

# $Id: linker.sh,v 1.16 2010/12/23 03:16:56 rtoy Exp $

# This file written by Raymond Toy as part of CMU Common Lisp and is
# placed in the public domain.
#
# This script takes parameters specified by the running lisp to create
# an executable image.
#
# Despite the name, it is used for Linux/x86, Darwin/x86, and
# Solaris/sparc, as specified in src/lisp/elf.h.

OPSYS=`uname`

if [ "X$CMU_DEBUG_LINKER" != "X" ]; then
    # Enable debugging if CMU_DEBUG_LINKER is defined and not empty.
    set -x
fi

# There must be exactly 6 parameters:
# - the name of the C compiler (sparc supports both Sun cc and GNU gcc).
# - the address of the initial function (in C hex format)
# - the path of the executable to be created
# - the address of the start of the read-only space
# - the address of the start of the static space
# - the address of the start of the dynamic space
if [ $# -ne 6 ]; then
    echo "Usage: `basename $0` <c-compiler> <initial-func-addr> <executable> <ro-addr> <static-addr> <dyn-addr>"
    exit 1
fi

CCOMPILER=$1
IFADDR=$2
EXEC=$3

# Figure out the directory and file name of the executable.
OUTDIR=`dirname $EXEC`
OUTNAME=`basename $EXEC`

# This tells us where the cmu lisp executable is and also the
# locations of lisp.a.
CMUCLLIB=`dirname $0`

# Name of file where we write the actual initial function address.
OPT_IFADDR="cmu-ifaddr-$$.c"
# Names of the core sections from Lisp.
OPT_CORE="CORRO.o CORSTA.o CORDYN.o"

uname_s=`uname`
case $uname_s in
  Linux|FreeBSD|NetBSD)
      # How to specify the starting address for each of the sections
      # These aren't needed for Linux any more.  map_core_sections
      # takes care of getting the addresses.

      #RO_ADDR="-Wl,--section-start=CORRO=$4"
      #STATIC_ADDR="-Wl,--section-start=CORSTA=$5"
      #DYN_ADDR="-Wl,--section-start=CORDYN=$6"

      #OPT_IF ADDR="-Wl,--defsym -Wl,initial_function_addr=$IFADDR"

      # Specify how to link the entire lisp.a library
      OPT_ARCHIVE="-Wl,--whole-archive -Wl,$CMUCLLIB/lisp.a -Wl,--no-whole-archive"

      # Extra stuff.

      OPT_EXTRA="-rdynamic"

      # See Config.x86_${uname_s}
      case $uname_s in
	Linux) OS_LIBS=-ldl;;
	FreeBSD) OS_LIBS=-lutil;;
      esac
      ;;
  Darwin)
      # How to specify the starting address for each of the sections.
      # We don't actually need these because map_core_sections sets
      # the addresses itself instead of from the segment address, but
      # if we don't set them up correctly, vmmap complains when run on
      # the resulting executable.  There's no harm in specifying them
      # here, though; the addresses are ignored by map_core_sections.
      RO_ADDR="-segaddr CORRO $4"
      STATIC_ADDR="-segaddr CORSTA $5"
      DYN_ADDR="-segaddr CORDYN $6"

      # Specify how to link the entire lisp.a library
      OPT_ARCHIVE="-all_load $CMUCLLIB/lisp.a"

      # Extra stuff.  For some reason one __LINKEDIT segment is mapped
      # just past the dynamic space.  This messes things up, so we move it
      # to another address.  This seems to be free, at least on 10.5.

      OPT_EXTRA="-segaddr __LINKEDIT 0x99000000 -rdynamic"
      # See Config.x86_darwin
      OS_LIBS=
      ;;
  SunOS)
      # A quick test indicates that gcc will accept the following
      # options too, so this will work whether we have Sun C or gcc.
      # Note, that this probably only works if gcc uses Sun ld and not
      # GNU ld.  Most (all?) prebuilt versions of gcc for Solaris use
      # Sun ld.

      # We don't need anything special to set the starting address.
      # map_core_sections does that for us on sparc.

      # Specify how to link the entire lisp.a library
      OPT_ARCHIVE="-Xlinker -z -Xlinker allextract -Xlinker $CMUCLLIB/lisp.a -Xlinker -z -Xlinker defaultextract"

      # Extra stuff.

      OPT_EXTRA="-Bdynamic"

      # See Config.sparc_sunc
      OS_LIBS="-lsocket -lnsl -ldl"
      ;;

esac

# Remove the C file and core section files when we're done.
trap 'rm -f $OUTDIR/$OPT_IFADDR $OUTDIR/CORRO.o $OUTDIR/CORSTA.o $OUTDIR/CORDYN.o' 0

(cd $OUTDIR
echo "long initial_function_addr = $IFADDR;" > $OPT_IFADDR
$CCOMPILER -m32 -o $OUTNAME $OPT_IFADDR $OPT_ARCHIVE $OPT_CORE $RO_ADDR $STATIC_ADDR $DYN_ADDR $OPT_EXTRA $OS_LIBS -lm)

