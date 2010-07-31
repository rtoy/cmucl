#!/bin/sh

# $Id: linker-x86.sh,v 1.5 2010/07/31 02:45:45 rtoy Exp $

# This file written by Raymond Toy as part of CMU Common Lisp and is
# placed in the public domain.

OPSYS=`uname`

if [ "X$CMU_DEBUG_LINKER" != "X" ]; then
    set -x
fi

if [ $# -ne 6 ]; then
    echo "Usage: `basename $0` <c-compiler> <initial-func-addr> <executable> <ro-addr> <static-addr> <dyn-addr>"
    exit 1
fi

CCOMPILER=$1
IFADDR=$2
EXEC=$3

OUTDIR=`dirname $EXEC`
OUTNAME=`basename $EXEC`

CMUCLLIB=`dirname $0`

# Name of file where we write the actual initial function address.
OPT_IFADDR="cmu-ifaddr-$$.c"
# Names of the core sections from Lisp.
OPT_CORE="CORRO.o CORSTA.o CORDYN.o"

case `uname` in
  Linux*)
      # How to specify the starting address for each of the sections
      #RO_ADDR="-Wl,--section-start=CORRO=$4"
      #STATIC_ADDR="-Wl,--section-start=CORSTA=$5"
      #DYN_ADDR="-Wl,--section-start=CORDYN=$6"

      #OPT_IFADDR="-Wl,--defsym -Wl,initial_function_addr=$IFADDR"

      # Specify how to link the entire lisp.a library
      OPT_ARCHIVE="-Wl,--whole-archive -Wl,$CMUCLLIB/lisp.a -Wl,--no-whole-archive"

      # Extra stuff.

      OPT_EXTRA="-rdynamic"
      # See Config.x86_linux
      OS_LIBS=-ldl
      ;;
  Darwin*)
      # How to specify the starting address for each of the sections
      #RO_ADDR="-segaddr CORRO $4"
      #STATIC_ADDR="-segaddr CORSTA $5"
      #DYN_ADDR="-segaddr CORDYN $6"

      # Specify how to link the entire lisp.a library
      OPT_ARCHIVE="-all_load $CMUCLLIB/lisp.a"

      # Extra stuff.  For some reason one __LINKEDIT segment is mapped
      # just past the dynamic space.  This messes things up, so we move it
      # to another address.  This seems to be free, at least on 10.5.

      OPT_EXTRA="-segaddr __LINKEDIT 0x99000000 -rdynamic"
      # See Config.x86_darwin
      OS_LIBS=
      ;;
  SunOS*)
      if [ "$CCOMPILER" != "cc" ]; then
	  echo Using $CCOMPILER is not currently supported
	  exit 1
      fi
      # We don't need anything special to set the starting address.
      # map_core_sections does that for us on sparc.

      # Specify how to link the entire lisp.a library
      OPT_ARCHIVE="-Xlinker -z -Xlinker allextract -Xlinker $CMUCLLIB/lisp.a -Xlinker -z -Xlinker defaultextract"

      # Extra stuff.  For some reason one __LINKEDIT segment is mapped
      # just past the dynamic space.  This messes things up, so we move it
      # to another address.  This seems to be free, at least on 10.5.

      OPT_EXTRA="-Bdynamic"

      # See Config.sparc_sunc
      OS_LIBS="-lsocket -lnsl -ldl"
      ;;

esac

trap 'rm -f $OUTDIR/$OPT_IFADDR' 0

(cd $OUTDIR
echo "long initial_function_addr = $IFADDR;" > $OPT_IFADDR
$CCOMPILER -m32 -o $OUTNAME $OPT_IFADDR $OPT_ARCHIVE $OPT_CORE $RO_ADDR $STATIC_ADDR $DYN_ADDR $OPT_EXTRA $OS_LIBS -lm)

