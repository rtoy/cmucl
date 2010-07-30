#!/bin/sh

# $Id: linker-x86.sh,v 1.2 2010/07/30 20:26:12 rtoy Exp $

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
RO_ADDR="-Wl,--section-start=CORRO=$4"
STATIC_ADDR="-Wl,--section-start=CORSTA=$5"
DYN_ADDR="-Wl,--section-start=CORDYN=$6"

OUTDIR=`dirname $EXEC`
OUTNAME=`basename $EXEC`

CMUCLLIB=`dirname $0`

#OPT_IFADDR="-Wl,--defsym -Wl,initial_function_addr=$IFADDR"
OPT_IFADDR="ifaddr.c"
OPT_ARCHIVE="-Wl,--whole-archive -Wl,$CMUCLLIB/lisp.a -Wl,--no-whole-archive"
OPT_CORE="CORRO.o CORSTA.o CORDYN.o"

trap 'rm -f $OUTDIR/$OPT_IFADDR' 0

(cd $OUTDIR
echo "long initial_function_addr = $IFADDR;" > $OPT_IFADDR
$CCOMPILER -m32 -o $OUTNAME -rdynamic $OPT_IFADDR $OPT_ARCHIVE $OPT_CORE $RO_ADDR $STATIC_ADDR $DYN_ADDR -ldl -lm)

