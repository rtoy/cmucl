#!/bin/sh -x

# $Id: linker-x86.sh,v 1.1 2010/07/29 04:36:28 rtoy Exp $

# This file written by Raymond Toy as part of CMU Common Lisp and is
# placed in the public domain.

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

OPT_IFADDR="-Wl,--defsym -Wl,initial_function_addr=$IFADDR"
OPT_ARCHIVE="-Wl,--whole-archive -Wl,$CMUCLLIB/lisp.a -Wl,--no-whole-archive"
OPT_CORE="CORRO.o CORSTA.o CORDYN.o"

(cd $OUTDIR
$CCOMPILER -m32 -o $OUTNAME -rdynamic $OPT_IFADDR $OPT_ARCHIVE $OPT_CORE $RO_ADDR $STATIC_ADDR $DYN_ADDR -ldl -lm)

