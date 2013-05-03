#!/bin/sh

while getopts "M:h?" arg
do
    case $arg in
      M) MFLAGS="$OPTARG" ;;
      h) usage ;;
      \?) usage ;;
    esac
done

usage() 
{
    echo "rebuild.-lisp.sh [-h?] [-M opts] target-directory"
    echo "    -h        this help"
    echo "    -?        this help"
    echo "    -M opts   Options to pass to make"

}

shift `expr $OPTIND - 1`

if [ "$1" = "" ]
then
	usage
	exit 1
fi

if [ ! -d "$1" ]
then
	echo "$1" must be a directory.
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"

# Find GNU make:

if [ "$MAKE" = "" ]
then	
	MAKE="`which gmake`" || MAKE="`which make`"
fi

export MAKE

${MAKE} -C $TARGET/lisp $MFLAGS clean && ${MAKE} -C $TARGET/lisp $MFLAGS
