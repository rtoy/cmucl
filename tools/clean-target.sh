#!/bin/sh

if [ "$1" = "" ]
then
	echo "Usage: $0 target-directory"
	exit 1
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"

find $TARGET -name "*.bytef" -o -name "*.lbytef" -o -name "*.assem" -o \
	-name "*.axpf" -o \
	-name "*.hpf" -o \
	-name "*.pmaxf" -o \
	-name "*.sgif" -o \
	-name "*.ppcf" -o \
	-name "*.sparcf" -o \
	-name "*.x86f" -o \
	-name "*.core" | xargs rm 2> /dev/null
rm -f $TARGET/compile-*.log $TARGET/hemlock/spell-dictionary.bin 2> /dev/null

true
