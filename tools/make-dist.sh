#!/bin/sh

if [ "$1" = "" -o "$2" = "" -o "$3" = "" -o "$4" = "" ]
then
	echo "Usage: $0 target-directory version arch os"
	exit 1
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"
VERSION=$2
ARCH=$3
OS=$4
ROOT=`dirname $0`

$ROOT/make-main-dist.sh $TARGET $VERSION $ARCH $OS || exit 1
$ROOT/make-extra-dist.sh $TARGET $VERSION $ARCH $OS || exit 2
