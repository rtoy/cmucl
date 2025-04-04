#!/bin/sh 

# If FILE=yes, print out the version as a C file #define.  Otherwise,
# just print the version to stdout and exit.
FILE=""

while getopts "f" arg; do
    case $arg in
	f) FILE=yes
	   ;;
    esac
done

# Script to determine the cmucl version based on git describe
GIT_HASH="`(git describe --dirty 2>/dev/null || git describe 2>/dev/null)`"

if [ `expr "X$GIT_HASH" : 'Xsnapshot-[0-9][0-9][0-9][0-9]-[01][0-9]'` != 0 ]; then
    # The git hash looks like snapshot-yyyy-mm-<stuff>.  Remove the
    # "snapshot-" part.
    DEFAULT_VERSION=`expr "$GIT_HASH" : "snapshot-\(.*\)"`
elif [ `expr "X$GIT_HASH" : 'X[0-9][0-9][a-f]'` != 0 ]; then
    # The git hash looks like a release which is 3 hex digits.  Use it as is.
    DEFAULT_VERSION="${GIT_HASH}"
fi

if [ -z "$FILE" ]; then
    echo $DEFAULT_VERSION
else
    cat <<EOF
/*
 * Cmucl version
 *
 * DO NOT EDIT!  This file is auto-generated via bin/git-version.sh.
 */

#define CMUCL_VERSION "$DEFAULT_VERSION"
EOF
fi

