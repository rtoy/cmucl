#!/bin/sh 

usage()
{
    cat <<EOF
git-version.sh [-hfv]
    -h    This help
    -f    The version is printed as a C file #define expression.
          Otherwise, the version is just printed to stdout

    -v    Use this as the version instead of using `git describe`, which
          is the default

Determine the version of cmucl.  By
EOF
    exit 1
}

# If FILE=yes, print out the version as a C file #define.  Otherwise,
# just print the version to stdout and exit.
FILE=""

while getopts "hfv:" arg; do
    case $arg in
	h) usage
	   ;;
	f) FILE=yes
	   ;;
	v) VERSION="$OPTARG"
	   ;;
    esac
done

# Script to determine the cmucl version based on git describe
if [ -n "$VERSION" ]; then
    GIT_HASH="$VERSION"
    DEFAULT_VERSION="$VERSION"
else
    GIT_HASH="`(git describe --dirty 2>/dev/null || git describe 2>/dev/null)`"

    if [ `expr "X$GIT_HASH" : 'Xsnapshot-[0-9][0-9][0-9][0-9]-[01][0-9]'` != 0 ]; then
	# The git hash looks like snapshot-yyyy-mm-<stuff>.  Remove the
	# "snapshot-" part.
	DEFAULT_VERSION=`expr "$GIT_HASH" : "snapshot-\(.*\)"`
    elif [ `expr "X$GIT_HASH" : 'X[0-9][0-9][a-f]'` != 0 ]; then
	# The git hash looks like a release which is 3 hex digits.  Use it as is.
	DEFAULT_VERSION="${GIT_HASH}"
    fi
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

