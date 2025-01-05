#!/bin/sh 

# Script to determine the cmucl version based on git describe
GIT_HASH="`(cd src; git describe --dirty 2>/dev/null || git describe 2>/dev/null)`"
# echo GIT_HASH = ${GIT_HASH}

if expr "X${GIT_HASH}" : 'Xsnapshot-[0-9][0-9][0-9][0-9]-[01][0-9]' > /dev/null; then
    # The git hash looks like snapshot-yyyy-mm-<stuff>.  Remove the
    # "snapshot-" part.
    DEFAULT_VERSION=`expr "${GIT_HASH}" : "snapshot-\(.*\)"`
fi

if expr "X${GIT_HASH}" : 'X[0-9][0-9][a-f]' > /dev/null; then
    # The git hash looks like a release which is 3 hex digits.  Use it as is.
    DEFAULT_VERSION="${GIT_HASH}"
fi

echo $DEFAULT_VERSION
