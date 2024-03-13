#! /bin/bash

# Run the ansi-tests.
#
# We need to check ou ansi-tests if we haven't already.  We expect
# this to be run from the root of the cmucl git tree.  We will check
# it out one level up from where we are.

usage() {
    echo "run-ansi-tests.sh [?h] [-l lisp]"
    echo "    -l lisp       Lisp to use for the tests; defaults to lisp"
    echo "    -h|?          This help message"
    echo ""
    echo "Run the ansi-tests"
    echo ""
    echo "If ../ansi-test does not exist a clone is checked out there."
    echo "Then the ansi-test is run in the clone using the given lisp."
    exit 0;
}

LISP=lisp
while getopts "h?l:" arg
do
    case $arg in
        l) LISP="$PWD/$OPTARG" ;;
        \?) usage ;;
        h) usage ;;
    esac
done

# Shift out the options
shift $[$OPTIND - 1]

set -x
if [ -d ../ansi-test ]; then
    # We already have clone; make sure it's clean by stashing any
    # changes.  Then pull any updates.
    (cd ../ansi-test; git stash; git pull --rebase)
else    
    (cd ../; git clone https://gitlab.common-lisp.net/cmucl/ansi-test.git)
fi

cd ../ansi-test
git checkout issue-276-xoroshiro

make LISP="$LISP batch -noinit -nositeinit"
# There should be no unexpected successes or failures; check these separately
grep -a 'No unexpected successes' test.out && grep -a 'No unexpected failures' test.out

