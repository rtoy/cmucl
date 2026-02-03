#! /bin/bash

# Run the testsuite.
#
# By default, all the tests are run, but if additional args are given,
# then just those tests are run.

usage() {
    echo "run-tests.sh [-?h] [-d test-dir] [-l lisp] [tests]"
    echo "    -d test-dir  Directory containing the unit test files"
    echo "    -l lisp      Lisp to use for the tests; defaults to lisp"
    echo "    -p           Skip package-local-nicknames test"
    echo "    -u           Skip lisp-unit tests"
    echo "    -?           This help message"
    echo "    -h           This help message"
    echo ""
    echo "Run the test suite"
    echo ""
    echo "Any remaining args are the names of the tests to run."
    echo "These are basically the file names (without extension)"
    echo "in the tests/ directory."
    echo ""
    echo "This script expects to be run from the top level of the"
    echo "cmucl source tree.  That is, is should be invoked as"
    echo "bin/run-tests.sh"
    exit 0;
}

LISP=lisp
while getopts "puh?l:d:" arg
do
    case $arg in
      l) LISP=$OPTARG ;;
      d) TESTDIR=$OPTARG ;;
      p) SKIP_PLN=yes ;;
      u) SKIP_UNIT=yes ;;
      h|\?) usage ;;
    esac
done

# Shift out the options
shift $((OPTIND - 1))

# Create the test directory needed by the issue.45 test.
rm -rf test-tmp
mkdir test-tmp
ln -s /bin/ls test-tmp/ls-link

# Set the timestamps on 64-bit-timestamp-2038.txt and
# 64-bit-timestamp-2106.txt, but only for OSes where we know this
# works.  (This is so we don't an annoying error message from touch
# that doesn't accept the -d option, like MacOS 10.13.)  The time for
# the first file is a negative value for a 32-bit time_t.  The second
# file won't fit in a 32-bit time_t value.  It's ok if this doesn't
# work in general, as long as it works on Linux for the stat test in
# tests/os.lisp.
case `uname -s` in
    Linux)
	# -t format is [[CC]YY]MMDDhhmm[.ss]
	touch -t 203804010000 tests/resources/64-bit-timestamp-2038.txt
	touch -t 210604010000 tests/resources/64-bit-timestamp-2106.txt
	;;
esac

# Cleanup temp files and directories that we created during testing.
function cleanup {
    rm -f /tmp/trac.36.bom.txt /tmp/trac.43.txt
    rm -rf /tmp/foo
}

trap cleanup EXIT

if [ -n "${TESTDIR}" ]; then
    TESTDIRARG=" :test-directory \"$TESTDIR/\""
else
    TESTDIR="tests/"
    TESTDIRARG=""
fi
# Compile up the C file that is used for testing alien funcalls to
# functions that return integer types of different lengths.  We use
# gcc since clang isn't always available.
(cd "$TESTDIR" || exit 1 ; gcc -m32 -O3 -c test-return.c)

if [ "$SKIP_UNIT" != "yes" ]; then
    if [ $# -eq 0 ]; then
	# Test directory arg for run-all-tests if a non-default 
	# No args so run all the tests
	$LISP -nositeinit -noinit -load "$TESTDIR"/run-tests.lisp -eval "(cmucl-test-runner:run-all-tests ${TESTDIRARG})" ||
	    exit 1
    else
	# Run selected files.  Convert each file name to uppercase and append "-TESTS"
	result=""
	for f in "$@"
	do
	    new=$(echo "$f" | tr '[:lower:]' '[:upper:]')
            result="$result "\"$new-TESTS\"
	done
	# Run unit tests.  Exits with a non-zero code if there's a failure.

	$LISP -nositeinit -noinit -load "$TESTDIR"/run-tests.lisp -eval "(progn (cmucl-test-runner:load-test-files) (cmucl-test-runner:run-test $result))" ||
	    exit 1
    fi
fi

## Now run tests for trivial-package-local-nicknames
if [ "$SKIP_PLN" != "yes" ]; then
    REPO=trivial-package-local-nicknames
    BRANCH=cmucl-updates

    set -x
    if [ -d ../$REPO ]; then
	(cd ../$REPO || exit 1; git stash; git checkout $BRANCH; git pull --rebase)
    else
	(cd ..; git clone https://gitlab.common-lisp.net/cmucl/$REPO.git)
    fi

    LISP=$PWD/$LISP
    cd ../$REPO || exit 1
    git checkout $BRANCH

    # Run the tests.  Exits with a non-zero code if there's a failure.
    $LISP -noinit -nositeinit -batch <<'EOF'
(require :asdf)
(push (default-directory) asdf:*central-registry*)
(asdf:test-system :trivial-package-local-nicknames)
EOF
fi
