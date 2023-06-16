#! /bin/bash

# Run the testsuite.
#
# By default, all the tests are run, but if additional args are given,
# then just those tests are run.

usage() {
    echo "run-tests.sh [?] [-l lisp] [tests]"
    echo "    -l lisp      Lisp to use for the tests; defaults to lisp"
    echo "    -?           This help message"
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
while getopts "h?l:" arg
do
    case $arg in
      l) LISP=$OPTARG ;;
      \?) usage ;;
    esac
done

# Shift out the options
shift $[$OPTIND - 1]

# Create the test directory needed by the issue.45 test.
rm -rf test-tmp
mkdir test-tmp
ln -s /bin/ls test-tmp/ls-link

# Cleanup temp files and directories that we created during testing.
function cleanup {
    rm -f /tmp/trac.36.bom.txt /tmp/trac.43.txt
    rm -rf /tmp/foo
}

trap cleanup EXIT

echo $PWD
ls tests/*.c
(cd tests; gcc -m32 -O3 -c test-return.c)

if [ $# -eq 0 ]; then
    # No args so run all the tests
    $LISP -noinit -load tests/run-tests.lisp -eval '(cmucl-test-runner:run-all-tests)'
else
    # Run selected files.  Convert each file name to uppercase and append "-TESTS"
    result=""
    for f in $*
    do
	new=`echo $f | tr '[a-z]' '[A-Z]'`
        result="$result "\"$new-TESTS\"
    done
    $LISP -noinit -load tests/run-tests.lisp -eval "(progn (cmucl-test-runner:load-test-files) (cmucl-test-runner:run-test $result))"
fi

