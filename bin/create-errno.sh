#! /bin/sh

# Generates the contents of the file code/errno.lisp.  The args
# to this script, if supplied, must be a list of files containing the
# definitions of all the Unix errno values.
#

usage ()
{
    cat <<EOF
create-erno.sh [-h?DS]
    -h    This help
    -?    This help
    -U    Update the errno file    
    -D	  Do not auto-generate; use default
    -S    Show the resulting generated file; the file is still created.

Auto-generates, if possible, the file src/code/errno.lisp that
contains the def-unix-error forms.

If no options are given (the usual case), src/code/errno.lisp is
updated with the OS-specific values in
bin/errno-{lisp,darwin,solaris}.lisp.  For other OSes a default value
in bin/errno-default.lisp is used.  For supported OSes, we also
regenerate the def-unix-error forms from the system header files and
display a diff between this and the existing files.

If -U is given, the existing OS-specific file is updated with the
generated values.  Use this to update the file if the OS has added or
modified errno values.

If -D is given, default values are used.  The defaults are the
original def-unix-error values from an older version of unix.lisp.
EOF
    exit 0
}

while getopts "h?DSU" arg
do
    case $arg in
	h) usage ;;
	\?) usage ;;
	D) DEFAULT=yes ;;
	S) SHOW=yes ;;
	U) UPDATE=yes ;;
    esac
done

# Output file containing the final errno defintions
OUTPUT="src/code/errno.lisp"
OUTPUT_PKG="src/code/exports-errno.lisp"

# Default file containing errno definitions.
ERRNO_FILE="bin/errno-default.lisp"

# Template file containing the default def-unix-error forms and other
# support code.
TEMPLATE="bin/errno-template.lisp"
TEMPLATE_PKG="bin/errno-pkg-template.lisp"

# Set ERRNO_FILE to an OS-specific name if possible.  If not, use the
# default ERRNO_FILE value.
if [ -z "$DEFAULT" ]; then
    case `uname -s` in
	Linux) ERRNO_FILE="bin/errno-linux.lisp"
	       ;;
	Darwin) ERRNO_FILE="bin/errno-darwin.lisp"
		;;
	SunOS) ERRNO_FILE="bin/errno-solaris.lisp"
	       ;;
	*) # The default case where we use the defaults.  But also disable updating.
	    UPDATE=""
	    ;;
    esac
fi

if [ -n "$DEFAULT" ]; then
    UPDATE=""
fi

find_errno ()
{
    # Create appropriate DEF-UNIX-ERROR forms by reading header files
    # containing the C definitions.  This version with cpp works on
    # Linux, Darwin, and Solaris (with Sun C) to dump the macros
    # defined in errno.h.
    echo '#include <errno.h>' |
	cpp -dM - |
	grep "#define[ \t]\{1,\}E[A-Z0-9]\{1,\}" |
	sed 's/#define \(.*\) \(.*\)$/(def-unix-error \1 \2)/' |
	sort -n -k 3
}

if [ "$UPDATE" = "yes" ]; then
   find_errno > "$ERRNO_FILE"
   exit 0
fi

if [ -z "$DEFAULT" ] && [ -n "$ERRNO_FILE" ]; then
    # First check that the errno definitions haven't changed.  If they
    # have, exit with an error.

    if ! (find_errno | diff -u "$ERRNO_FILE" -) ; then
	echo "Error:  Current $ERRNO_FILE differs from header file definitions"
	exit 1
    fi
fi

# Create the src/code/errno.lisp file from the template and the
# OS-specific errno values (or the default).
cat "$TEMPLATE" "$ERRNO_FILE" > $OUTPUT

cut -d ' ' -f 2 "$ERRNO_FILE" |
    sed 's/\(.*\)/   "\1"/' |
    sort |
    cat "$TEMPLATE_PKG" - > "$OUTPUT_PKG"
echo "   ))" >> "$OUTPUT_PKG"

# If -S option given, cat the output file to stdout
if [ -n "$SHOW" ]; then
    cat $OUTPUT
fi
