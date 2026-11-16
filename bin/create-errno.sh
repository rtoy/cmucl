#! /bin/sh

# Generates the contents of the file code/errno.lisp.  The args
# to this script, if supplied, must be a list of files containing the
# definitions of all the Unix errno values.
#

usage ()
{
    cat <<EOF
create-erno.sh [-h?DSU] target
    -h    This help
    -?    This help
    -U    Update the errno file    
    -D	  Do not auto-generate; use default
    -S    Show the resulting generated file; the file is still created.

The errno values are found using the C compiler from the Config file
in the target build directory.  The environment variable MAKE names
GNU make; the default is make.

Auto-generates, if possible, the file src/code/errno.lisp that
contains the def-unix-error forms.

If no options are given (the usual case), src/code/errno.lisp is
updated with the OS-specific values in
bin/errno-{lisp,darwin,solaris}.lisp.  For other OSes a default value
in bin/errno-default.lisp is used.  For supported OSes, we also
regenerate the defconstant forms from the system header files and
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
shift `expr $OPTIND - 1`

# The target build directory.  Its Config file determines the C
# compiler used to find the errno values.
TARGET="$1"
if [ -z "$TARGET" ]; then
    echo "Error:  target build directory is required"
    exit 1
fi
if [ ! -f "$TARGET/lisp/Config" ]; then
    echo "Error:  $TARGET/lisp/Config not found"
    exit 1
fi

# Output file containing the final errno defintions
OUTPUT="src/code/errno.lisp"
OUTPUT_PKG="src/code/exports-unix.lisp"

# Default file containing errno definitions.
ERRNO_FILE="src/tools/errno-default.lisp"

# Template file containing the default def-unix-error forms and other
# support code.  TEMPLATE is for the definition of all the errno
# values.  TEMPLATE_PKG is for the definition of the UNIX package.
TEMPLATE="src/tools/errno-template.lisp"
TEMPLATE_PKG="src/tools/unix-pkg-template.lisp"

# Set ERRNO_FILE to an OS-specific name if possible.  If not, use the
# default ERRNO_FILE value.
if [ -z "$DEFAULT" ]; then
    # shellcheck disable=SC2006
    case `uname -s` in
	Linux) ERRNO_FILE="src/tools/errno-linux.lisp"
	       ;;
	Darwin) ERRNO_FILE="src/tools/errno-darwin.lisp"
		;;
	SunOS) ERRNO_FILE="src/tools/errno-solaris.lisp"
	       ;;
	*) # The default case where we use the defaults.  But also disable updating.
	    UPDATE=""
	    ;;
    esac
fi

if [ -n "$DEFAULT" ]; then
    UPDATE=""
fi

# Have make generate the errno values using the C compiler from the
# target's Config file.
if [ -z "$DEFAULT" ]; then
    ${MAKE:-make} -C "$TARGET/lisp" errno-defs.lisp || exit 1
    DEFS="$TARGET/lisp/errno-defs.lisp"
fi

if [ "$UPDATE" = "yes" ]; then
    cp "$DEFS" "$ERRNO_FILE"
    exit 0
fi

if [ -n "$DEFS" ]; then
    # First check that the errno definitions haven't changed.  If they
    # have, exit with an error.

    if ! diff -u "$ERRNO_FILE" "$DEFS" ; then
	echo "Error:  Current $ERRNO_FILE differs from header file definitions"
	exit 1
    fi
fi

# Create the src/code/errno.lisp file from the template and the
# OS-specific errno values (or the default).
cat "$TEMPLATE" "$ERRNO_FILE" > $OUTPUT

cut -d ' ' -f 2 "$ERRNO_FILE" |
    sed 's/\(.*\)/           "\1"/' |
    sort |
    cat "$TEMPLATE_PKG" - > "$OUTPUT_PKG"
echo "   ))" >> "$OUTPUT_PKG"

# If -S option given, cat the output file to stdout
if [ -n "$SHOW" ]; then
    cat $OUTPUT
fi
