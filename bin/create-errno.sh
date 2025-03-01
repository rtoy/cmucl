#! /bin/sh

# Generates the contents of the file code/unix-errno.lisp.  The args
# to this script, if supplied, must be a list of files containing the
# definitions of all the Unix errno values.
#

# Where the output should go.
OUTPUT="src/code/errno.lisp"

# Copy the main errno template to the output.  The template is a lisp
# file so we can read and modify it more easily.
cat bin/errno-template.lisp > $OUTPUT

# Create appropriate DEF-UNIX-ERROR forms by reading header files
# containing the C definitions.

# Set ERRNO_FILES to the files where we can find errno definitions.
case `uname -s` in
    Linux) ERRNO_FILES=/usr/include/asm-generic/errno*.h
	   ;;
    Darwin) ERRNO_FILES=/usr/include/sys/errno.h
	    ;;
esac

awk -f bin/create-def-unix-error.awk ${ERRNO_FILES} >> $OUTPUT

# The tail was also copied from code/unix.lisp.  It's needed to tell
# Lisp about the errno values.
cat >>$OUTPUT <<EOF
;;; End auto-generated forms, if any.

;;;
;;; And now for something completely different ...
(emit-unix-errors)
EOF
