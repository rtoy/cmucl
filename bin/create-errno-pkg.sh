#! /bin/sh

# For each supported OS, ERRNO_FILES should be set to a list of all
# the files that contain the definitions of the errno values.
case `uname -s` in
    Linux) ERRNO_FILES=/usr/include/asm-generic/errno*.h
	   ;;
esac

cat <<EOF
(defpackage "UNIX-ERRNO"
  (:export
`awk -f bin/gen-errno-exports.gawk ${ERRNO_FILES} | sort `
   ))

EOF
