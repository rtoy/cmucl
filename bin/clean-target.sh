#!/bin/sh

usage() {
    cat <<EOF
Usage: (basename "$0") [-l] dir [dir1 dir2 ...]
  -h       This help
  -l       Clean out the C runtime as well
  -K what  Specify what to keep:  lib, core, all
             lib keeps the module libraries
             core keeps all lisp core files
             all keeps the libs and cores
           (for build.sh)

Cleans out all Lisp fasls from the given directories
If -l is also given, the C runtime is cleared as well.  This includes
all object files, the lisp binary itself, and any selected configuration
files.  The motif server is also removed.
EOF
    exit 1
}

while getopts "h?lK:" arg
do
    case $arg in
	l) CLEAN_C=1 ;;
        K) KEEP=$OPTARG ;;
	h | \?) usage ;;
    esac
done
	
shift $((OPTIND - 1))

if [ $# -lt 1 ]; then
    usage
fi

for d in "$@" 
do
    if [ ! -d "$d" ]; then
	echo "$d isn't a directory"
	exit 2
    fi
    D="$(echo "$d" | sed 's:/*$::')"
    TARGET="$TARGET $D"
done

# Default:  don't keep any libraries, and delete an lisp cores
GREP="cat"
CORE='-o -name "*.core"'

if [ -n "$KEEP" ]; then
    case $KEEP in
      lib) GREP='grep -Ev'
	   PATTERN='(gray-streams|gray-compat|simple-streams|iodefs|external-formats|clx|hemlock|clm)-library' ;;
      core) CORE='' ;;
      all) GREP='grep -Ev'
	   PATTERN='(gray-streams|gray-compat|simple-streams|iodefs|external-formats|clx|hemlock|clm)-library|(asdf|defsystem)'
	   CORE='' ;;
    esac
fi
	  
find "$TARGET" -name "*.bytef" -o -name "*.lbytef" -o -name "*.assem" \
	-o -name "*.armf" \
	-o -name "*.axpf" \
	-o -name "*.hpf" \
	-o -name "*.ppcf" \
	-o -name "*.pmaxf" \
	-o -name "*.sgif" \
	-o -name "*.sparcf" \
	-o -name "*.sse2f" \
	-o -name "*.x86f" \
	"$CORE" |
	$GREP "$PATTERN" | xargs rm 2> /dev/null

for d in $TARGET
do
    rm -f "$d"/compile-*.log "$d"/hemlock/spell-dictionary.bin 2> /dev/null
    if [ -n "$CLEAN_C" ]; then
	rm -f "$d"/lisp/* "$d"/motif/server/*
    fi
done


true
