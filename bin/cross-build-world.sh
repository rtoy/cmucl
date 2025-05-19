#!/bin/sh

usage() {
    echo "cross-build-world.sh [-crlX] [-B file] [-G Gnumake] target-dir cross-dir cross-compiler-script [build-binary [flags]]"
    echo "  -c      Clean target and cross directories before compiling"
    echo "  -i      Enable interactive mode (output to screen instead of log file)."
    echo "  -r      Recompile lisp runtime"
    echo "  -l      Load cross-compiled kernel to make a new lisp kernel"
    echo "  -B file Use this as the cross bootstrap file." 
    echo "  -G make Specifies the name of GNU make"
    echo "  -X      Execute (break) before quitting the cross-build"
}

MAKE=make
INTERACTIVE=nil
BREAK=""

while getopts "cirlXB:G:" arg
do
    case $arg in
      c) CLEAN_DIR=yes ;;
      i) INTERACTIVE=t ;;
      r) BUILD_RUNTIME=yes ;;
      l) LOAD_KERNEL=yes ;;
      B) BOOTSTRAP=$OPTARG ;;
      G) MAKE=$OPTARG ;;
      X) BREAK="(break)" ;;
      h | \?) usage; exit 1 ;;
    esac
done

shift `expr $OPTIND - 1`

if [ "$1" = "" -o "$2" = "" ]
then
    usage
    exit 1
fi

if [ ! -d "$1" ]
then
        echo "$1 isn't a directory"
        exit 2
fi

if [ -f "$2" ]
then
	echo "$2 exists but isn't a directory"
	exit 2
fi

if [ ! -f "$3" ]
then
	echo "$3 doesn't exist, or isn't a normal file"
fi

TARGET="`echo $1 | sed 's:/*$::'`"
CROSS="`echo $2 | sed 's:/*$::'`"
SCRIPT="$3"
LISP="${4:-lisp}"

if [ -z "$BOOTSTRAP" ]; then
    CROSSBOOT="$TARGET/cross-bootstrap.lisp"
else
    CROSSBOOT=$BOOTSTRAP
fi

if [ $# -ge 4 ]
then
	shift 4
else
	shift 3
fi

if [ "$CLEAN_DIR" = "yes" ]; then
    echo "Cleaning directories:  $TARGET $CROSS"
    bin/clean-target.sh $TARGET $CROSS
fi

if [ ! -d "$CROSS" ]
then
	# Create a directory tree that mirrors the source directory tree
	find src -name 'CVS' -prune -o -type d -print | \
		sed "s:^src:$CROSS:g" | xargs mkdir
fi

# Create the errno file containing all the def-unix-error forms.
bin/create-errno.sh

echo cross boot = $CROSSBOOT
$LISP "$@" -noinit -nositeinit <<EOF
(in-package :cl-user)

(setf lisp::*enable-package-locked-errors* nil)

(intl::install)
(setf (ext:search-list "target:")
      '("$CROSS/" "src/"))

(print "$CROSSBOOT")
(load "$CROSSBOOT" :if-does-not-exist nil)

(load "target:code/exports")
(load "target:tools/setup" :if-source-newer :load-source)
(comf "target:tools/setup" :load t)

(setq *gc-verbose* nil *interactive* $INTERACTIVE)

(load "$SCRIPT")

(pushnew :bootstrap *features*)

(setf (ext:search-list "target:")
      '("$TARGET/" "src/"))

(when (probe-file "target:bootstrap.lisp")
  (load "target:bootstrap.lisp"))

(load "target:setenv")

(pushnew :no-pcl *features*)
(pushnew :no-clx *features*)
(pushnew :no-clm *features*)
(pushnew :no-hemlock *features*)

;; At this point we can stop and play around with the compiler to various bits.
;; EOF

(load "target:tools/worldcom")
#-(or no-compiler runtime) (load "target:tools/comcom")
;; Compile at least new-genesis, so that genesis doesn't take ages
#+(or no-compiler runtime) (comf "target:compiler/generic/new-genesis")
#-(or no-pcl runtime) (load "target:tools/pclcom")

(setq *gc-verbose* t *interactive* t)

(load "target:tools/worldbuild")
$BREAK
(ext:quit)
EOF

if [ "$BUILD_RUNTIME" = "yes" ]; then
    echo Building runtime
    bin/git-version.sh -f > src/lisp/cmucl-version.h
    (cd $TARGET/lisp; ${MAKE})
fi

if [ "$LOAD_KERNEL" = "yes" ]; then
    echo Load kernel.core
    bin/load-world.sh -p $TARGET
fi
