#!/bin/sh

if [ "$1" = "" -o "$2" = "" ]
then
	echo "Usage: $0 target-directory lisp-variant [motif-variant]"
	# List the available lisp-variants
	echo Possible lisp-variants:
	( cd src/lisp/ ; ls -1 Config.* ) | sed 's;^Config[.];;g' | \
		pr -3at -o 8
	echo Possible Motif-variants:
	( cd src/motif/server/ ; ls -1 Config.* ) | sed 's;^Config[.];;g' | \
		pr -3at -o 8
	exit 1
fi

[ -d $1 ] && echo "Error: $1 exists already!" && exit 2

TARGET="`echo $1 | sed 's:/*$::'`"

# Make sure the given variants exist
if [ ! -f src/lisp/Config.$2 ]; then
	echo "No such lisp-variant could be found: Config.$2"
	exit 1
fi

# From the given variant, try to derive a motif variant
if [ "$3" = "" ]; then
    case $2 in
      alpha_linux) motif=alpha_linux ;;
      alpha_osf1) motif=alpha_osf1 ;;
      FreeBSD*) motif=FreeBSD ;;
      NetBSD*) motif=NetBSD ;;
      OpenBSD*) motif=OpenBSD ;;
      sun4_solaris*) motif=solaris ;;
      sun4c*) motif=sun4c_411 ;;
      hp700*) motif=hpux_cc ;;
      pmax_mach) motif=pmax_mach ;;
      sgi*) motif=irix ;;
      linux*) motif=x86 ;;
    esac
elif [ ! -f src/motif/server/Config.$3 ]; then
	echo "No such motif-variant could be found: Config.$3"
	exit 1
fi

# Create a directory tree that mirrors the source directory tree
find src -name 'CVS' -prune -o -type d -print \
	| sed "s:^src:$TARGET:g" | xargs mkdir

# Link Makefile and Config files
( cd $TARGET/lisp ; ln -s ../../src/lisp/GNUmakefile ./Makefile )
( cd $TARGET/lisp ; ln -s ../../src/lisp/Config.$2 ./Config )

# Create empty initial map file
echo 'Map file for lisp version 0' > $TARGET/lisp/lisp.nm

# Create dummy internals.h so we get warned to recompile
echo '#error You need to run genesis (via build-world.sh) before compiling the startup code!' > $TARGET/lisp/internals.h

# Create sample setenv.lisp file
cat <<EOF > $TARGET/setenv.lisp
;;; Put code to massage *features* list here...
;;; This is read early in the build process so don't include complicated
;;; things there.  pushnew, setf, remove, are ok.  In particular, reader
;;; conditionals aren't supported.
;;;
;;; Most of these don't need to be set explicitly anymore unless you're 
;;; changing the features.
(in-package :cl-user)

;; Specific features that most people want:

;;(pushnew :hash-new *features*)
;;(pushnew :random-mt19937 *features*)
;;(pushnew :conservative-float-type *features*)
;;(pushnew :relative-package-names *features*)

;; Version tags

;;(pushnew :cmu18e *features*)
;;(pushnew :cmu18 *features*)
;;(setf *features* (remove :cmu17 *features*))
;;(setf *features* (remove :cmu18c *features*))
;;(setf *features* (remove :cmu18d *features*))

;; Select the target platform and OS here

EOF

# Put in some platform specific items
case $2 in
  *linux*)
      case $2 in
        *_gencgc*) gcname=":gencgc" ;;
	*) gcname=":cgc" ;;
      esac
      cat <<EOF >> $TARGET/setenv.lisp
;; e.g. for Linux on x86 you probably want:
;;(pushnew :x86 *features*)
;;(pushnew :linux *features*)
;; Note! If you are still running glibc 2.1, you need the following:
;; (pushnew :glibc2.1 *features*)
;; Otherwise, i.e. for glibc 2.2 and later you want:
(setf *features* (remove :glibc2.1 *features*))

;; Select conservative GC:
;;(pushnew $gcname *features*)
;; This X86 port supports multiprocessing and linkage-table
;;(pushnew :mp *features*)
;;(pushnew :linkage-table *features*)

;; CPU selection:
;; i486 gives you faster locking
;;(pushnew :i486 *features*)
;; Pentium gives you CPU cyclecounts in time
;;(pushnew :pentium *features*)
EOF
      ;;
  *OpenBSD*)
      case $2 in
        *_gencgc*) gcname=":gencgc" ;;
	*) gcname=":cgc" ;;
      esac
      cat <<EOF >> $TARGET/setenv.lisp
;; e.g. for OpenBSD on x86 you probably want:
;;(pushnew :x86 *features*)
;;(pushnew :openbsd *features*)
;;(pushnew :bsd *features*)

;; Select conservative GC:
;;(pushnew $gcname *features*)
;; This X86 port supports multiprocessing
;;(pushnew :mp *features*)

;; CPU selection:
;; i486 gives you faster locking
;;(pushnew :i486 *features*)
;; Pentium gives you CPU cyclecounts in time
;;(pushnew :pentium *features*)
EOF
      ;;
  *solaris*)
      cat <<EOF >> $TARGET/setenv.lisp
;; e.g. for Solaris on sparc you probably want some of these:
;;(pushnew :sparc *features*)
;;(pushnew :svr4 *features*)

;; Solaris supports linkage-table
;;(pushnew :linkage-table *features*)

;; CPU selection:
;; Sparc-v7 gives us fsqrt instruction
;;(pushnew :sparc-v7 *features*)
;;(setf *features* (remove :sparc-v7 *features*))
;; Sparc-v8 gives us fast fixnum multiplies and divides
;;(pushnew :sparc-v8 *features*)
;;(setf *features* (remove :sparc-v8 *features*))
;; Sparc-v9 gives us the extra FP registers, fast bignum multiply and
;; floor, other float operations available on the Sparc V9, and other
;; assorted V9 features.
;;(pushnew :sparc-v9 *features*)
;;(setf *features* (remove :sparc-v9 *features*))

;;(pushnew :complex-fp-vops *features*)
EOF
      ;;
  *)
      cat <<EOF >> $TARGET/setenv.lisp

;; Select some appropriate features for the $2 platform, otherwise you'll
;; run into problems later on.
EOF
      ;;
esac


echo Motif = $motif
# Do Motif setup
if [ "$motif" != "" ]
then
	( cd $TARGET/motif/server ; ln -s ../../../src/motif/server/GNUmakefile ./Makefile )
	( cd $TARGET/motif/server ; ln -s ../../../src/motif/server/Config.$motif ./Config )
fi
