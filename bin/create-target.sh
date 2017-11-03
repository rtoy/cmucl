#!/bin/sh

diag() { echo "($prgm_name) $@" >&2; }
quit() { diag "not OK: $@"; exit 1; }
usage() {
    echo "Usage: $prgm_name TARGET-DIR [LISP-VARIANT [MOTIF-VARIANT]]"
    echo ""
    echo "Creates a directory structure in TARGET-DIR for use in building CMUCL."
    echo "A simple logic is used to find the optional -VARIANT parameters."
    echo ""
    # List possible values for lisp-variant and motif-variant
    echo "Possible LISP-VARIANTs:"
    ( cd src/lisp/ && ls -1 Config.* ) | sed 's;^Config[.];;g' | grep -v common | pr -3at -o 8 || quit "Can't list lisp-variants"
    echo "Possible MOTIF-VARIANTs:"
    ( cd src/motif/server/ && ls -1 Config.* ) | sed 's;^Config[.];;g' | pr -3at -o 8 || quit "Can't list lisp-variants"
    exit 2
}

##--
prgm_name=`basename $0` bld_dir=$1 lisp_variant=$2 motif_variant=$3

while getopts "h?" arg
do
    case $arg in
      h) usage ;;
      \?) usage ;;
    esac
done

bld_dir=$1
lisp_variant=$2
motif_variant=$3

exec 2>&1

[ -n "$bld_dir" ] || usage

uname_s=`uname -s`
uname_m=`uname -m 2>/dev/null`
[ -n "$lisp_variant" ] || {
    case $uname_s in
	Linux) lisp_variant=x86_linux ;;
	FreeBSD) lisp_variant=x86_freebsd ;;
	SunOS)
	    case $uname_m in
		i86pc) lisp_variant=x86_solaris_sunc ;;
		sun*) lisp_variant=sparc_sunc ;;
	    esac 
	    ;;
	Darwin)
	    case $uname_m in
		ppc) lisp_variant=ppc_darwin ;;
		i386|x86_64) lisp_variant=x86_darwin ;;
	    esac
	    ;;
        NetBSD) lisp_variant=x86_netbsd ;;
	*) quit "Unsupported OS: $uname_s";;
    esac
}
[ -n "$lisp_variant" ] || quit "Can't determine lisp_variant"
[ -f src/lisp/Config.$lisp_variant ] || quit "Config.$lisp_variant not found"

case $lisp_variant in
    *linux*) lvshort=linux;;
    *freebsd*) lvshort=freebsd;;
    *solaris*|sparc*) lvshort=solaris;;
    *) lvshort=unknown;;
esac

# Beats me why we care about Motif today but let it be, other than on FreeBSD,
# where the config file is wrong anyway. (agoncharov, 2011-10-07)
case $uname_s in
    FreeBSD) :;; # Free
    *) # Enjoy
	[ -n "$motif_variant" ] || {
	    case $lisp_variant in
		NetBSD*) motif_variant=NetBSD ;;
		OpenBSD*) motif_variant=OpenBSD ;;
		*_darwin) motif_variant=Darwin ;;
		sun4_solaris_gcc|sparc_gcc) motif_variant=solaris ;;
		sun4_solaris_sunc|sparc_sunc|x86_solaris_sunc|sparc64_sunc) motif_variant=solaris_sunc ;;
		sun4c*) motif_variant=sun4c_411 ;;
		hp700*) motif_variant=hpux_cc ;;
		pmax_mach) motif_variant=pmax_mach ;;
		sgi*) motif_variant=irix ;;
		x86_linux|arm_linux|linux*) motif_variant=x86 ;;
	    esac
	}
	[ -f src/motif/server/Config.$motif_variant ] || quit "No such motif-variant could be found: Config.$motif_variant"
	;;
esac

# Tell user what's we've configured
diag "Settings: bld_dir=$bld_dir lisp_variant=$lisp_variant ${motif_variant:+motif_variant=$motif_variant}"

# Create a directory tree that mirrors the source directory tree
[ -d "$bld_dir" -o -f "$bld_dir" ] && quit "Exists: `ls -ld $bld_dir`"
mkdir -p "$bld_dir"
(cd src && find . -name .git -prune -o -type d -print) | (cd $bld_dir && xargs mkdir -p) ||
quit "Can't create target directories"

top_dir=$PWD
cd $bld_dir/lisp || quit "Can't cd $bld_dir/lisp"

# Link Makefile and Config files
ln -s ../../src/lisp/GNUmakefile ../../src/lisp/Config.$lisp_variant ../../src/lisp/Config.*_common .
ln -s Config.$lisp_variant Config
[ -n "$motif_variant" ] && (
    cd ../motif/server || quit "Can't cd motif/server" # We will still continue in the outer shell
    ln -s ../../../src/motif/server/GNUmakefile ./Makefile
    ln -s ../../../src/motif/server/Config.$motif_variant ./Config
)

echo 'Map file for lisp version 0' > lisp.nm # Empty initial map file
echo '#error You need to run genesis (via build-world.sh) before compiling the startup code!' > internals.h
(
    setenv_dir=$top_dir/src/tools/setenv-scripts
    cat $setenv_dir/base-features.lisp 
    case $lvshort in
	linux|freebsd) gcname=":gencgc"; sed "s;@@gcname@@;$gcname;" $setenv_dir/$lvshort-features.lisp >> setenv.lisp;;
	solaris) cat $setenv_dir/solaris-features.lisp;;
	*) sed "s;@@LISP@@;$lisp_variant;" $setenv_dir/unknown.lisp;;
    esac
) > ../setenv.lisp || quit "Can't create setenv.lisp"
