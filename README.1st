January 5, 1997

This is release 1.03.7 of the X86 port of CMUCL.

The source kit should work on any port. The kit contains
port specific directories only for X86. The best way to
use the source kit is to shadow the original 17f tree
using the EXTENTIONS:SEARCH-LIST feature.

Changes since the 1.03.6 kit (23 November 96) include:

Generic CMUCL: 

	1. Ray Toy has provided support for logical-pathname
	namestrings in more (all?) IO functions that take
	logical-pathname arguments. Probe-file and load
	with LP namestrings work.

	2. A bug in TRANSLATE-PATHNAME which caused recursive
	logical-pathname translations to fail is fixed.

	3. A few bugs were flushed and fixed by Douglas Crosher
	while working the CL-HTTP port.

	4. Ray Toy's enhancements to floating-point type
	propagation seem real stable and really useful.
	Add :propagate-float-type to *features* before compiling
	a world.

	5. A bug in file-author which signalled an error on
	a pathname argument was fixed. Hemlock's change-log
	function now works.

	6. A few random bugs in Hemlock were fixed.

X86 specific changes:

	0. Due to the following, there may be bootstrap problems
	building this release. Please load p86/boot.lisp
	prior to running worldcom/worldbuild/comcom. You
	must build at least 3 generations of compiler before
	all changes propagate into place. You will need the
	boot.lisp file only for the first generation build.

	1. Major improvements in debugger support thanks to
	Douglas Crosher. Previous "unknown code location"
	notes from debug with confusing debug data are now
	replaced by useful and accurate information.

	2. More math functions are open coded using Intel's
	fancy instructions. Sin, Cos and Tan are fast
	inlined if the argument range is known to be (mod 2^64).
	Currently arguments outside this range return a zero
	value. An efficiency note is printed if the compiler
	can't optimize to fast code.

	3. A problem where PCL CLOS sometimes left dangling
	pointers in unscanned (by GC) memory has been fixed.

Bugs:

Compiler errors are sometimes generated from LABELS
forms which use optional arguments. A workaround is
to not use optional arguments or include all args at
the call site. This seems to be the major outstanding bug.

There is one known bug in restart-case which does not 
want to work unless compiled to native code.

A mis-feature is that compiled code is allocated in static
memory. This restriction may go away in a future release.
The impact of static allocation of compiled code is that
any dead code is not reclaimed and contributes to VM bloat.
This is an operational problem only in intensive incremental
code development.

Please send comments and bugs to 
Paul Werkowski
pw@snoopy.mv.com

or to cmucl-imp@cons.org

-----------------------------------------------------------------------
Informal Release History:

November 23, 1996. 1.03.6

Changes since release 1.03.1 include

	* The FreeBSD/X86 source code has been merged with
	the current CMU tree which had a number of unpublished
	fixes applied since version 17f.

	* Changes specific to the Linux port have been
	merged as well. These changes don't effect FreeBSD users.

	* Major improvements to the floating-point code generators
	thanks to Douglas Crosher.

	* The internal "static-space" has changed along with
	some internal static variable names. You probably will
	have to recompile everything. Sorry.

	* The source kit now contains improvements to floating
	point type propagation, thanks to Ray Toy. These
	changes are still experimental, and disabled by default
	in the binary distribution. When enabled, Python is able
	to optimize the results of intermediate floating point
	types through most arithmetic operations. This is useful
	when bounded float types are used,
	 eg (declare (type (double-float 0d0) x y), then
	(* x y) is known to be non-negative. See file srctran.lisp
	for hints on how to enable this feature.

	* Many internal changes and improvements too 
	numerous to remember.


Changes since release 1.03 include:

	* A nasty little bug was found in floating-point
	register loading that caused erroneous data to
	be created in some conditions. It is now fixed thanks
	to Douglas Crosher.

Changes since release 1.02 include:

	* Fixed some obscure bugs in the Object System.
	Non-standard method-combination now works. These
	changes are platform-independent.

	* Fixed a problem in the definition of the arithmetic-error
	condition which caused bad behavior with divide-by-zero
	and floating-point exceptions.

	* Changes made by Peter Van Eynde for the first Linux release
	have been merged into the current source tree.

	* Improved version of some irrational functions are included
	thanks to Raymond Toy.

	* A few random bug fixes here and there.

Changes since release 1.01 include:

	* Compiled library files for the major subsystems,
	while not in the basic lisp image, are provided
	separately on the www.mv.com/users/pw/lisp/dl
	page. You can download what you want and use
	bin/config to build a custom system.

	* Fixed problem with handling interrupts occuring
	during execution of forms enclosed by
	(without-interrupts ..), for example, interrupting
	a garbage collection and then continuing caused
	a bus error or segment violation.

	* Some random optimizations.

	* Reorganized the way foreign symbols are handled.
	If you get "Unknown foreign symbol" warnings you
	will need to recompile some code.

Changes since release 1.0 include:

	* Fix to floating-point code generator that
	caused incorrect results from FADD or FMUL when
	both operands were in same register.

	* Support for auto-gc-trigger now notifies
	lisp to consider whether or not to initiate GC.

	* Fix to save-lisp C support code which lost
	the restart function address in some cases.

	* Improvements to platform specific parts of
	debug-internals allow better stack backtracking,
	with less hackery.

	* Includes all known patches from CMU. The bug
	which caused the Motif inspector grief is now gone,
	and the inspector is actually usable and useful.


