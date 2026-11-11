# CMUCL 22a

# Work in progress

The CMUCL project is pleased to announce the release of CMUCL 22a.
This is a major release which contains numerous enhancements and bug
fixes from the <previous> release.

CMUCL is a free, high performance implementation of the Common Lisp
programming language which runs on most major Unix platforms. It
mainly conforms to the ANSI Common Lisp standard. CMUCL provides a
sophisticated native code compiler; a powerful foreign function
interface; an implementation of CLOS, the Common Lisp Object System,
which includes multi-methods and a meta-object protocol; a
source-level debugger and code profiler; and an Emacs-like editor
implemented in Common Lisp. CMUCL is maintained by a team of
volunteers collaborating over the Internet, and is mostly in the
public domain.

## New in this release:
  * Known issues:
  * Feature enhancements:
  * Changes:
    * Updated CLX from upstream to version from 2024-09-11.
  * ANSI compliance fixes:
  * Bug fixes:
  * Gitlab tickets:
    * #156: <= and >= must be false for NaN
    * #187: Correctly disassemble imul inst with immediate arg
    * #317: String comparisons done with code-units
    * #318: Add `standard-char` type
    * #373: Add macros to handle temp files and directories
    * #387: Update CLX from upstream to version from 2024-09-11
    * #408: Add C functions to get user name and home dir
    * #424: Use fdlibm `hypot` to implement the `hypot` function
            instead of using the version from libm.
    * #426: Type `float-modes` is incorrect for x87 FPU modes
    * #428: Add some constants for common values in irrat.lisp
    * #431: `set-floating-point-modes` incorrectly clears out x87
             precision and rounding modes
    * #434: Add support for correctly rounded double-precision math
            functions
    * #437: Use `realpath` to find the lisp executable
    * #441: Fedora 42 unable to compile core-math pow.c function
    * #443: cim-info.tar.bz2 missing from cmucl.org
    * #444: `unix-stat` and friends return wrong timestamp
    * #446: Simplify getting the errno values.
    * #449: Touch 64-bit timestamp.txt with desired time for testing
    * #446: Use C compiler to get errno values to update UNIX
            defpackage with errno symbols
    * #452: Move bin/errno-foo.lisp to src/tools.
    * #453: Use correct flags for analyzer and always save logs.
    * #454: `make-pathname` signals errors for "silly" values.
    * #456: Improve accuracy for division of complex double-floats
            using Baudin and Smith's robust complex division algorithm
            with improvements by Patrick McGehearty.
    * #457: `delete-directory` was not returning anything so you
	     couldn't tell if it succeeded or not.
    * #458: Spurious overflow in double-double-float multiply.
    * #459: Improve accuracy for division of complex.
            double-double-floats.  The same algorithm is used here as
            for #456.
    * #460: Unit tests were not being recognized as failing on CI.
    * #463: `double-double-float` is missing comparison operations
            between `double-double-float` and `double-float`
    * #468: Make core-math signal the same errors as fdlibm
    * #469: Renable `cr_pow`
    * #471: Add tests for `kernel:%pow`
    * #473: Signal underflow with core-math exp
    * #474: Add functions to print and parse C-style hex floats.
    * #477: Support reading and writing double-double-float in hex
	    format.  "w" is the suffix used to denote
	    double-double-floats.
    * #480, #481:  Add `double-double-float` hex printer and reader
    * #484: Use openlibm single-float functions for lisp
    * #488: Add `log2f` function from openlibm
    * #490: Provide more info in test `mkstemp.name-returned`
    * #495: Describe for deftype returns more information.
    * #496: Fix undefined var in `with-temporary-file`
    * #501: `unix-tests::mkstemp.name-returned` sometimes fails
    * #504: Do correct rounding in `scale-float-maybe-underflow`.
            This was causing some denormals to be read incorrectly.
    * #513: Use correct form for `:report` in `hex-parse-float-error`
    * #515: Handle duplicate ERRNO values
    * #517: Add openlibm double-float special functions
    * #518: Compile and use openlibm for special functions
    * #633: Print the one-operand IMUL with REG/MEM format
    * #659: `NREVERSE` mangles displaced strings
    * #660: Use correct specifier for `deftransform` of `LOGAND`
    * #667: `COMPILE` signals a type error for `(setf foo)` function
            names
    * #668: `READTABLE-CASE` does not check that its arugment is a
            readtable
    * #669: `FILE-STRING-LENGTH` of `BROADCAST-STREAM` is last stream
    * #670: Report the correct datum for `LENGTH` on a dotted list on
            x86
    * #671: Check element-type in `MAKE-STRING-OUTPUT-STREAM`
    * #672: Evaluate the `ELEMENT-TYPE` form in
            `WITH-OUTPUT-TO-STRING`
    * #675: `DEFPACKAGE` `:NICKNAMES` option is cumulative
    * #677: `~E` adds a spurious fraction digit when `D` is supplied
    * #678: `~E` adds a trailing zero digit past the field width
    * #679: `~F` prints trailing-zero fraction when value rounds to
            zero
    * #681: `~F` must print at least one digit after the decimal point
    * #685: `#S` must coerce slot names to keywords
    * #686: Add `-E` option to build.sh to control errno handling
    * #687: Simplify generation of errno files
    * #692: Need to search PATH to find executable
  * Other changes:
  * Improvements to the PCL implementation of CLOS:
  * Changes to building procedure:

This release is not binary compatible with code compiled using CMUCL
21f; you will need to recompile FASL files.

See http://www.cmucl.org or
https://gitlab.common-lisp.net/cmucl/cmucl for more information,
See
https://gitlab.common-lisp.net/cmucl/cmucl/wikis/GettingCmucl
for obtaining CMUCL, including sources and binaries..


We hope you enjoy using this release of CMUCL!
