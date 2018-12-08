# CMUCL 21d

The CMUCL project is pleased to announce the release of CMUCL 21c.
This is a major release which contains numerous enhancements and bug
fixes from the 21a release.

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
  * Feature enhancements
    * Update to ASDF 3.3.2
  * Changes
    * x86 and sparc have replaced the MT19937 RNG with xoroshiro128+ RNG.
      * The required state for this generator is just 4 32-bit words instead of the 600+ for MT19937.
      * The generator is also faster than MT19937 (approximately 28% faster on x86 and 18% on sparc).
      * The new function `KERNEL:RANDOM-STATE-JUMP` modifies the given state to jump 2^64 samples ahead, allowing 2^64 non-overlapping sequences.
    * Updated CLX to telent clx version 06e39a0d.
    * New functions `SET-GC-ASSERTIONS` and `GET-GC-ASSERTIONS`.  See the docstrings for more information and also ~~#69~~.
    * `MACHINE-TYPE` and `MACHINE-VERSION` return more information about thep rocessor cmucl is running on, using information from the `cpuid` instruction.
  * ANSI compliance fixes:
  * Bug fixes:
  * Gitlab tickets:
    * ~~#48~~ Update RNG from MT19937 to xoroshiro128+
    * ~~#45~~ Handling of relative paths in `EXT:RUN-PROGRAM`
    * ~~#50~~ CLX (Hemlock) fails to run.
    * ~~#49~~ CLM crashes
    * ~~#47~~ Backquate and multiple splices
    * ~~#59~~ Incorrect type-derivation for `decode-float`
    * ~~#60~~ The function `C::%UNARY-FROUND` is undefined
    * ~~#58~~ Bogus type error in comparison of complex number with `THE` form
    * ~~#61~~ Segfault when compiling call to `ARRAY-HAS-FILL-POINTER-P` on bit vector constant
    * ~~#62~~ Segfault when compiling `ARRAY-DISPLACEMENT` on a string constant
    * ~~#69~~ GC assertions compiled in and allow user to enable them.
    * ~~#71~~ More info for `MACHINE-TYPE` and `MACHINE-VERSION` for x86
  * Other changes:
  * Improvements to the PCL implementation of CLOS:
  * Changes to building procedure:

This release is not binary compatible with code compiled using CMUCL
21c; you will need to recompile FASL files.

See http://www.cmucl.org or
https://gitlab.common-lisp.net/cmucl/cmucl for more information,
See
https://gitlab.common-lisp.net/cmucl/cmucl/wikis/GettingCmucl
for obtaining CMUCL, including sources and binaries..


We hope you enjoy using this release of CMUCL!
