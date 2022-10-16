# CMUCL 21e

## Work in progress

The CMUCL project is pleased to announce the release of CMUCL 21e.
This is a major release which contains numerous enhancements and bug
fixes from the 21d release.

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
  * Changes
    * Update to ASDF 3.3.6
  * ANSI compliance fixes:
  * Bug fixes:
    * ~~#97~~ Fixes stepping through the source forms in the debugger.  This has been broken for quite some time, but it works now.

  * Gitlab tickets:
    * ~~#68~~ gcc8.1.1 can't build lisp.  Change optimization from `-O2` to `-O1`
    * ~~#72~~ CMU user manual now part of cmucl-site
    * ~~#73~~ Update clx from upstream clx
    * ~~#77~~ Added tests for sqrt for exceptional values
    * ~~#79~~ Autoload ASDF when calling `REQUIRE` the first time.  User's no longer have to explicitly load ASDF anymore.
    * ~~#80~~ Use ASDF to load contribs.  cmu-contribs still exists but does nothing.  The contrib names are the same, except it's best to use a keyword instead of a string.  So, `:contrib-demos` instead of `"contrib-demos"`.
    * ~~#81~~ Added contribs from Eric Marsden
    * ~~#82~~ Replace bc with expr in GNUMakefile
    * ~~#86~~ Building with gcc 8 and later works when using -O2 optimization
    * ~~#90~~ Some static symbols have been removed.  This probably makes the fasl files incompatible with older versions.
    * ~~#91~~ Loop destructuring no longer incorrectly signals an error
    * ~~#95~~ Disassembler syntax of x86 je and movzx is incorrect
    * ~~#97~~ Define and use ud2 instruction instead of int3.  Fixes single-stepping.
    * ~~#98~~ fstpd is not an Intel instruction; disassemble as `fstp dword ptr [addr]`
    * ~~#100~~ ldb prints out Unicode base-chars correctly instead of just the low 8 bits.
    * ~~#103~~ RANDOM-MT19937-UPDATE assembly routine still exists
    * ~~#104~~ Single-stepping broken (fixed via #97).
    * ~~#107~~ Replace u_int8_t with uint8_t
    * ~~#108~~ Update ASDF
    * ~~#112~~ CLX can't connect to X server via inet sockets
    * ~~#113~~ REQUIRE on contribs can pull in the wrong things via ASDF.
    * ~~#121~~ Wrong column index in FILL-POINTER-OUTPUT-STREAM
    * ~~#122~~ gcc 11 can't build cmucl
    * ~~#125~~ Linux `unix-stat` returning incorrect values
    * ~~#127~~ Linux unix-getpwuid segfaults when given non-existent uid.
    * ~~#128~~ `QUIT` accepts an exit code
    * ~~#132~~ Ansi test `RENAME-FILE.1` no fails
    * ~~#134~~ Handle the case of `(expt complex complex-rational)`
    * ~~#136~~ `ensure-directories-exist` should return the given pathspec
    * ~~#141~~ Disallow locales that are pathnames to a localedef file
    * ~~#142~~ `(random 0)` signals incorrect error
  * Other changes:
  * Improvements to the PCL implementation of CLOS:
  * Changes to building procedure:

This release is not binary compatible with code compiled using CMUCL
21d; you will need to recompile FASL files.

See http://www.cmucl.org or
https://gitlab.common-lisp.net/cmucl/cmucl for more information,
See
https://gitlab.common-lisp.net/cmucl/cmucl/wikis/GettingCmucl
for obtaining CMUCL, including sources and binaries..


We hope you enjoy using this release of CMUCL!
