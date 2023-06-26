# CMUCL 21e

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
    * The default external format is `:utf-8` instead of `:iso8859-1`.
  * ANSI compliance fixes:
  * Bug fixes:
    * ~~#97~~ Fixes stepping through the source forms in the debugger.  This has been broken for quite some time, but it works now.

  * Gitlab tickets:
    * ~~#68~~ gcc8.1.1 can't build lisp.  Change optimization from `-O2` to `-O1`.
    * ~~#72~~ CMU user manual now part of cmucl-site.
    * ~~#73~~ Update clx from upstream clx.
    * ~~#77~~ Added tests for sqrt for exceptional values.
    * ~~#79~~ Autoload ASDF when calling `REQUIRE` the first time.  User's no longer have to explicitly load ASDF anymore.
    * ~~#80~~ Use ASDF to load contribs.  cmu-contribs still exists but does nothing.  The contrib names are the same, except it's best to use a keyword instead of a string.  So, `:contrib-demos` instead of `"contrib-demos"`.
    * ~~#81~~ Added contribs from Eric Marsden.
    * ~~#82~~ Replace bc with expr in GNUMakefile.
    * ~~#86~~ Building with gcc 8 and later works when using -O2 optimization.
    * ~~#90~~ Some static symbols have been removed.  This probably makes the fasl files incompatible with older versions.
    * ~~#91~~ Loop destructuring no longer incorrectly signals an error.
    * ~~#95~~ Disassembler syntax of x86 je and movzx is incorrect.
    * ~~#97~~ Define and use ud2 instruction instead of int3.  Fixes single-stepping.
    * ~~#98~~ fstpd is not an Intel instruction; disassemble as `fstp dword ptr [addr]`.
    * ~~#100~~ ldb prints out Unicode base-chars correctly instead of just the low 8 bits.
    * ~~#103~~ RANDOM-MT19937-UPDATE assembly routine still exists.
    * ~~#104~~ Single-stepping broken (fixed via #97).
    * ~~#107~~ Replace u_int8_t with uint8_t.
    * ~~#108~~ Update ASDF.
    * ~~#112~~ CLX can't connect to X server via inet sockets.
    * ~~#113~~ REQUIRE on contribs can pull in the wrong things via ASDF.
    * ~~#120~~ `SOFTWARE-VERSION` is implemented in C.
    * ~~#121~~ Wrong column index in FILL-POINTER-OUTPUT-STREAM
    * ~~#122~~ gcc 11 can't build cmucl
    * ~~#124~~ directory with `:wild-inferiors` doesn't descend subdirectories 
    * ~~#125~~ Linux `unix-stat` returning incorrect values
    * ~~#127~~ Linux unix-getpwuid segfaults when given non-existent uid.
    * ~~#128~~ `QUIT` accepts an exit code.
    * ~~#130~~ Move file-author to C.
    * ~~#132~~ Ansi test `RENAME-FILE.1` no longer fails.
    * ~~#134~~ Handle the case of `(expt complex complex-rational)`.
    * ~~#136~~ `ensure-directories-exist` should return the given pathspec.
    * #139 `*default-external-format*` defaults to `:utf-8`; add alias for `:locale` external format.
    * ~~#140~~ External format for streams that are not `file-stream`'s.
    * ~~#141~~ Disallow locales that are pathnames to a localedef file.
    * ~~#142~~ `(random 0)` signals incorrect error.
    * ~~#143~~ `LISTEN` doesn't signal error when given more than one arg
    * ~~#147~~ `stream-line-column` method missing for `fundamental-character-output-stream`.
    * ~~#149~~ Call setlocale(3C) on startup.
    * ~~#150~~ Add aliases for external format cp949 and euckr.
    * ~~#151~~ Change `*default-external-format*` to `:utf-8`.
    * ~~#152~~ Add new external format, `:locale` as an alias to the codeset from LANG and friends.
    * ~~#!53~~ Terminals default to an encoding of `:locale`.
    * ~~#155~~ Wrap help strings neatly.
    * ~~#157~~ `(directory "foo/**/")` only returns directories now.
    * #158 Darwin uses utf-8, but we don't support all the rules for pathnames.
    * ~~#162~~ `*filename-encoding*` defaults to `:null` to mean no encoding.
    * ~~#163~~ Add command-line option `-version` and `--version` to get lisp version.
    * ~~#165~~ Avoid inserting NIL into simple `LOOP` from `FORMAT`.
    * ~~#166~~ Fix incorrect type declaration for exponent from `integer-decode-float`.
    * ~~#167~~ Low bound for `decode-float-exponent` type was off by one.
    * ~~#168~~ Don't use negated forms for jmp instructions when possible.
    * ~~#169~~ Add pprinter for `define-vop` and `sc-case`.
    * ~~#172~~ Declare `pathname-match-p` as returning `nil` or `pathname`.
    * ~~#173~~ Add pprinter for `define-assembly-routine`.
    * ~~#176~~ `SHORT-SITE-NAME` and `LONG-SITE-NAME` return `NIL`.
    * ~~#177~~ Add pprinter for `deftransform` and `defoptimizer`.
    * ~~#192~~ Print radix marker in disassemblies and adjust note column to be larger for x86.
    * ~~#193~~ Treat `NIL` and `:UNSPECIFIC` as equivalent when comparing pathnames with `equal`.
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
