# CMUCL 21e

## Work in prograss

The CMUCL project is pleased to announce the release of CMUCL 21e.
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
    * Building with gcc8 or later doesn't work with the default -O option. Use -O1 instead.  This shouldn't really impact overall speed much.
    * Added simple support to compile with clang instead, which works. (Use x86_linux_clang).
  * Feature enhancements
  * Changes
    * Update to ASDF 3.3.4
  * ANSI compliance fixes:
  * Bug fixes:
  * Gitlab tickets:
    * ~~#73~~ Update clx from upstream clx
    * ~~#79~~ Autoload ASDF when calling `REQUIRE` the first time.  User's no longer have to explicitly load ASDF anymore.
    * ~~#80~~ Use ASDF to load contribs.  cmu-contribs still exists but does nothing.  The contrib names are the same, except it's best to use a keyword instead of a string.  So, `:contrib-demos` instead of `"contrib-demos"`.
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
