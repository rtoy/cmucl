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
    * #387: Update CLX from upstream to version from 2024-09-11.
    * #444: `unix-stat` and friends return wrong timestamp
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
