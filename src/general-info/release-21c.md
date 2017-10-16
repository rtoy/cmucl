<span style="color:red">Work in progress</span>

========================== C M U C L  21 c =============================

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

New in this release:
  * Known issues:

  * Feature enhancements

  * Changes
    * ASDF 3.3.0

  * ANSI compliance fixes:

  * Bug fixes:
    * `ENCODE-UNIVERSAL-TIME` accepts dates from 1899 if the final date
      after accounting for time zones results in a positive
      value. (See ticket ~~#36~~.)
    * `CL:SLEEP` no longer gets interrupted (See ticket ~~#26~~.)
    * Fix some compiler warnings and clean up funny indentation in
      asin code.

  * Trac Tickets:

  * Gitlab tickets:
    * Ticket ~~#36~~: encode-universal-time signals error
    * Ticket ~~#26~~: The cmucl that never sleeps
    * Ticket ~~#40~~: Move heap space location for linux
    * Ticket ~~#41~~: Report proper process status
    * Ticket ~~#44~~: Add docstrings for process accessors

  * Other changes:
    * Continuous integration added to build and test cmucl on every
      check-in.  This does a build on a Linux system.

  * Improvements to the PCL implementation of CLOS:

  * Changes to building procedure:


This release is not binary compatible with code compiled using CMUCL
21b; you will need to recompile FASL files.

See http://www.cmucl.org or
https://gitlab.common-lisp.net/cmucl/cmucl for more information,
See
https://gitlab.common-lisp.net/cmucl/cmucl/wikis/GettingCmucl
for obtaining CMUCL, including sources and binaries..


We hope you enjoy using this release of CMUCL!
