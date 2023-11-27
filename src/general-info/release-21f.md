# CMUCL 21f

# Work in progress

The CMUCL project is pleased to announce the release of CMUCL 21f.
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
  * ANSI compliance fixes:
  * Bug fixes:
  * Gitlab tickets:
    * ~~#154~~ piglatin translation does not work anymore
	* ~~#171~~ Readably print `(make-pathname :name :unspecfic)`
    * ~~#196~~ Fix issues with mapping and nconc accumulation (mapcan)
    * ~~#216~~ `enough-namestring` with relative pathname fails
    * ~~#234~~ Make :ASCII external format builtin
    * ~~#240~~ Speed up set operations
    * ~~#242~~ Fix bug in `alien-funcall` with `c-call:char` as result type
    * ~~#244~~ Add `c-call:signed-char`
    * ~~#248~~ Print MOVS instruction with correct case
    * ~~#249~~ Replace LEA instruction with simpler shorter instructions in arithmetic vops for x86
    * ~~#253~~ Block-compile list-to-hashtable and callers
    * ~~#258~~ Remove `get-page-size` from linux-os.lisp
    * ~~#269~~ Add function to get user's home directory
  * Other changes:
  * Improvements to the PCL implementation of CLOS:
  * Changes to building procedure:

This release is not binary compatible with code compiled using CMUCL
21e; you will need to recompile FASL files.

See http://www.cmucl.org or
https://gitlab.common-lisp.net/cmucl/cmucl for more information,
See
https://gitlab.common-lisp.net/cmucl/cmucl/wikis/GettingCmucl
for obtaining CMUCL, including sources and binaries..


We hope you enjoy using this release of CMUCL!
