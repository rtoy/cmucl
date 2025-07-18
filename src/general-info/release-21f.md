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
    * Add support for Gray streams implementation of file-length via
      `ext:stream-file-length` generic function.
  * Changes:
    * Update to ASDF 3.3.7.4
    * The RNG has changed from an old version of xoroshiro128+ to
      xoroshiro128**.  This means sequences of random numbers will be
      different from before.  See ~~#276~~.
    * The layout of the distribution has changed.  Version numbers are
      added to files and directories.  For the exact layout, see !261.
    * Update mk:defsystem to v3.9.2.
  * ANSI compliance fixes:
  * Bug fixes:
  * Gitlab tickets:
    * #135 `(unix-namestring ".")` returns "" instead of "."
    * #154 piglatin translation does not work anymore
    * #171 Readably print `(make-pathname :name :unspecfic)`
    * #180 Move `get-page-size` to C
    * #196 Fix issues with mapping and nconc accumulation (mapcan)
    * #216 `enough-namestring` with relative pathname fails
    * #234 Make :ASCII external format builtin
    * #240 Speed up set operations
    * #242 Fix bug in `alien-funcall` with `c-call:char` as result type
    * #244 Add `c-call:signed-char`
    * #245 Replace `egrep` with `grep -E` in `make-dist.sh`
    * #248 Print MOVS instruction with correct case
    * #249 Replace LEA instruction with simpler shorter instructions in arithmetic vops for x86
    * #253 Block-compile list-to-hashtable and callers
    * #258 Remove `get-page-size` from linux-os.lisp
    * #252 Add script to run ansi-tests
    * #256 loop for var nil works
    * #259 `system::*software-version*` undefined when compiling
      on linux
    * #260 Command line options `-edit` and `-slave` no longer
      available for Hemlock
    * #261 Remove `get-system-info` from "bsd-os.lisp"
    * #268 Can't clone ansi-test repo on Mac OS CI box
    * #262 [arch_skip_inst invalid code -55]
    * #265 CI for mac os is broken
    * #266 Support "~user" in namestrings
    * #269 Add function to get user's home directory
    * #270 Simplify `os_file_author` interface
    * #271 Update ASDF to 3.3.7
    * #272 Move scavenge code for static vectors to its own function
    * #274 1d99999999 hangs
    * #275 FP underflow in reader allows restarting with 0
    * #276 Implement xoroshiro128** generator for x86
    * #277 `float-ratio-float` returns 0 for numbers close to
      least-positive-float
    * #278 Add some more debugging prints to gencgc
    * #283 Add VOP for `integer-length` for `(unsigned-byte 32)` arg.
    * #284 Microoptimize `signed-byte-32-int-len` VOP for x86.
    * #288 Re-enable `deftransform` for random integers.
    * #290 Pprint `with-float-traps-masked` better
    * #291 Pprint `handler-case` neatly.
    * #293 Allow restarts for FP overflow in reader.
    * #294 Implement assembly routine for xoroshiro update function
    * #296 Disassembly of movd instruction broken
    * #297 Pprint `new-assem:assemble` with less indentation.
    * #298 Add `with-float-rounding-mode` macro
    * #299 Enable xoroshiro assembly routine
    * #303 Variable `*assert-not-standard-readtable*` defined but
      not used.
    * #309 obj_run_linker does unnecessary allocations
    * #312 Compiler error building motif server on Fedora 40
    * #314 tanh incorrect for large args
    * #316 Support roundtrip character casing
    * #320 Motif variant not defaulted for `x86_linux_clang` config
    * #321 Rename Motif Config.x86 to Config.linux
    * #323 Make string casing functions compliant
    * #327 Fix up weird CLRLF and LF line terminators in the same file
    * #329 Fix compiler warnings in os.lisp
    * #330 Fix typos in unicode.lisp
    * #333 `load` doesn't accept generalized boolean for
      `:if-does-not-exist` arg
    * #338 Solaris/x86 build
    * #336 Clean up some compiler notes
    * #337 Cross-compile from x86 (linux) to x86 fails
    * #339 Solaris/x86 `nl_langinfo` returns "646"
    * #340 Use `+ascii-limit+` instead of `#x7f` in srctran.lisp
      for consistency
    * #341 Update version feature in cross-compile script
    * #342 Add CI job to run gcc static analyer
    * #348 Solaris/x86: u_int64_t vs uint64_t
    * #347 Solaris/x86: Update cross-compile script
    * #350 Export warnings on Solaris
    * #351 Solaris does not recognize `-E` option for grep
    * #352 Always use bzip2 compression for tarballs
    * #353 Automatically use gtar on Solaris when making a distribution
    * #354 Check that executables can be created in CI
    * #356 Return value from `vm::x87-floating-point-modes` should
      have status word in low part of result
    * #357 Solaris needs limits.h to get `PATH_MAX` in elf.c
    * #360 Adding site-init file
    * #361 Add herald item to mention where to report issues
    * #362 Simplify "library:" search-list
    * #363 Version numbers added to files and directories.  The
      distribution layout has changed.
    * #364 Add interface to `mkdtemp` and `mkstemp`
    * #367 Add `stream:string-count-octets` to count octets in a string
    * #369 Improve docstring for `unix::unix-setlocale`
    * #375 `unix-mkstemp` and `unix-mkdtemp` actually returns the
      file names now.
    * #379 Support GNU-style command-line option names
    * #382 Command-line options are case-sensitive
    * #385 Fixed compiler warning about `%p` in Linux-os.c
    * #386 Generate `def-unix-error` forms from OS-specific files.
      This replaces the original versions in unix.lisp.
    * #394 Add `os_getcwd` to get the current directory
    * #398 Update mk:defsystem to v3.9.2    
    * #400 `unix-resolve-links` no longer breaks on some lengths
    * #401 `file-position` now returns the correct value
    * #404 Use `realpath(3)` to implement `unix::unix-resolve-links`
    * #405 `unix::unix-resolve-links` has been removed.
    * #415 Support package-local-nicknames
    * #417 PCL complains about repeated aux variables in defmethod
    * #418 Update asdf to version 3.3.7.4 (for
      package-local-nicknames)
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
