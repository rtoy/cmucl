CMU Common Lisp is a public domain implementation of Common Lisp.  Both sources
and executables are freely available via anonymous FTP; this software is 
"as is", and has no warranty of any kind.  CMU and the authors assume no
responsibility for the consequences of any use of this software.

15e will probably be the last version 15 release, and is thus the most stable
system you're going to see for a while.  We're reluctant to call it a
"default" release because some things are stably broken:
 -- There still isn't any good stack overflow detection.  Probably stack
    overflow detection won't appear until the C code rewrite associated with
    generational GC comes out (version 17 or later.)
 -- The Alien/foreign function call mechanism is fairly broken.  It doesn't
    work at all in interpreted code, and DEF-C-ROUTINE doesn't work properly
    for many argument type signatures.  We've redesigned and reimplemented
    our foreign interface for version 16.

The CMU Common Lisp project's goal is to develop a high quality public domain
system, so we want your bug reports, bug fixes and enhancements.  However,
staff limitations prevent us from providing extensive support to people outside
of CMU.  We are looking for university and industrial affiliates to help us
with porting and maintenance for hardware and software that is not widely used
at CMU.

See "man cmucl" (man/man1/cmucl.1) for other general information.

Distribution:

CMU Common Lisp is only available via anonymous FTP.  We don't have the
manpower to make tapes.  All of our files are in the AFS file system.  Here are
some suggested gateway machines:
    lisp-rt1.slisp.cs.cmu.edu (128.2.217.9)
    lisp-rt2.slisp.cs.cmu.edu (128.2.217.10)

Log in with the user "anonymous" and "username@host" as password (i.e. your
EMAIL address.)  Due to the way anonymous FTP access control is done, it is
important to "cd" to the source directory with a single command, and then do a
"get" operation.  If you have any trouble with FTP access, please send mail to
slisp@cs.cmu.edu.

The binary release area is /afs/cs.cmu.edu/project/clisp/release.  This
directory holds compressed tar files with names of the form:
    <version>-<machine>_<os>.tar.Z

FTP compressed tar archives in binary mode.  To extract, "cd" to the
directory that is to be the root of the tree, then type:
    uncompress <file.tar.Z | tar xf - .

As of 2/26/92, the latest SunOS Sparc release is:
    15e-sun4c_41.tar.Z (9.3 meg)

The resulting tree is 23 megabytes.  For installation directions, see the
section "site initialization" in README file at the root of the tree.

If poor network connections make it difficult to transfer a 10 meg file, the
release is also available split into five parts, with the suffix ".0" to ".4".
To extract from multiple files, use:
    cat file.tar.Z.* | uncompress | tar xf - .

The release area also contains source distributions and other binary
distributions.  A listing of the current contents of the release area is in
release/FILES.  Major release announcements will be made to comp.lang.lisp
until there is enough volume to warrant a comp.lang.lisp.cmu.

SunOS credit:

The SunOS support was written by Miles Bader and Ted Dunning.

SPARC Notes:

We have not done any SPARC-specific tuning yet.  Performance will improve from
10-30% when we add instruction scheduling and register windows.  At least 16
meg of memory is recommended, and more is better.

Site initialization:

To run CMU CL, place bin/ in PATH and setenv CMUCLLIB to point to lib/.  The
file lib/site-init.lisp contains site-specific initialization, such as setting
of the site name.  Any site-specific initialization should be placed in this
file; this file can be compiled.  See bin/sample-wrapper for a shell script
template that sets up environment variables and then runs CMU CL.

TMPFS NOTE:

It is not possible to mmap a file in a tmpfs filesystem.  If /tmp is a "tmpfs"
filesystem, then you must setenv CMUCL_EMPTYFILE to a pathname (in a normal
filesystem) that can be used instead of /tmp/empty.  The "df" command will
show tmpfs filesystems as mounted on "swap".  If this problem exists on
your system, lisp will get an error like:
    mapin: mmap: Invalid argument
    ensure_space: Failed to validate 67108864 bytes at 0x01000000


Running CMU CL:

Run "lisp".  Try also "lisp -edit", which starts Hemlock.  Hemlock will use X
if the DISPLAY environment variable is defined, otherwise it will use terminal
i/o based on TERM and /etc/termcap.

Source availability:

Lisp and documentation sources are available via anonymous FTP ftp to any CMU
CS machine.  [See the "Distribution" section for FTP instructions and source
distribution information.]  All CMU written code is public domain, but CMU CL
also makes use of several imported packages: PCL, CLX and XP.  Although these
packages are copyrighted, they may be freely distributed without any licensing
agreement or fee.

/afs/cs/project/clisp/release/15e-source.tar.Z (3.6 meg) is an image of all
the ".lisp" source files used to build version 15e.

Totally machine-independent compiler code:
    /afs/cs/project/clisp/src/beta/compiler/*.lisp
See especially node.lisp and ir1tran.lisp for the front end.  vop.lisp,
vmdef.lisp and ir2tran.lisp for the back end.

Stuff that is dependent on our choice of object format, but not
particularly machine-dependent:
    /afs/cs/project/clisp/src/beta/compiler/generic/*.lisp

Compiler back-end for the PMAX and SPARC:
    /afs/cs/project/clisp/src/beta/compiler/mips/*.lisp
    /afs/cs/project/clisp/src/beta/compiler/sparc/*.lisp

Miscellaneous Lisp run-time code:
    /afs/cs/project/clisp/src/beta/code/*.lisp

C run-time code:
    /afs/cs/project/clisp/src/beta/ldb/*

A very drafty version of an internal design document: (160 pages) Some of
the "tex" files may be more humanly readable, since many formatting
commands need to be added.  There is some inaccurate (dated) material in
the compiler description.
    /afs/cs/project/clisp/hackers/ram/docs/internals/*.tex
    /afs/cs/project/clisp/hackers/ram/docs/internals/design.ps

See hackers/ram/scribe/internals/architecture.tex (the System Architecture
chapter in the internals document) for more information on the source tree
organization.
