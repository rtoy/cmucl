CMU Common Lisp is a public domain implementation of Common Lisp.  Both sources
and executables are freely available via anonymous FTP; this software is 
"as is", and has no warranty of any kind.  CMU and the authors assume no
responsibility for the consequences of any use of this software.

This is a general beta release, meaning that anyone can FTP it, but we won't be
very sympathetic about catastrophes resulting from your dependence on CMU CL.
After the bug reports die down, we will announce a full release, and will then
try to be sympathetic toward desperate users.

The CMU Common Lisp project's goal is to develop a high quality public domain
system, so we want your bug reports, bug fixes and enhancements.  However,
staff limitations prevent us from providing extensive support to people outside
of CMU.  We are looking for university and industrial affiliates to help us
with porting and maintenance for hardware and software that is not widely used
at CMU.

See "man cmucl" (man/cmucl.1) for other general information.

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

The latest SunOS Sparc release is:
    -rw-r--r--  1 wlott     9330285 Nov  1 17:35 15b-sun4c_41.tar.Z

The resulting tree is 23 megabytes.

The release area also contains source distributions and other binary
distributions.  A listing of the current contents of the release are is in
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
file; this file can be compiled.

TMPFS NOTE:

It is not possible to mmap a file in a tmpfs filesystem.  If /tmp is a "tmpfs"
filesystem, then you must setenv CMUCL_EMPTYFILE to a pathname that can be
used instead of /tmp/empty.  If this problem exists on your system, you will
get map failures on Lisp startup.

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

Also in /afs/cs/project/clisp/release:

-rw-r--r--  1 ram       3257791 Oct 19 19:50 15b-sun4-source.tar.Z
	Image of all ".lisp" source files used to build version 15b for SPARC
	machines.  Probably more interesting to most people than the RCS
        distribution. 

-rw-r--r--  1 ram       7267593 Oct 16 16:42 10-16-91-cmucl-master.tar.Z
	The project/clisp/master subtree: the RCS source (,v) files for all of
	CMU CL.


Totally machine-independent compiler code:
    /afs/cs/project/clisp/pmax_mach/alpha/compiler/*.lisp
See especially node.lisp and ir1tran.lisp for the front end.  vop.lisp,
vmdef.lisp and ir2tran.lisp for the back end.

Stuff that is dependent on our choice of object format, but not
particularly machine-dependent:
    /afs/cs/project/clisp/pmax_mach/alpha/compiler/generic/*.lisp

Compiler back-end for the PMAX and SPARC:
    /afs/cs/project/clisp/pmax_mach/alpha/compiler/mips/*.lisp
    /afs/cs/project/clisp/sun4_mach/alpha/compiler/sparc/*.lisp

Miscellaneous Lisp run-time code:
    /afs/cs/project/clisp/pmax_mach/alpha/code/*.lisp

C run-time code:
    /afs/cs/project/clisp/pmax_mach/alpha/ldb/*

A very drafty version of an internal design document: (160 pages) Some of
the "tex" files may be more humanly readable, since many formatting
commands need to be added.  There is some inaccurate (dated) material in
the compiler description.
    /afs/cs/project/clisp/hackers/ram/scribe/internals/*.tex
    /afs/cs/project/clisp/hackers/ram/scribe/internals/design.ps

See hackers/ram/scribe/internals/architecture.tex (the System Architecture
chapter in the internals document) for more information on the source tree
organization.
