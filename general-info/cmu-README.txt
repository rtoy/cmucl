To get CMU Common Lisp, run:
    /usr/cs/etc/modmisc - cs.misc.cmucl

This establishes /usr/misc/.cmucl as a symbolic link to the release area.
In your .login, add CMU CL to your path:
    setpath -i /usr/misc/.cmucl

Then run "lisp".  Note that the first time you run Lisp, it will take AFS
several minutes to copy the image into its local cache.  Subsequent starts
will be much faster.

Or, you can run directly out of the AFS release area (which may be necessary on
SunOS machines).  Put this in your .login shell script:
    setenv CMUCLLIB "/afs/cs/misc/cmucl/@sys/beta/lib"
    setpath -i /afs/cs/misc/cmucl/@sys/beta

After setting your path, "man cmucl" will give an introduction to CMU CL and 
"man lisp" will describe command line options.  For SunOS installation notes,
see the README file in the SunOS release area. 

See /usr/misc/.cmucl/doc for release notes and documentation.  Rather old
hardcopy documentation is available as tech reports in the document room.

Send bug reports and questions to cmucl-bugs@cs.cmu.edu.  If you send a bug
report to gripe, they will just forward it to this mailing list.

Running CMU CL:

Run "lisp".  Try also "lisp -edit", which starts Hemlock.  Hemlock will use X
if the DISPLAY environment variable is defined, otherwise it will use terminal
i/o based on TERM and /etc/termcap.

Source availability:

Lisp and documentation sources are publicly readable in /afs/cs/project/clisp.
All CMU written code is public domain, but CMU CL also makes use of several
imported packages: PCL, CLX and XP.  Although these packages are copyrighted,
they may be freely distributed without any licensing agreement or fee.
