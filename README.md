[![pipeline status](https://gitlab.common-lisp.net/cmucl/cmucl/badges/master/pipeline.svg)](https://gitlab.common-lisp.net/cmucl/cmucl/commits/master)

CMUCL is a free, high performance implementation of the Common Lisp
programming language which runs on most major Unix platforms. It
mainly conforms to the ANSI Common Lisp standard. CMUCL provides a
sophisticated native code compiler; a powerful foreign function
interface; an implementation of CLOS; the Common Lisp Object System;
which includes multimethods; a metaobject protocol; a source-level
debugger and code profiler; and an Emacs-like editor implemented in
Common Lisp. CMUCL is maintained by a team of volunteers collaborating
over the Internet, and is mostly in the public domain.

Here is a summary of its main features:

* support for [**static arrays**](https://cmucl.org/docs/cmu-user/html/Static-Arrays.html) that are never moved by GC but are
  properly removed when no longer referenced.
* [**Unicode**](https://cmucl.org/docs/cmu-user/html/Internationalization.html) support, including many of the most common external
  formats such as UTF-8 and support for handling Unix, DOS, and
  Mac end-of-line schemes.
* native [**double-double floats**](https://cmucl.org/docs/cmu-user/html/Extended-Floats.html) including complex double-double
  floats and specialized arrays for double-double floats and and
  complex double-double floats that give approximately 106 bits
  (32 digits) of precision.
* a **sophisticated native-code compiler** which is capable of
  powerful type inferences, and generates code competitive in
  speed with C compilers.
* **generational garbage collection** on all supported
  architectures.  
* **multiprocessing capability** on the x86 ports.
* a [foreign function interface](https://cmucl.org/docs/cmu-user/html/Alien-Objects.html) which allows interfacing with C code
  and system libraries, including shared libraries on most platforms,
  and direct access to Unix system calls.
* support for [interprocess communication](https://cmucl.org/docs/cmu-user/html/Interprocess-Communication-under-LISP.html) and remote procedure calls.
* an implementation of CLOS, the [Common Lisp Object
  System](http://en.wikipedia.org/wiki/Common_Lisp_Object_System),
  which includes multimethods and a metaobject protocol.
* a graphical source-level debugger using a [Motif interface](https://cmucl.org/docs/interface/toolkit/html/index.html), and a
  [code profiler](https://cmucl.org/docs/cmu-user/html/Profiling.html).
* an interface to the X11 Window System ([CLX](https://sharplispers.github.io/clx/)), and a sophisticated
  graphical widget library ([Garnet](https://www.cs.cmu.edu/~garnet/),
  available separately).
* programmer-extensible input and output streams ([Gray
  Streams](http://www.nhplace.com/kent/CL/Issues/stream-definition-by-user.html)
  and
  [simple-streams](http://www.franz.com/support/documentation/current/doc/streams.htm)).
* an Emacs-like editor,
  [Hemlock](http://cmucl.org/hemlock/index.html), implemented in
  Common Lisp.
* **freely redistributable**: free, with full source code (most of
  which is in the public domain) and no strings attached (and no
  warranty). Like the GNU/Linux and *BSD operating systems, CMUCL is
  maintained and improved by a team of volunteers collaborating over
  the Internet.

For the latest news and other information, see [the wiki](https://gitlab.common-lisp.net/cmucl/cmucl/wikis/home).
