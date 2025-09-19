# MK:DEFSYSTEM

An updated version (3.9.1) of the first widely available (and quite
portable) DEFSYSTEM for Common Lisp.

Copyright (c) 1989 - 1999 Mark Kantrowitz, all rights reserved.  
1999 - 2005 Mark Kantrowitz and Marco Antoniotti, all rights reserved.  
2005 - 2008 Marco Antoniotti all rights reserved.  
2008 - 2024 Marco Antoniotti, and Madhu all rights reserved.  

See the preamble in the file `defsystem.lisp` for licensing information.


## Description

**MK:DEFSYSTEM** was, and is, the first widely available,
implementation independent, `DEFSYSTEM` (or `make`) for Common Lisp.

The current distribution and repository (3.9) contains a file
`defsystem.lisp`, a `docs` directory, and this `README` file. Plus it
contains a `.asd` for redistributability with
[Quicklisp](https://www.quicklisp.org). The current version is based on
**MK:DEFSYSTEM** distributed with
[CLOCC](http://clocc.sourceforge.net/), but it contains
enhancements and extensions for newer CL implementations.

**MK:DEFSYSTEM** works on most current (February 2022) Common Lisp
implementations and it is still used by a wide variety of projects.


## Installation

To install **MK:DEFSYSTEM**, you should just make sure to `load` the
file `defystem.lisp` in you CL environment.  You can do that by
putting the appropriate `load` statement in your CL initialization file
(which most implementations have).


## Documentation

The documentation for **MK:DEFSYSTEM** is part of a the following CMU
Technical Report:

Mark Kantrowitz, *Portable Utilities for Common Lisp, User Guide
and Implementation Notes*, Tech Report CMU-CS-91-143, School of
Computer Science, Carnegie Mellon University, May 1991.

The `docs` directory contains a quick introduction to **MK:DEFSYSTEM**.


## A Note on Forking

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.
It helps keeping the entropy level at an acceptable level.
