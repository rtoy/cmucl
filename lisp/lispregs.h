/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/lispregs.h,v 1.1 1992/07/28 20:14:41 wlott Exp $ */

#ifdef mips
#include "mips-lispregs.h"
#endif

#ifdef sparc
#include "sparc-lispregs.h"
#endif

#ifdef ibmrt
#include "rt-lispregs.h"
#endif

#ifdef i386
#include "x86-lispregs.h"
#endif

#ifdef parisc
#include "hppa-lispregs.h"
#endif

#ifndef LANGUAGE_ASSEMBLY
extern char *lisp_register_names[];
#endif
