/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/lispregs.h,v 1.2 1994/03/27 15:20:39 hallgren Exp $ */

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

#ifdef alpha
#include "alpha-lispregs.h"
#endif

#ifndef LANGUAGE_ASSEMBLY
extern char *lisp_register_names[];
#endif
