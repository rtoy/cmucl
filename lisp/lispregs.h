/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/lispregs.h,v 1.3 1994/07/05 16:10:38 hallgren Exp $ */

#if defined(mips) || defined(irix)
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
