/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/lispregs.h,v 1.4 2002/08/23 17:01:01 pmai Exp $ */

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

/* This matches the  definition of sc-offset in code/debug-info.lisp */
#define SC_OFFSET(sc,offset) (((offset) << 5) | (sc))

#ifndef LANGUAGE_ASSEMBLY
extern char *lisp_register_names[];
#endif
