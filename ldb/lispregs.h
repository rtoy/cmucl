/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/lispregs.h,v 1.8 1991/05/24 17:55:24 wlott Exp $ */

#ifdef mips
#include "mips-lispregs.h"
#endif

#ifdef sparc
#include "sparc-lispregs.h"
#endif

#ifdef ibmrt
#include "rt-lispregs.h"
#endif

#ifndef LANGUAGE_ASSEMBLY
extern char *lisp_register_names[];
#endif
