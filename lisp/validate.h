/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/validate.h,v 1.2 1994/03/27 15:22:16 hallgren Exp $ */

#if !defined(_INCLUDE_VALIDATE_H_)
#define _INCLUDE_VALIDATE_H_

#ifdef parisc
#include "hppa-validate.h"
#endif parisc

#ifdef mips
#include "mips-validate.h"
#endif

#ifdef ibmrt
#include "rt-validate.h"
#endif

#ifdef sparc
#include "sparc-validate.h"
#endif

#ifdef i386
#include "x86-validate.h"
#endif

#ifdef alpha
#include "alpha-validate.h"
#endif

extern void validate(void);

#endif
