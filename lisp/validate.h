/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/validate.h,v 1.3 2002/05/02 21:10:53 toy Exp $ */

#if !defined(_INCLUDE_VALIDATE_H_)
#define _INCLUDE_VALIDATE_H_

#ifdef parisc
#include "hppa-validate.h"
#endif

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
