/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/validate.h,v 1.4 2004/05/18 22:22:59 cwang Exp $ */

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

#if defined(i386) || defined(__x86_64)
#include "x86-validate.h"
#endif

#ifdef alpha
#include "alpha-validate.h"
#endif

extern void validate(void);

#endif
