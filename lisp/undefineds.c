/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.c,v 1.2.1.1 1994/10/24 19:52:41 ram Exp $ */

#ifdef sun
#ifndef MACH
#if !defined(SUNOS) && !defined(SOLARIS)
#define SUNOS
#endif
#endif
#endif

typedef int func();

extern func
#include "undefineds.h"
;

func *reference_random_symbols[] = {
#include "undefineds.h"
};
