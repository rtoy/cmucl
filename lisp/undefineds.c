/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.c,v 1.4 1997/06/18 08:57:22 dtc Exp $ */

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

#ifdef SOLARIS
void reference_random_symbols_fun(void) {
#endif

func *reference_random_symbols[] = {
#include "undefineds.h"
};

#ifdef SOLARIS
}
#endif
