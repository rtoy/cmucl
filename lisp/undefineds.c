/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.c,v 1.2 1993/08/18 22:14:34 wlott Exp $ */

#ifdef sun
#ifndef MACH
#define SUNOS
#endif
#endif

typedef int func();

extern func
#include "undefineds.h"
;

func *reference_random_symbols[] = {
#include "undefineds.h"
};
