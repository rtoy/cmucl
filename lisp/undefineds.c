/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.c,v 1.1 1992/09/08 20:19:03 wlott Exp $ */

#ifdef sun
#ifndef MACH
#define SUNOS
#endif
#endif

typedef int func();

extern func
#include "undefineds.h"
;

static func *foo[] = {
#include "undefineds.h"
};
