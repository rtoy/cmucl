/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.c,v 1.3.2.2 1998/06/23 11:25:08 pw Exp $ */

#ifdef sun
#ifndef MACH
#if !defined(SUNOS) && !defined(SOLARIS)
#define SUNOS
#endif
#endif
#endif

typedef int func();

extern func
#define F(x) x,
#if !(defined(irix) || defined(SOLARIS))
/* XXXfixme next line probably wrong; was previous behavior */
#define D(x) x,
#else
#define D(x)
#endif
#include "undefineds.h"
#undef F
#undef D
exit; /* just a random function known to exist */

#if defined(SOLARIS) || defined(irix)

#ifdef irix
int errno; /* hack to be sure works with newer libc without having to redump */
           /* causes libc to be relocated to match cmucl rather than vice
              versa */
#endif

extern int
#define F(x)
#define D(x) x,
#include "undefineds.h"
#undef F
#undef D
errno;                          /* a random variable known to exist */

int reference_random_symbols(void) {
   int a;
#define F(x) x();
#define D(x) a+=x;
#include "undefineds.h"
#undef F
#undef D
   return a;
   }

#else

func *reference_random_symbols[] = {
#define F(x) x,
   /* XXXfixme next line is probably wrong but was previous behavior */
#define D(x) x,
#include "undefineds.h"
#undef F
#undef D
   exit                         /* a random function known to exist */
};

#endif
