/*
 * Header file for GC
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/gc.h,v 1.4 1990/11/12 02:36:28 wlott Exp $
 */

#if !defined(_INCLUDED_GC_H_)
#define _INCLUDED_GC_H_

#include "lisp.h"

extern void gc_lose();

#define gc_abort() do { \
	fprintf(stderr, "GC invariant lost!  File \"%s\", line %d\n", \
	__FILE__, __LINE__); \
	gc_lose(); \
} while (0)

#if 0
#define gc_assert(ex) do { \
	if (!(ex)) gc_abort(); \
} while (0)
#else
#define gc_assert(ex)
#endif


#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))
#define FIXNUM_TO_INT(x) ((x)>>2)
#define INT_TO_FIXNUM(x) ((x)<<2)

#endif
